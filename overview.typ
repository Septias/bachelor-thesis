#import "functions.typ": *
#set heading(numbering: "1.")
#set page(margin: 2em)

= Current Efforts of Typing the Nix Language and Coding an LSP
In this document I try to lay out the current efforts of creating a type system and its implementation as a language server written in Rust. This document should act as an overview such that we (Peter Thiemann, Taro Sekiyama and I) have a common ground to discuss next steps and current efforts. Most of it is WIP and there are many loose ends and even contradictions.


= Type System <ts>

I use _spread syntax_ like `{…rc}` which means a new record is created from the fields of `rc` where `rc` is a record. These new fields never overwrite existing fields, meaning `{a : int, …{a : string}}` will reduce to `{a: int}`. Similar is possible for arrays, but naturally without deduplication. Example: [a, …[b c a]] => [a, b, c, a]

I also oftentimes abbreviate $l_0 … l_n$ as $l_i$.

#v(.5cm)
#colored_box(title: "Syntax: Basetypes", color: blue)[
  Definition of some base types using regular expressions.
  $
                          c & ::= "[^\"$\\] | $(?!{) | \\."    \
                    "inter" & ::= "${"\^} *"}"                 \
     #type_name("String") s & ::= "\"(c"*" inter)"*" c"*"\""   \
    #type_name("Boolean") b & ::= "true" | "false"             \
       #type_name("Path") p & ::= "(./|~/|/)([a-z A-Z .]+/?)+" \
     #type_name("Number") n & ::= "([0-9]*\.)?[0-9]+"          \
      #type_name("Label") l & ::= "[A-Za-z_][A-Za-z0-9_'-]*"   \
  $
]
The language consists of the standard base types string, boolean, number and label. Labels are distinct here, because we need a syntactic class in some places where only labels are allowed. An example is a path that is constructed from labels interspersed by dots, i.e. `hm.packages.git`.

#colored_box(title: "Syntax", color: blue)[
  $
    t ::=
    &| b | s | p | n | l | "null" \
    #type_name("Record") &| {#text(fill: green)[a] _i} | #text(weight: "bold")[rec] {#text(fill: green)[a] _i}\
    #type_name("Array") &| [ space t_0 space t_1 space ... space t_n space] \
    #type_name("Has-Attribute") &| t #text(weight: "bold", " ? ") l \
    #type_name("Has-Attribute-Or") &| t.l #text(weight: "bold")[or] t \
    #type_name("Record-Concat") &| t "//" t \
    #type_name("Array-Concat") &| t "⧺" t \
    #type_name("Lookup") &| t "." l \
    #type_name("Dynamic-Lookup") &| t "." t \
    #type_name("Function") &| #text(fill: red, "pat"): t \
    &| #text(weight: "bold")[let] #text(fill: green)[a] _i #text(weight: "bold")[in] t \
    &| #text(weight: "bold")[if] t #text(weight: "bold")[then] t #text(weight: "bold")[else] t \
    &| #text(weight: "bold")[with] t; t \
    &| #text(weight: "bold")[assert] t; t
  $
]

- Functions take one argument which can be a pattern. This pattern has a record-like structure and allows for multiple fields to be present, this way a function taking multiple arguments can be created without resorting to currying.
- Array elements are delimited by spaces, which is uncommon and records can be marked _recursive_ with the `rec` keyword. Both of these datatypes are _immutable_ but there are the concat operations (*Record-Concat* and *Array-Concat*) that can be used to create new, bigger datatypes.
- Record lookups can be static (with a given label) or dynamic, with an arbitrary expression t, that has to reduce to a string. This is further discussed in @dynamic_lookup.
- Let statements can have multiple bindings $a_1 = t_1; … ; a_n = t_n$ before the `in` keyword appears.
- The with statement expects an arbitrary expression that reduces to a record. Every field from the record is then added to the scope of the next expression without shadowing existing variables. This is further discussed in @with.

#colored_box(title: "Syntax: Record and Let fields", color: blue)[
  $
    #text(fill: green)[a] & ::= l = t; " | " "i" \
    "i" & ::= #text(weight: "bold")[inherit] l_0 " … " l_n; " | " #text(weight: "bold")[inherit] (p) " " l_0 " … " l_n; \
    p & ::= l | p.l \
  $
]
Both let statements and records allow for _inherit statements_ to be placed between ordinary field declarations. Inherit statements take a known label for a value and _reintroduce_ the label as "label = value;" to the record or let statement. This feature is only syntactic sugar to make it easier to build records. Let statements can take a root path `(p)` which is prefixed to all following lookups. This way a deep record can be referenced from which all values are taken. For example, the statement `inherit (world.objects.players) robert anders;` will add `robert = world.objects.players.robert; anders = world.objects.players.anders;` to the surrounding record or let expression.

#colored_box(title: "Syntax: Pattern", color: blue)[
  $
    #text(fill: red, "pat") & ::= { space e_i space }                        \
                            & | { space e_i, #text(weight: "bold")[…] space} \
                            & | l                                            \
                        "e" & ::= l | l space ? space t                      \
  $
]
Patterns can be open (…) or closed and can also be given default arguments with the `?` syntax. An example would be `{a, b ? "pratt", …}` which is an open pattern with a default value of "pratt" for the label b.


#colored_box(title: "Reduction Rules", color: blue)[
  Let $a,b$ and $t$ range over syntax terms and $l$ over label.
  $
    (l: b)a & arrow.long b[l := a] &&#rule_name("R-Fun") \
    ({l}: b){l: a} & arrow.long b[l := a] &&#rule_name("R-Fun-Pat") \
    ({l}: b){l: a, m: b} & arrow.long ¤ &&#rule_name("R-Fun-Err") \
    ({l, ...}: b){l: a, m: b} & arrow.long b[l := a] &&#rule_name("R-Fun-Pat-Open") \
    ({l" ? "t}: b)({..}\\l) & arrow.long b[l := t] &&#rule_name("R-Fun-Pat-Default") \
    {l: t}.l & arrow.long t &&#rule_name("R-Lookup") \
    ({..}\\b).b & arrow.long "null" &&#rule_name("R-Lookup-Null") \
    ({..}\\b).b" or "t & arrow.long b &&#rule_name("R-Lookup-Default") \
    ({..}\\b)" ? "b & arrow.long "false" &&#rule_name("R-Has-Pos") \
    {b: t,..}" ? "b & arrow.long "true" &&#rule_name("R-Has-Neg") \
    ("rec" { l = {l = l};};).l & arrow.long {l = { l = l;}};"   " &&#rule_name("R-Rec") \
    "let" l_i = a_i; "in" b & arrow.long b[l_i = a_i] &&#rule_name("R-Let") \
    "with" {l_i = a_i}; b & arrow.long b[l_i "/=" a_i ] &&#rule_name("R-With") \
    "if true then "a" else "b & arrow.long a &&#rule_name("R-Cond-True") \
    "if false then "a" else "b & arrow.long b &&#rule_name("R-Cond-False") \
    a ⧺ b & arrow.long [ …a, …b] &&#rule_name("R-Array-Concat") \
    a " //" b & arrow.long {…b, …a} &&#rule_name("R-Record-Concat") \
  $
]

- R-Fun is the standard function β-reduction where the argument is replaced by the supplied argument's value in the body. `b[l := a]` means that the variable l is assigned value a in the body.
- What follows are function application variations for the different patterns that are possible. If a function expects a record with field l and is supplied such a record, it reduces like a normal function (R-Fun-Pat). If there are more arguments than needed, an error is raised (R-Fun-Err) but only if the pattern is not _open_ (R-Fun-Pat-Open). Lastly, it is possible to give default arguments for arguments that do not supply certain fields. I use the syntax `{..} \ l` to create an arbitrary record without the label l.
- The pattern rules (R-Fun-Pat for example) only reduce for one field which is a problem. This is fixed by applying the rules for single cases matching their structure exhaustively until every possible pattern item is handled or (R-Fun-Err) stops reduction. TODO: This is still a bit hand-wavy and needs better formalization, but I hope the idea can be seen.
- Lookup is handled by three rules, (R-Lookup, R-Lookup-Null, R-Lookup-Default) which are straightforward. The two rules for the "has"-operator are straightforward as well.
- Recursive records can be looked up but don't change their inner structure by this operation. The only difference is that the rec keyword is removed. TODO: I don't know how to feel about this and whether this "marker" should be kept or whether it is only used initially to check wellformedness of parsed expressions.

- To reduce with statements the first term has to reduce to a record and I don't like the formalization of that currently. For the next expression the record fields are added to the scope without shadowing existing bindings. I use the `/=` operator to get this behavior. See @with for further discussion.

- The concat operations are quite natural given the _spread syntax_ described in @ts.


#colored_box(title: "Values", color: blue)[$
    p: b"  |  "x; "  |  "{..}"  |  rec" {..}
  $]


#colored_box(title: "Types", color: green)[
  $
    tau &::= tau -> tau | alpha | top | bot \
    #type_name("Type Connectives") &| tau union.sq tau | tau inter.sq tau \
    #type_name("Recursion") &| mu alpha space tau \
    #type_name("Base Types") &| "bool" | "string" | "path" | "num" \
    #type_name("Records") &| {l_0 : tau" ... "l_n: tau} | ⟨l_0 : tau " ... " l_n: tau⟩ \
    #type_name("Lists") &| [" "tau" "] | [" "τ_1" "…" "τ_n" "] \
    #type_name("Patterns") &| ({l_0: tau; ...; l_n: tau }, "bool") \
    #type_name("Kind") k &:= star, P, L
  $
]

- TODO: Do we need an option type because we have functions with default arguments?
- TODO: Kinds (label, pattern, )
- TODO: Is recursion handled correctly?

#colored_box(title: "Typing Rules", color: purple)[
  #typings(
    [],
    (
      (
        derive(
          "T-Var",
          ($x: ∀ arrow(α). space τ in Γ$,),
          $Γ tack x: τ[arrow(α) \\ arrow(τ)]$,
        ),
        derive("T-Abs", ($Γ, x: τ_1 tack t: τ_2$,), $Γ tack x: t: τ_1 → τ_2$),
        derive(
          "T-App",
          ($Γ tack t_1: τ_1 → τ_2$, $Γ tack t_2: τ_1$),
          $t_1 t_2: τ_2$,
        ),
      ),
      (
        derive(
          "T-Rcd",
          ($Γ tack t_0: τ_0$, "...", $Γ tack t_n: τ_n$),
          $Γ tack {arrow(l): arrow(t)}: {arrow(l): arrow(τ)}$,
        ),
        derive("T-Proj", ($ Γ tack t: {l: τ} $,), $Γ tack t.l: τ$),
        derive("T-Sub", ($Γ tack t: τ_1$, $τ_1 <= τ_2$), $Γ tack t: τ_2$),
      ),
      (
        derive("T-Negate", ($Γ tack e: "bool"$,), $Γ tack !e: "bool"$),
        derive("T-Check", ($Γ tack e: {l: τ}$,), $Γ tack e ? l: "bool"$),
        derive(
          "T-Or",
          ($Γ tack t_1: {l: τ_1}$, $Γ tack t_2: τ_2$),
          $Γ tack t_1.l "or" t_2: τ_1 union.sq τ_2$,
        ),
      ),
      (
        derive(
          "T-Lst-Hom",
          ($Γ tack t_0: τ$, "...", $Γ tack t_n: τ$),
          $Γ tack [ " " t_0 " " t_1 " " ... " " t_n " "]: [ τ]$,
        ),
        derive(
          "T-Lst-Agg",
          ($Γ tack t_0: τ_0$, "...", $Γ tack t_n: τ_n$),
          $Γ tack [space t_0 space t_1 space ... " " t_n] : [ τ_0 space τ_1 space ... space τ_n]$,
        ),
      ),
      (
        derive(
          "T-List-Concat-Hom",
          ($Γ tack a: "[τ]"$, $Γ tack b: "[τ]"$),
          $Γ tack a "⧺" b: "[τ]"$,
        ),
        derive(
          "T-List-Concat-Multi",
          ($Γ tack a: [arrow(τ_1)]$, $Γ tack b: [arrow(τ_2)]$),
          $Γ tack a "⧺" b: [arrow(τ_1)arrow(τ_2)]$,
        ),
      ),
      (
        derive(
          "T-Rec-Concat",
          ($Γ tack a: { l_i: τ_i }$, $Γ tack b: { l_j: τ_j }$),
          $Γ tack a "//" b: a backslash b union b$,
        ),
      ),
      (
        derive(
          "T-Multi-Let",
          (
            $Γ overline([x_i: τ_i tack t_i : τ_i]^i)$,
            $Γ overline([x_i:∀ arrow(α). τ_i]^i) tack t: τ$,
          ),
          $Γ tack "let" x_0 = t_0; ... ; x_n = t_n "in" t: τ$,
        ),
      ),
      (
        derive(
          "T-If",
          ($Γ tack t_1: "bool"$, $Γ tack t_2: τ$, $Γ tack t_3: τ$),
          $ "if" t_1 "then" t_2 "else" t_3: τ $,
        ),
      ),
      (
        derive(
          "T-With",
          (
            $Γ tack t_1 : {arrow(l): arrow(τ)}$,
            $Γ, l_0 : τ_0, ..., l_n: τ_n tack t_2: τ$,
            $l_i in.not Γ$,
          ),
          $Γ tack "with" t_1; t_2 : τ$,
        ),
      ),
      (
        derive(
          "T-Assert",
          ($Γ tack t_1: "As<bool>"$, $Γ tack t_2: τ_2$),
          $Γ tack "assert" t_1; t_2: τ₂$,
        ),
      ),
    ),
  )
]

- We have a standard typing context Γ, pre-filled with the standard library functions from @prelude and functions to handle the basic logic, arithmetic and comparison operators.
- TODO: The rule "T-Or" should distinguish between the positive and negative case similar to if, and only return one type instead of a union.
- TODO: "T-Rec-Concat" doesn't work really because of the generic subtyping rule. Further discussed in @records
- TODO: T-multi-let can be made simpler because we can always rewrite multi-let to let-chains. Recursion has to be accounted for, that is still an open question.
- TODO: T-With $l_i in.not Γ$ is too restrictive because shadowing labels are allowed, they will just not be used.

#colored_box(title: "Subtyping Rules", color: purple)[
  #typings([], (
    (
      derive("S-Refl", (), $τ <= τ$),
      derive(
        "S-Trans",
        ($Σ tack τ_0 <= τ_1$, $Σ tack τ_1 <= τ_2$),
        $Σ tack τ_0 <= τ_2$,
      ),
      derive("S-Weaken", ($H$,), $Σ tack H$),
      derive("S-Assume", ($Σ,gt.tri H tack H$,), $Σ tack H$),
    ),
    (
      derive("S-Hyp", ($H in Σ$,), $Σ tack H$),
      derive("S-Rec", (), $μ α.τ eq.triple [μ α.τ slash α]τ$),
      derive(
        "S-Or",
        ($∀ i, exists j,Σ tack τ_i <= τ'_j$,),
        $Σ tack union.sq_i τ_i <= union.sq_j τ'_j$,
      ),
      derive(
        "S-And",
        ($∀ i, exists j,Σ tack τ_j <= τ'_i$,),
        $Σ tack inter.sq_j τ_j <= inter.sq_i τ'_i$,
      ),
    ),
    (
      derive(
        "S-Fun",
        ($lt.tri Σ tack τ_0 <= τ_1$, $lt.tri Σ tack τ_2 <= τ_3$),
        $Σ tack τ_1 arrow.long τ_2 <= τ_0 arrow.long τ_3$,
      ),
      derive(
        "S-Rcd",
        (),
        ${arrow(t) : arrow(τ)} eq.triple inter.sq_i {l_i : t_i}$,
      ),
      derive(
        "S-Depth",
        ($lt.tri Σ tack τ_1 <= τ_2$,),
        $Σ tack {l: τ_1} <= { l: τ_2}$,
      ),
    ),
    (
      derive("S-Lst", ($ Γ tack τ_1 <= τ_2 $,), $Γ tack [τ_1] <= [τ_2]$),
    ),
  ))
  // TODO: no two child elements?
  // #pad_stack((
  //   $lt.tri(H_0, H_1) = lt.tri H_0, lt.tri H_1$,
  //   $lt.tri(gt.tri H) = H$,
  //   $lt.tri ( τ_0 <= τ_1) = τ_0 <= τ_1$,
  // ))
]

What follows are the constraining rules used in the constrain subroutine of the implementation. This uses the subtyping rules and applies them to types.
#colored_box(title: "Constraining rules", color: purple)[
  Constraining takes two types τ₁ and τ₂ and constraints the first type to be subtype of the other.
  #v(1cm)
  $
    (τ_1 → τ_2), (τ_3 → τ_4) &arrow.squiggly "constrain"(τ_3, τ_1); "constrain"(τ_2, τ_4) &&#rule_name("C-Fun")\
    {τ_1}, {τ_2} &arrow.squiggly ∀i ∈ τ_2. "constrain"(τ_(1i), τ_(2i)) "  if A" &&#rule_name("C-Rec")\
    {τ_1},({τ_2}, #text("true", weight: "bold")) &arrow.squiggly ∀i ∈ τ_2. "constrain"(τ_(1i), τ_(2i))"   if A" &&#rule_name("C-Pat-Open") \
    {τ_1} , ({τ_2}, #text("false", weight: "bold")) &arrow.squiggly ∀i ∈ τ_2. "constrain"(τ_(1i), τ_(2i)) "  if A ∧ B  "&&#rule_name("C-Pat-Closed")\
    [τ_1] , [τ_2] &arrow.squiggly "constrain"(τ_1, τ_2) &&#rule_name("C-Array") \
    ("lo", "up")^n, τ^m "  if" m <= n &arrow.squiggly "lo" ⩲ τ; ∀l ∈ "lo". "constrain"(l, τ) &&#rule_name("C-Var-⋆")\
    ("lo", "up")^n@τ_1, τ_2"       " &arrow.squiggly "constrain("τ_1", extrude("τ_2", false, n))" &&#rule_name("C-Var-⋆")\
    τ^n , ("lo", "up")^m "if" n <= m &arrow.squiggly "lo" ⩲ τ; ∀u ∈ "ul". "constrain"(τ, u) &&#rule_name("C-⋆-Var")\
    τ_1, ("lo", "up")^m@τ_2 &arrow.squiggly "constrain(extrude("τ_1", true, m), "τ_2")" &&#rule_name("C-⋆-Var")\
  $
  #v(1cm)
  *Conditions*:
  - A: Fields in $τ_2$ must be present in $τ_1$
  - B: $τ_1$ must only have the fields in $τ_2$

  *Remarks*
  - $("lo", "up")^n$ is used to match a _type variable_ and their lower and upper bounds. The superscript gives the _level_ of the variable that is used to handle generalization of variables.
  - $τ @ τ_1$ the \@ symbol is used to bind the whole preceding type to a new name (here $τ_1$)
  - $"lo" ⩲ τ$ is a shorthand for $"lo" = "lo" + τ$ and used to extend the list of upper or lower bounds.
]



= TODO
- Define Wellformedness?
- Define Evaluation contexts?
- Add polarized variables?
- Explain constrain fuction and move below subtyping

= Equality
Attribute sets and lists are compared recursively, and therefore are fully evaluated.

= Datatypes
== Records <records>
Records are defined very simply in this type system. The only supported record type is a list of `label: type` mappings which can be added during subtyping. There is no way to reorder them, or remove some. During typing, multiple object constraints are concatenated, so there is a way to add new fields.

Two problems occur with the current implementation. Firstly, we have the `//` operator which implements _open record extension_. Given two records `A: { X: string, Y: int }` and `B: { X: int }` the open record concatenation between the two records `(C = A \\ B)` is `C: {X: int, Y: int}`. This together with the generic subtyping rule T-Sub leaves the type system unsound, because fields can be removed, leaving the record B empty (`T-SUB: B -> {}`). In this case, the type system would predict `A.X` to be of type `string` which is simply wrong after the application.



== Context Strings
Context strings and dynamic lookup share the same syntax in that you can insert some arbitrary term `t` into braces like this `${t}`. For ordinary strings and paths, the value of `t` will be coerced into a string and added literally. From a typing perspective this is the easy case because inserted values get a constraint of string and that's it. For dynamic lookup it gets trickier though.

== Dynamic Lookup <dynamic_lookup>
Context strings allow lookups of the form `a.${t}` where t is allowed to be any expression that ultimately reduces to a string. The reduced string is then used to index the record which a is supposed to be. Since a type system only computes a type and not the actual value, the only possible approach to handle first-class labels is to evaluate nix expressions to some extent. Writing a full evaluator is probably too much, but there could be heuristics for simple evaluation. One approach would be to work backwards from return statements in functions up until it gets too unwieldy.
This would also mean implementing the standard library functions like map, readToString etc. One ray of hope is that these were probably already implemented in Tvix.


= Constructs
== With Statements <with>
With statements in nix are very tricky. They basically allow introducing all bindings of a record into the following expression. For this, the first expression (A) in $"with " A"; "B$ has to reduce to a record. If this does not work, typing should raise an error. For explicit records, the following typing is straightforward. Just introduce all fields to the scope without shadowing and continue typechecking $B$. For the case that A is a type variable, it gets tricky however because of the generic subsumption rule. When A is subtyped like follows $A: {X: "int"} arrow A: {}$, then the field X would not be accessible in the function body.
The second problem is what I call the _attribution problem_. This happens when there is a chain of with statements $"with "A; ("with "B;) t$ and A and B are type variables. Now when trying to lookup $x$ in t, it is unclear whether x came from B or A.


== Inherit Statements
In my Bachelor's Thesis, I handled inherit statements as syntactic rewrites which is still the preferred way to implement the feature. After I have written down the constraining rules, we can see if that is still the best way.


== Function Patterns
Functions luckily are pure and functional which helps in inferring a proper type immensely. Patterns are given as records, showing which exact fields are wanted for this function. The ellipsis `(…)` then allows for arbitrary extra fields, and the `?` question mark syntax for default values.
To handle these, all expected record fields need to be present in the function argument so a record constraint with these fields can be added to the argument of the function. If a default value is given for some record fields, a constraint can be made on the arguments as well.

== Dunder methods
There seem to be some special dunder methods for representations which are handled specially by the evaluator. I have not had the chance to look into it further.

= Laziness and Recursiveness
Laziness and recursion occur in two language constructs. The first one being _recursive records_ and the second one being _let bindings_. To evaluate them, a lazy evaluation scheme is needed which is currently implemented as follows:
When typing a let binding or record, the algorithm adds all name bindings to the context up-front. This way referenced values will not be undefined when looked up, even if their definition was not type checked yet. The typecheck algorithm then starts with some arbitrary first label $A$ which may contain an unchecked expression labeled $B$.
When this undefined label $B$ is found, it is simply used to create upper and lower bounds (constraints). For empty type variables that is fine to do, but when we actually check this $B$, it will unfold and be constrained with upper and lower bounds. These bounds are missing on the typecheck run of $A$ then. An example would be `let f = a: a + 1; x = f b; b = "hi" in {}` In this case b would be constrained to be a number (because of the application and its implication) but afterwards it will get its "real" type which is string. Currently, the constraint error would be placed at the wrong location (that of the true definition).

#figure(
  ```
  rec { x = { x = x;};}.x = { x = «repeated»}
  let x = {x = y;}; y = x; in x { x = «repeated»; }
  ```,
  caption: [Examples of recursive patterns from the nix repl],
)

= A Note about Implementation
One unique problem of nix is that every functionality (all 100,000 packages, the operating system, and the standard library) are rooted in a _single file_ at github.com/nixpkgs/flake.nix or github.com/nixpkgs/default.nix, depending on whether you use a flake based system or not. To not get lost in the weeds, the nix evaluator heavily relies on the laziness features of the language to not evaluate all of the packages exhaustively. For the ultimate goal of auto-completing nixos options one would have to parse and type this very file with the goal to resolve the module system. This includes the standard library and bootstrapping code for the module system. To even reach it, the type inference algorithm has to support the same kind of laziness the nix evaluator uses to not get lost.


== Practical Type Inference in Face of Huge Syntax
Code inference in the general case is similar to depth-first search, digging down one syntax tree and only returning as soon as all branches have been exhausted. Since nix trees are huge, this approach is not feasible and one has to lean towards a breadth-first search style, which focuses on the currently inferred file and stops when "too far away". To achieve this behavior, the inference algorithm at some point has to decide to stop inference and jump to another unfinished function, remembering at which place it left off.
In the nix language, there are two natural places to do so. Laziness of records and let statements gives the natural approach that every newly named binding is a stop-point at which inference only proceeds as far as needed. One heuristic could be to go two more functions down and then return to the let or record to generate at least some approximation of the final type.
The import statement semantics of nix come in very handy at this point. Import statements act just as function calls with the only difference being that the goto location is defined by path and not by name. Other than that, they can take arguments just as a function, and then try to apply given arguments to the file's expression. This language design comes in very handy because that way, import statements do not occur at the top of the file where it would need to be decided how to continue typechecking them. They occur right at the location where they are needed, sometimes in let statements or record fields. This way, the laziness of records and let statements could already be enough to get laziness into the language.
As for the practical approach, I propose a new marker type which can be set to bindings of a context. This marker type should contain all the information to go back to type inference at a previous location. This probably means cloning the context or restoring it to the previous state – cloning is probably easier. Another approach could be to keep the names undefined and add another mapping between names and reconstruction information somewhere that acts as a fallback.


Some real-world example of import:
```nix
let
  overrides = {
    builtins = builtins // overrides;
  }
  // import ./lib.nix;
in
scopedImport overrides ./imported.nix
```

== Type inference in a language server setting
A language server setting adds one more level of complexity. A language server has to handle the communication between client (an editor like vim, emacs, vscode, etc.) and the server itself. It will be notified frequently of code changes and has to adapt to these changes almost immediately to not annoy the user. This is why rust-analyzer and nil, which I take as template for my own efforts, have chosen to use or create _incremental computation_ frameworks for the rust language.
The one used by rust-analyzer and nil (which is based off of rust-analyzer) is _salsa_. The name stems from the underlying red-green algorithm that decides whether a function needs to be reevaluated because its arguments changed or whether the memoized return value can be returned immediately.
In the end salsa consists of _inputs_, _tracked functions_ and _tracked structs_. Inputs are divided into their durability and given to tracked functions. These tracked functions record the inputs and do some arbitrary computation with them. During these computations, the functions might create immutable tracked structs which can act as new inputs to other tracked functions. Tracked structs are interned into a db and act as a single identifier which are cheap to copy around and provide great performance benefits. With these components alone it is possible to create a hierarchy of pure functions that allow for reproducibility.

When implementing this incrementality framework one has to decide where to draw the line between tracking everything too closely such that the framework bloat adds latency and tracking too few intermediate results such that recomputation is heavy again.

The generalized structure of the three language servers ought to be as follows. A user opens a file and the lsp client sends the text to the language server. The language server stores the text somewhere and adds it to the typing pipeline. The first step of this pipeline is of course lexing and parsing the file. Nil already provides a parser for lossless syntax trees that are handy for error reporting. The file is then lowered into another HIR which is more or less syntax independent and thus changes less frequently. This is necessary because otherwise everything would have to be recomputed all the time. After this, the HIR is given to the inference algorithm that tries to infer a type.
For this an arena is used to store all of the small, allocated code fragments. This is another form of interning, that enables us to only work with small ids instead of cloning the actual heavy AST.

I am currently working to transition from salsa 0.17-pre2 to salsa 0.24 which is the newest version of salsa. As a lot has changed and virtually every part of code is touched, this is very time consuming.

= Code Overview
*Inputs of LSP*:
- `File {content: string, }`

*Inputs of infer:*
- `AST { With(ExprId, ExprId) }` (lowered AST with expressions from the arena)

*Tracked structs:*
- `Ty { Lambda(Ty), With(Ty, Ty)}` (enum that stores the whole AST)
- `Context {bindings: Vec<_>, }`
- `TyVar {lower_bounds: Vec<Ty>, upper_bounds: Vec<Ty>, level: int}`

*Functions:*
- `infer` (main work)
  - calls itself with subtrees of the AST and new contexts
  - *Mutates* context
- `constrain` (constrains two types to be the same)
  - calls itself with subtrees of Ty and might cycle
  - *Mutates* Type variables → *Changes context*
- `coalesce` (reduce types to unions and intersections)
  - Create new types
- `extrude` (fix levels of problematic variables in a type scheme)
  - only creates new types
- `freshen_above` (Add new type variables at level > x)
  - only creates new types

#pagebreak()
= Appendix A <prelude>
== List of Builtins
- *abort* `s` : Abort Nix expression evaluation and print the error message `s`.
- *add* `e1 e2` : Return the sum of the numbers `e1` and `e2`.
- *addDrvOutputDependencies* `s` : Copy string `s` while turning constant string context elements into derivation-deep string context.
- *all* `pred list` : Return `true` if `pred` returns `true` for all elements of `list`, else `false`.
- *any* `pred list` : Return `true` if `pred` returns `true` for any element of `list`, else `false`.
- *attrNames* `set` : Return the attribute names of `set`, sorted alphabetically.
- *attrValues* `set` : Return the values of attributes in `set`, ordered by sorted names.
- *baseNameOf* `x` : Return the last component of path or string `x`.
- *bitAnd* `e1 e2` : Bitwise AND of integers `e1` and `e2`.
- *bitOr* `e1 e2` : Bitwise OR of integers `e1` and `e2`.
- *bitXor* `e1 e2` : Bitwise XOR of integers `e1` and `e2`.
- *break* `v` : In debug mode, pause evaluation and enter REPL; otherwise return `v`.
- *builtins* : A set containing all built-in functions and values.
- *catAttrs* `attr list` : Collect the attribute `attr` from each set in `list`, ignoring sets without it.
- *ceil* `double` : Round `double` up to the nearest integer.
- *compareVersions* `s1 s2` : Compare version strings; returns `-1`, `0`, or `1`.
- *concatLists* `lists` : Flatten a list of lists into a single list.
- *concatMap* `f list` : Equivalent to `concatLists (map f list)`.
- *concatStringsSep* `sep list` : Join strings in `list` with separator `sep`.
- *convertHash* `args` : Convert a hash string between formats (base16, sha256, SRI, etc.).
- *currentSystem* : System string like `"x86_64-linux"`.
- *currentTime* : Unix time at moment of evaluation (cached).
- *deepSeq* `e1 e2` : Like `seq`, but fully evaluate nested structures in `e1` first.
- *dirOf* `s` : Directory component of string `s`.
- *div* `e1 e2` : Integer division.
- *elem* `x xs` : `true` if `x` is in list `xs`.
- *elemAt* `xs n` : Return the `n`-th element of `xs`.
- *false* : Boolean literal `false`.
- *fetchClosure* `args` : Fetch a store path closure from a binary cache.
- *fetchGit* `args` : Fetch a Git repo or revision.
- *fetchTarball* `args` : Download and unpack a tarball.
- *fetchTree* `input` : Fetch a tree or file with metadata.
- *fetchurl* `arg` : Download a URL and return store path.
- *filter* `f list` : Return elements where `f` yields `true`.
- *filterSource* `pred path` : Copy sources filtering by `pred`.
- *findFile* `search lookup` : Search `lookup` in `search` path.
- *floor* `double` : Round `double` down to nearest integer.
- *foldl'* `op nul list` : Left fold over `list` with `op`.
- *fromJSON* `e` : Parse JSON string `e` into Nix value.
- *fromTOML* `e` : Parse TOML string `e` into Nix value.
- *functionArgs* `f` : Return formal argument set of function `f`.
- *genList* `generator length` : Generate list of given `length` using `generator`.
- *genericClosure* `attrset` : Compute transitive closure of a relation.
- *getAttr* `s set` : Return attribute `s` from `set`.
- *getContext* `s` : Return derivation context of string `s`.
- *getEnv* `s` : Return environment variable `s`.
- *getFlake* `args` : Fetch flake reference and outputs.
- *groupBy* `f list` : Group elements by key `f(element)`.
- *hasAttr* `s set` : `true` if `set` has attribute `s`.
- *hasContext* `s` : `true` if string `s` has nonempty context.
- *hashFile* `type p` : Compute hash of file at `p`.
- *hashString* `type s` : Compute hash of string `s`.
- *head* `list` : First element of `list`.
- *import* `path` : Load and evaluate Nix file at `path`.
- *intersectAttrs* `e1 e2` : Attributes in `e2` whose names occur in `e1`.
- *isAttrs* `e` : `true` if `e` is an attribute set.
- *isBool* `e` : `true` if `e` is a boolean.
- *isFloat* `e` : `true` if `e` is a float.
- *isFunction* `e` : `true` if `e` is a function.
- *isInt* `e` : `true` if `e` is an integer.
- *isList* `e` : `true` if `e` is a list.
- *isNull* `e` : `true` if `e` is `null`.
- *isPath* `e` : `true` if `e` is a path.
- *isString* `e` : `true` if `e` is a string.
- *langVersion* : Integer of current Nix language version.
- *length* `e` : Length of list `e`.
- *lessThan* `e1 e2` : `true` if `e1 < e2`.
- *listToAttrs* `e` : Convert list of `{name, value}` to attrset.
- *map* `f list` : Apply `f` to each element of `list`.
- *mapAttrs* `f attrset` : Apply `f` to each attribute in `attrset`.
- *match* `regex str` : If `regex` matches `str`, return capture groups, else `null`.
- *mul* `e1 e2` : Multiply integers `e1 * e2`.
- *nixPath* : List of search path entries for lookups.
- *nixVersion* : String version of Nix.
- *null* : Literal `null`.
- *outputOf* `drv out` : Return output path of derivation.
- *parseDrvName* `s` : Parse a derivation name into components.
