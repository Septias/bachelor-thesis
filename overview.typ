#import "functions.typ": *

= Current Efforts of Typing the Nix language and Coding an LSP
In this document I try to lay out the current efforts of creating a type system and its implementation as a language server written in rust. This document should act as an overview such that we (Peter Thiemann, Taro Sekiyama and me) have a common progress report upon which we can act. Most of it is WIP and there are a lot of loose ends and even contradictions. I hope not to confuse you too much with it though.

What follows is first the current efforts of a type system and then some implimentatino notes.


= Type system
#stack(
  dir: ttb,
  spacing: 1em,
  colored_box(title: "Syntax: Basetypes", color: blue, [$
      #type_name("Boolean") b & ::= "true" | "false"             \
       #type_name("String") s & ::= "[a-z A-Z _]*"               \
         #type_name("Path") p & ::= "(./|~/|/)([a-z A-Z .]+/?)+" \
       #type_name("Number") n & ::= "([0-9]*\.)?[0-9]+"          \
        #type_name("Label") l & ::= "[a-z A-Z _]*"               \
    $]),

  colored_box(
    title: "Syntax",
    color: blue,
    [
      $
        t ::=
        #type_name("Basetype") &| b | s | p | n \
        #type_name("Record") &| {#text(fill: blue)[b] _i} | #text(weight: "bold")[rec] {#text(fill: blue)[b] _i}\
        #type_name("Array") &| [ space t_0 space t_1 space ... space t_n space] \
        #type_name("Has-Attribute") &| t #text(weight: "bold", " ? ") l \
        #type_name("Has-Attribute-Or") &| t.l #text(weight: "bold")[or] t \
        #type_name("Record-Concat") &| t "∕∕" t \
        #type_name("Array-Concat") &| t "⧺" t \
        #type_name("Lookup") &| t "." l \
        #type_name("Dynamic-Lookup") &| t "." t \
        #type_name("Function") &| #text(fill: red, "pat"): t \
        &| #text(weight: "bold")[let] #text(fill: green)[a] _i #text(weight: "bold")[in] t \
        &| #text(weight: "bold")[if] t #text(weight: "bold")[then] t #text(weight: "bold")[else] t \
        &| #text(weight: "bold")[with] t; t \
        &| #text(weight: "bold")[assert] t; t
      $
    ],
  ),
  [
    - Should paths be added as their own syntax category? I feel like that makes sense if you want to create intrinsically scoped syntax but we are very far from that.
  ],
  colored_box(
    title: "Syntax: Inner record",
    color: blue,
    [$
        #text(fill: blue)[b] & ::= l: t; " | " "ir" \
        "ir" & ::= "inherit" l_0 " … " l_n; " | " "inherit" (p) " " l_0 " … " l_n; \
      $],
  ),
  colored_box(title: "Syntax: Pattern", color: blue, [$
      #text(fill: red, "pat") & ::= { space e_i space }                        \
                              & | { space e_i, #text(weight: "bold")[…] space} \
                              & | l                                            \
                          "e" & ::= l | l space ? space t                      \
    $]),

  colored_box(
    title: "Syntax: Inner let",
    color: blue,
    [$
        #text(fill: green)[a] & ::= l = t; " | " "il" \
        p & ::= l | p.l \
        "il" & ::= "inherit" l_0 " … " l_n; " | " "inherit" (p) " " l_0 " … " l_n; \
      $],
  ),
  // colored_box(title: "Wellformedness", color: red_700, []),
  colored_box(title: "Reduction rules", color: blue, [$
                Σ, (x: b)a & arrow.long Σ, b[a := x]           \
               Σ, {l: t}.l & arrow.long Σ, t                   \
      Σ, "with "t_1"; "t_2 & arrow.long Σ,{..t_1} t_2          \
                 Σ, a ++ b & arrow.long [a_0 … a_n, b_0 … b_n] \
               Σ, a \/\/ b & arrow.long [...b, ...a]           \
    $]),


  colored_box(
    title: "Types",
    color: green,
    [
      $
        tau &::= tau -> tau | alpha | top | bot \
        #type_name("Type connectives") &| tau union.sq tau | tau inter.sq tau \
        #type_name("Recursion") &| mu alpha space tau \
        #type_name("Base Types") &| "bool" | "string" | "path" | "num" \
        #type_name("Records") &| {l_0 : tau" ... "l_n: tau} | ⟨l_0 : tau " ... " l_n: tau⟩ \
        #type_name("Lists") &| [" "tau" "] | [" "τ_1" "…" "τ_n" "] \
        #type_name("Patterns") &| ({l_0: tau; ...; l_n: tau }, "bool")
      $
    ],
  ),
  [Do we need an option type because we have functions with default arguments?],
  colored_box(
    title: "Constraining rules",
    color: green,
    [
      Constraining takes two types t₁ and t₂ and constraints the first type to be subtype of the other.
      $
        (τ_1 → τ_2),&& (τ_3 → τ_4) &arrow.squiggly "constrain"(τ_3, τ_1); "constrain"(τ_2, τ_4) \
        {τ_i} ,&& {τ_j} &arrow.squiggly ∀i. "constrain"(τ_i, τ_i) "  " A \
        {τ_i} ,&& ({τ_j}, #text("true", weight: "bold")) &arrow.squiggly ∀i. "constrain"(τ_i, τ_j) \
        {τ_i} ,&& ({τ_j}, #text("false", weight: "bold")) &arrow.squiggly "TODO" \
        [τ_1] ,&& [τ_2] &arrow.squiggly "constrain"(τ_1, τ_2) \
        (("lo", "up"),&& "rhs") : "if #rhs.level()" <= "#lhs.level()" &arrow.squiggly "lo" += "rhs"; "foreach(lo): lo => constrain(lo, rhs)" \
        ("lhs" ,&& ("lo", "up")) : "if #lhs.level()" <= "#rhs.level()" &arrow.squiggly "lo" += "lhs"; "foreach(up): up => constrain(lhs, up)" \
        ("lo", "up"),&& "rhs" &arrow.squiggly "constrain(#lhs, extrude(rhs, false, #lhs.level()))" \
        "lhs",&& ("lo", "up") &arrow.squiggly "constrain(extrude(lhs, true, #rhs.level()), rhs)" \
      $
      In this code, \#lhs and \#rhs respectively reference the first and second argument of the constrain function of the current call. They are thought of beeing available in every case distinction.
      *Conditions*:
      - A: Both records are sorted lexiographically and thus align their fields. If some field of the right record is not present in the left one, then.
    ],
  ),
  colored_box(title: "Typing Rules", color: purple, typings([], (
    (
      derive(
        "T-Var",
        ($x: ∀ arrow(α). space τ in Γ$,),
        $Γ tack x: τ[arrow(α) \\ arrow(τ)]$,
      ),
      derive("T-Abs", ($Γ, x: τ_1 tack t: τ_2$,), $Γ tack λ x. t: τ_1 → τ_2$),
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
        "T-List-Concat-Hom",
        ($Γ tack a: "[τ]"$, $Γ tack b: "[τ]"$),
        $Γ tack a "++" b: "[τ]"$,
      ),
      derive(
        "T-List-Concat-Multi",
        ($Γ tack a: [arrow(τ_1)]$, $Γ tack b: [arrow(τ_2)]$),
        $Γ tack a "++" b: [arrow(τ_1)arrow(τ_2)]$,
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
        $Γ tack "let" x_0 = t_1; ... ; x_n = t_n "in" t: τ$,
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
  ))),

  colored_box(
    title: "Subtying Rules",
    color: purple,
    typings([], (
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
    )),
    // TODO: no two child elements?
    // pad_stack((
    //   $lt.tri(H_0, H_1) = lt.tri H_0, lt.tri H_1$,
    //   $lt.tri(gt.tri H) = H$,
    //   $lt.tri ( τ_0 <= τ_1) = τ_0 <= τ_1$,
    // ))
  ),

  colored_box(
    title: "Lists",
    color: red,
    [$
        #align(center)[
          #pad_stack((
            derive("S-Lst", ($ Γ tack τ_1 <= τ_2 $,), $Γ tack [τ_1] <= [τ_2]$),
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
          ))
        ]
      $],
  ),
  colored_box(title: "Standart Library", color: red, [$
      "import": "path" -> {...}? -> t
    $]),
)
= TODO
- Define Kinds? needed?
  - Only needed for when you have different type classes (for rows, variants, etc.) I only have one here because there are no row variables
- Define Wellformedness? needed?
  - Not really, stuck is just a possible state and we have a small step semantics
- Define Evaluation contexts?
- Define Values? needed?
  - Not really needed, since small-step
- Add polrized variables?

= Actual TODO
- Add builtins
- Define Constraining
- Define Operational Semantics


= Datatypes
== Records
Records are defined very simple in this type system. The only supported record type is a list of label → type mappings which can be added during subtyping. There is no way to reorder them, or remove some. During typing, multiple object constraints are concatenated, so there is a way to add new fields. Two problems are present with the current implementation of the typesystem.
Firstly we have have the `//` operator which implements _open record extension_. Given two records `A: { X: string, Y: int }` and `B: { X: int }` the open record concatenation between the two records `(C = A \\ B)` is `C: {X: int, Y: int}`. This together with the generic subtyping rule T-Sub makes the typesystem unsound, because fields can be removed, making the recod B empty (`T-SUB: B -> {}`). In this case, the typesystem would predict `A.X` to be of type `string` which is simply wrong after the application.


== Context Strings
Context strings and dyanamic lookup share the same syntax in that you can you some arbitrary term t into braces like this `${t}`. For ordinary strings and paths, the value of t will be coerced into a string and added literally. From a typing perspective this is the easy case because inserted values get a constraint of string and thats it. For dyanmic lookups however...


== Dynamic Lookup
Context string allow lookups of the form `a.${t}` where t is allowed to be any expression that ultimately reduces to a string. The reduced string is then used to index the record which a is supposed to be. Since a type system only computes a type and not the actal value, the only possible approach to handle first-class-labels is to evaluate nix expressions to some extend. Writing a full evaluator is probably too much, but there could be heuristics for simple evaluation. One approach would be to work backwards from return statements in functions up until it gets to wieldy.
This would also mean to implement the standart library functions like map, readToString etc. One ray of hope is that these were probably already implemented in Tvix.


= Constructs
== With Statements
With statements in nix are very tricky. They basically allow to introduc all bindings of a record into the following expression. For this, the first expression (A) in $"with " A"; "B$ has to reduce to a record. If this does not work, typing should raise an error. For explicit records, the following typing is straigt forward. Just introduce all fields to the scope and continue typchecking $B$. For the case that A is a type variable, it gets tricky again because of the generic subsumption rule. When A is subtyped like follows $A: {X: "int"} arrow A: {}$, then the field X would not be accessible in the function body.
The second problem is what I call the _attribution problem_. This happens when there is a chain of with statements $"with "A; ("with "B;) t$ and B is a type variable. Now when trying to lookup $x$ in t, it is unclear whether x came from B or A. But only in the case that


== Inherit Statements
In my Bachelors Thesis, I handled inherit statements as syntactic rewrites which is still the preferred way to implement the feature. After I have written down the constraing rules, we can see if that is still the best way.


== Function Patterns
Functions luckily are pure and functional which helps in inferring a proper type immensely. The patterns though add back a bit of a hussle.
Pattern are given als records, showing which exact fields are wanted for this function. The ellipsis `(…)` then allows for arbitrary extra fields, and the `?` question mark syntax for default values.
To handel these, all expected record fields need to be present in the function argument so a record constraint with these fields can be added to the argument of the function. If a default value is given for some record fields, this a constraint can be made on the arguments aswell.


= Laziness and Recursiveness
Laziness and recursion occur in two language constructs. The first one being recursive records and the second one being recursive let bindings. To evaluate the properly a lazy evaluation scheme is needed. The currently used approach to handle this is as follows:
When typing a let binding or record, the algorithm adds all name bindings to the context up-front. This way referenced values will not be undefined when looked up, even if their definition was not type checked yet. The typcheck algorithm then starts with the first Label $A$ which references an unchecked expression labeled $B$.
During typechecking of unchecked expressions (i.e $B$), they can simply be used to create upper and lower bounds (constraints). For empty type variables that is fine to do, but what happens when we actually check an unchecked expression (i.e $B$)? The inherent structure only then unfolds and during a normal typechecking flow, the type variable would get upper and lower bounds. These bounds are missing on the typecheck run of $A$ though.
The implications of this are not clear to me yet. We have a later run of type simplification which could maybe tie the knots.


= A Note about Implementation
As if creating a sound type system for nix and all its features isn't enough work already, implementing an efficient type inference algorithm seems just as hard. The unique problem of nix is that every functionality (all 100.000 packages, the operating system, and the standart library) are part of one big piece of code that roots in a single file at github:com/nixpkgs/flake.nix or github.com/nixpkgs/default.nix, depending whether you use a flake based system or not. To handle this, the nix evaluator heavily relies on the laziness features of the language as to not have to evaluate all of the packages on its own. If you really wanted to auto-complete nixos options (which is the ultimate goal) you would have to parse and type the nixos module system in all its greatness and whatever is needed to reach it. This includes the standart library and bootstrappign code for the module system. To even reach this point, the type inference algorithm has to support the same kind of laziness, the nix evaluator uses because otherwise it would just keep evaluating forever.


== Practical Type Inference in Face of Huge Syntax
Code inference in the general case is similar to depth-first-search where you dig down one syntax tree and only return as soon as all branches have been exhausted. Since nix trees are huge, this approach is not feasible and one has to lean towards a breath-first-search style, which leans towards the currently inferred file and stops when too far off. To achive this behaviour, the inference algorithm at some point has to decide to stop inference and jump to another unfinished function, remembering at which place it left of.
In the nix language, there are two natural plces to do so. Laziness of records and let-statements give the natural approach that every newly named binding is a stop-point at which inference only proceed as far as needed. One heuristic could be to go two more functions down and then return to the let or record to generate at least some approximation of the final type.
The import statements semantics of nix come in very handy at this point. Import statements act just as function calls with the only difference beeing, that the goto locations is defined by path and not by name. Other than that, they can take arguments just as a function, and then try to appyl given arguments to the files expression. This language design comes in very handy because that way, import staments do not occur at the top of the file where it would need to be decided how to continue typechecking them. They occur right at the location where they are needed, sometimes in let statements or record fields. This way, the laziness of records and let statements could already be enough to get laziness into the language.
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
A language server settings adds one more level of complexity to the stack. A language server has to handle the communication between client (an editor like vim, emacs, vscode) and the server itself. It will be notified frequently of code changes and has to adapt to these changes almost immediately to not annoy the user. This is why rust-analyzer and nil, which I take as template for my own efforts, have chosen to use or create _incremental computation_ frameworks for the rust language.
The one used by rust-analyzer and nil (which is based off of rust-analzyer) is _salsa_. The name stems from the underlying red-green algorithm that decides whether a function needs to be reevaluated because their arguments changed or whether the memoized return value can be returned immediately.
In the end salsa consists of _inputs_, _tracked functions_ and _tracked structs_. Inputs are divided into their durability and given to tracked functions. These tracked functions record the inputs and do some arbitrary computation with them. During these computations, the functions might create immutable tracked structs which can act as new inputs to other tracked functions. Tracked structs are interned into a db and act as a single identifier which are cheap to copy around and provide great performance benefits. With these components alone it is possible to create a hirarchy of pure functions that allow for reproducability.

When implementing this incrementality framework one has to decide where to draw the line between tracking everything too closely such that the framework bloat adds latency and tracking too few intermediate results such that recomputation is heavy again.

The generalized structure of the three language servers ought to be as follows. A user opens a file and the lsp client sends the text to the language server. The lange server stores this somewhere and adds it to the typing pipeline. The first step of this pipeline is of cause lexing and parsing the file. Nil already provides a parser for lossless syntax trees that are handy for error reporting.
