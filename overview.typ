#import "functions.typ": *


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
        #type_name("Record") &| {l_0 : t; ...; l_n : t} | #text(weight: "bold")[rec] {l_0 : t; ...; l_n : t}\
        #type_name("Array") &| [ space t_0 space t_1 space ... space t_n space] \
        #type_name("Has-attribute") &| t #text(weight: "bold", " ? ") l \
        #type_name("Has-attribute-or") &| t.l #text(weight: "bold")[or] t \
        #type_name("Record-Concat") &| t "∕∕" t \
        #type_name("Array-Concat") &| t "⧺" t \
        #type_name("Function") &| #text(fill: red, "pat"): t \
        &| #text(weight: "bold")[let] #text(fill: green)[a] _i #text(weight: "bold")[in] t \
        &| #text(weight: "bold")[if] t #text(weight: "bold")[then] t #text(weight: "bold")[else] t \
        &| #text(weight: "bold")[with] t; t \
        &| #text(weight: "bold")[assert] t; t
      $
    ],
  ),

  colored_box(title: "Syntax: Pattern", color: blue, [$
      "e" & ::= l | l space ? space t \
      #text(fill: red, "pat") & ::= { space "e"_0, dots, "e"_n space } \
      & | { space "e"_0 , dots, "e"_n, space #text(weight: "bold")[…] space} \
      & | l \
    $]),

  colored_box(title: "Syntax: Inner Let", color: blue, [$
      p & ::= l | p.l \
      s & ::= "inherit" l_0 " … " l_n; " | " "inherit" (p) " " l_0 " … " l_n; \
      #text(fill: green)[a] & ::= l = t; " | " s \
    $]),
  colored_box(title: "Syntax: Inner Record", color: blue, [$
      p & ::= l | p.l \
      s & ::= "inherit" l_0 " … " l_n; " | " "inherit" (p) " " l_0 " … " l_n; \
      #text(fill: blue)[b] & ::= x = t; " | " s \
    $]),
  // colored_box(title: "Wellformedness", color: red_700, []),
  colored_box(
    title: "Types",
    color: green,
    [
      $
        tau ::= &tau -> tau | alpha | top | bot | tau union.sq tau | tau inter.sq tau | mu alpha tau \
        &| "bool" | "string" | "path" | "num" \
        #type_name("Record") &| {l_0 : tau" ... "l_n: tau} | ⟨l_0 : tau " ... " l_n: tau⟩ \
        #type_name("Lists") &| [" "tau" "] | [" "τ_1" "…" "τ_n" "] \
        #type_name("Patterns") &| ({l_0: tau; ...; l_n: tau }, "bool")
      $
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
)

= Datatypes
== Records
Records are defined very simpel in this type system. We don't use Wadlers scoped record labels so reordering of fields in not possible.
The only supported record type is a list of label->type mappings which can be added during subtyping. There are no lacks predicates so fields present once will be there forever when they have not been subtyped. Two problems are present with the current implementation of the typesystem.
Firstly we have have the `//` operator which implements `open record extension`. Given two recortds `A: { X: string, Y: int }` and `B: { X: int }` the open record concatenation between the two records `(C = A \\ B)` is `C: {X: int, Y: int}`. This together with the generic subtyping rule T-Sub makes the typesystem unsound, because fields can be removed, making the recod B empty (`TSUB: B -> {}`). In this case, the typesystem would predict `A.X` to be of type `string` which is simply wrong after the application.


== Context strings
Context string allow for string based lookup of values.


= Constructs
== With statements
With statements in nix are very tricky. They basically allow to introduc all bindings of a record into


== Inherit statements
In my Bachelor Thesis, I handled inherit statements as actual syntactic rewrites which is still the preferred way to implement the feature. After I have written down the constraing rules, we can if that is still the best way.


== Function patterns
Functions luckily are pure and functional which helps in inferring a proper type immensely.
The patterns though add back a bit of a hussle.
Pattern are given als records, showing which exact fields are wanted for this function. The ellipsis(…) then allows for arbitrary extra fields, and the `?` question mark syntax for default values. For constraining, the patterns need to be handled *bidirectional*?. Firstly, all expected record fields need to be present in the function argument, so the flow is function -> argument. Then, the arguments have to be of proper type when a default value is given, this also flows function -> argument.
The last constraints are given for the extracted arguments in the function.


= Laziness and Recursiveness
Laziness and cursion occur in two lanugage constructs. The first one being recursive records and the second one being recursive let bindings. To evaluate the properly a lazy evaluation scheme is needed. The currently used approach to handle this is as follows:
When typing a let binding or record, the algorithm adds all name bindings to the context up-front. This way referenced values will not be undefined when looked up, even if their definition was not type checked yet. The typcheck algorithm then starts with the first Label $A$ which references an unchecked expression labeled $B$.
During typechecking of unchecked expressions (i.e $B$), they can simply be used to create upper and lower bounds (constraints). For empty type variables that is fine to do, but what happens when we actually check an unchecked expression (i.e $B$)? The inherent structure only then unfolds and during a normal typechecking flow, the type variable would get upper and lower bounds. These bounds are missing on the typecheck run of $A$ though.
The implications of this are not clear to me yet. We have a later run of type simplification.
