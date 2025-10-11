// styling
#set heading(numbering: none)
#show heading.where(level: 1): set text(font: "FreeSans", fill: luma(20))
#show heading.where(level: 2): set text(
  font: "FreeSans",
  fill: luma(20),
  size: 12pt,
)
#show math.equation: set text(size: 14pt)

// colors
#let red_700 = rgb(185, 28, 28)


// functions
#let type_name(name) = text(
  font: ("FreeSans",),
  size: 11pt,
  fill: rgb(10, 10, 10),
  weight: "semibold",
  name,
)
#let colored_box(title: "", color: blue, content) = {
  block(breakable: false, {
    stack(
      dir: ttb,
      spacing: 0.5em,
      align(left, {
        show heading: set text(fill: color)
        heading(title)
      }),
      rect(
        stroke: 2pt + color,
        radius: 4pt,
        width: 100%,
        inset: 8pt,
        content,
      ),
    )
  })
}
#let derive(name, prem, conclusion) = [
  #table(
    stroke: none,
    inset: (x: 0pt, y: 5pt),
    align: center,
    table.cell(align: start)[#smallcaps(name)],
    table.cell(inset: (y: 5pt), [#prem.join("     ")]),
    table.hline(),
    table.cell(inset: (y: 10pt), [#conclusion]),
  )
]

#let pad_stack(ct) = stack(
  dir: ltr,
  spacing: 3em,
  ..ct,
)

#let to_stack(item) = pad_stack(item)

#let typings(caption, items) = figure(
  align(
    center,
    grid(
      align: center,
      ..items.map(pad_stack)
    ),
  ),
)


#stack(
  dir: ttb,
  spacing: 1em,
  colored_box(
    title: "Syntax: Basetypes",
    color: blue,
    [
      $
        #type_name("Boolean") b & ::= "true" | "false" \
         #type_name("String") s & ::= "[a-z A-Z _]*" \
           #type_name("Path") p & ::= "(./|~/|/)([a-z A-Z .]+/?)+" \
         #type_name("Number") n & ::= "([0-9]*\.)?[0-9]+"
      $
    ],
  ),
  colored_box(
    title: "Syntax",
    color: blue,
    [$
      t ::= #type_name("Basetype") &| b | s | p | n \
      #type_name("Record") &| "rec" {l_0 : t; ...; l_n : t} \
      #type_name("Array") &| [ space t_0 space t_1 space ... space t_n space] \
      // &| t + t | t - t | t * t | t space \/ space t \
      // &| (t "&&" t) | (t "||" t) | (t -> t) | !t \
      // &| t < t | t <= t | t >= t | t > t | t == t | t != t \
      #type_name("Has-attribute") &| t "? " l \
      #type_name("Has-attribute-or") &| t.l "or" t \
      #type_name("Record-concat") &| t "//" t \
      #type_name("Array-Concat") &| t "++" t \
      #type_name("Function") &| "pat": t \
      &| "let" a_i "in" t
      &| "if" t "then" t "else" t \
      &| "inherit (t) t;" \
      &| "with" "set"; t \
      &| "assert" t; t
    $],
  ),
  colored_box(
    title: "Syntax: Pattern",
    color: blue,
    [
      $
        "elem" & ::= x | x space ? space t \
         "pat" & ::= { space "elem"_0, dots, "elem"_n space } \
               & | { space "elem"_0 , dots, "elem"_n, space ... space} \
               & | x
      $
    ],
  ),
  colored_box(
    title: "Syntax: Inherit",
    color: blue,
    [
      $
        p & ::= x | p.x \
        s & ::= "inherit" x; | "inherit" (p) " " x; \
        a & ::= x = t; | s
      $
    ],
  ),

  colored_box(
    title: "Wellformedness",
    color: red_700,
    [],
  ),
  colored_box(
    title: "Types",
    color: green,
    [$
      tau ::= &tau -> tau | {l_0 : tau; ...;l_n: tau} | alpha | top | bot | tau union.sq tau | tau inter.sq tau | mu alpha tau \
      &| "bool" | "string" | "path" | "num" \
      &| [" "tau" "] \
      &| ({l_0: tau; ...; l_n: tau }, "bool")
    $],
  ),
  colored_box(
    title: "Typing Rules",
    color: purple,
    typings(
      [],
      (
        (
          derive(
            "T-Var",
            ($x: ∀ arrow(α). space τ in Γ$,),
            $Γ tack x: τ[arrow(α) \\ arrow(τ)]$,
          ),
          derive(
            "T-Abs",
            ($Γ, x: τ_1 tack t: τ_2$,),
            $Γ tack λ x. t: τ_1 → τ_2$,
          ),
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
          derive(
            "T-Let",
            ($Γ, x: τ_1 tack t_1 : τ_1$, $Γ, x: ∀ arrow(α). τ_1 tack t_2: τ_2$),
            $Γ tack "let rec" x = t_1 "in" t_2: τ_2$,
          ),
        ),
        (
          derive("T-Negate", ($Γ tack e: "bool"$,), $Γ tack !e: "bool"$),
          derive("T-Check", ($Γ tack e: {l: τ}$,), $Γ tack e ? l: "bool"$),
          derive("T-Or", ($Γ tack t_1: {l: τ_1}$, $Γ tack t_2: τ_2$), $Γ tack t_1.l "or" t_2: τ_1 union.sq τ_2$),
          
        ),
        (
          derive("T-List-Concat-Hom", ($Γ tack a: "[τ]"$, $Γ tack b: "[τ]"$), $Γ tack a "++" b: "[τ]"$),
          derive(
            "T-List-Concat-Multi",
            ($Γ tack a: [arrow(τ_1)]$, $Γ tack b: [arrow(τ_2)]$),
            $Γ tack a "++" b: [arrow(τ_1)arrow(τ_2)]$,
          ),
        ),
        (
          derive(
            "T-Rec-Update",
            ($Γ tack a: { l_i: τ_i }$, $Γ tack b: { l_j: τ_j }$),
            $Γ tack a "//" b: a backslash b union b$,
          ),
        ),          
        (
            derive(
    "T-Multi-Let",
    ($Γ overline([x_i: τ_i tack t_i : τ_i]^i)$, $Γ overline([x_i:∀ arrow(α). τ_i]^i) tack t: τ$),
    $Γ tack "let" x_0 = t_1; ... ; x_n = t_n "in" t: τ$,
  ),

        ),
        (
    derive("T-If", ($Γ tack t_1: "bool"$, $Γ tack t_2: τ$, $Γ tack t_3: τ$), $ "if" t_1 "then" t_2 "else" t_3: τ $),
          
        ),
        (
    derive(
      "T-With",
      ($Γ tack t_1 : {arrow(l): arrow(τ)}$, $Γ, l_0 : τ_0, ..., l_n: τ_n tack t_2: τ$, $l_i in.not Γ$),
      $Γ tack "with" t_1; t_2 : τ$,
    ),
          
        ),
        (
          derive("T-Assert", ($Γ tack t_1: "As<bool>"$, $Γ tack t_2: τ_2$), $Γ tack "assert" t_1; t_2: τ₂$),
        )
      ),
    ),
  ),
  colored_box(
    title: "Subtying Rules",
    color: purple,
    typings(
      [],
      (
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
      ),
    ),
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
