#import "functions.typ": *


= Overview of Rec specifications
- Semantic Modeling
  - Very strong
  - Hard to formalize
- Scoped Records
  - Medium strong
  - Medium to formalize
- Subtyping
  - Medium strong
  - Medium to formalize
  - Hard to infer
- Algebraic subtying
- Semantic subtyping
- Row Polymorphism
  - "Inference Hell"

== Scoped Records
- Because the only change (visibly) is the notion of row equality and permit duplicate labels, the system can be integrated with standard Hindley–Milner polymorphism (and qualified types, even higher‐rank via MLF) with relatively modest modifications.
- In fact, in the broader ecosystem, there are warnings about unsound type inference when extensible records are involved.


== MLSub
MLsub supports records by category theory and some crazy construction in lattice space and order theory. Basically the theory is maximally extensible because it uses an algebraic definition of subtyping and creates a category, that is _extensible_ in math terms. Since it already posses primitive types (booleans), function types and records, there is really not so much that nix adds to it. The only difference being some primitive types, function patterns, the concat operators, two new language constructs (which are not too difficult) and lazyness.

Even though that might sound like a bit, the core calculus should in theory still work out if the constructs and function pattern don't break it. In multiple places, @dolan2017algebraic noted that an algebraic interpretation of subtying overcomes the shortcomming (non-termination, complexity) of syntactic definitions so it _could_ be interesting to do an algebraic definition aswell. The only problem is that it is very mathsy and needs a general and good understanding of _order theory_ as well as _category theory_. I don't think it is a good idea to add this to the stack now ☹

For now we should stick to a syntactic definition of this style:

We assume some set ℒ of record labels which are ranged over by i ∈ ℐ and j ∈ 𝒥.

#derive(
  "T-Rec",
  (${oi(l_i : τ_i)} "//" {overline(l_j : τ_j)^j}$,),
  ${overline(l_k : τ_k)^k} #h(0.5cm) k ∈ { ℐ ∪ 𝒥 } #h(0.5cm) τ_k = cases(τ_i "if" ∃i. l_k = l_i, τ_j "otherwise")$,
)


During type inference, when we reach the record-concat operator, simplify the bounds of a typevariable and force it to be a record. If both operands are records, T-Rec can be applied.


```rust
let ty1 = /* given */;
let ty2 = /* given */;
let acc_bounds = ty1.as_record().extend(ty2.as_record());
let tyvar = context.ty_var();
constrain(tyvar, acc_bounds);
return tyvar;
```

TODO: find out whether to use algebraic subtyping or syntactic.

#bibliography("bib/algebraic_subtyping.bib", style: "iso-690-author-date")

