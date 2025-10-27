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
- Row Polymorphism
  - "Inference Hell"

== Scoped Records
- Because the only change (visibly) is the notion of row equality and permit duplicate labels, the system can be integrated with standard Hindley–Milner polymorphism (and qualified types, even higher‐rank via MLF) with relatively modest modifications.
- In fact, in the broader ecosystem, there are warnings about unsound type inference when extensible records are involved.

