## Patterns:
- make methods pure by adding `var`s as parameters and adding a return type
- extract (cohesive) parts that belong together
- First new method, then replace on extraction to minimize time in uncompilable state

## Heuristics
- prefer Pure functions
- minimize uncompilable time
- commit often!
- reveal intention
- remove duplication
- Limit scope as much as possible
- rerun tests after every change
- focus on higher level policies before diving into the nitty gritty
-
