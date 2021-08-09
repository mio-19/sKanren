# miniKanren

An experimental miniKanren

Done:

+ multi-threading
+ not

Todo:

+ constraints (from [cKanren](https://github.com/calvis/cKanren) [paper](scheme2011.ucombinator.org/papers/Alvis2011.pdf)) as defer - `=/=`, `+fd` and more can always be rewritten with `=`, but sometimes we want to keep it
+ condp ? from [ICFP 2018 A Surprisingly Competitive Conditional Operator: miniKanrenizing the Inference Rules of Pie](https://icfp18.sigplan.org/details/scheme-2018-papers/7/A-Surprisingly-Competitive-Conditional-Operator-miniKanrenizing-the-Inference-Rules-)
+ branch prediction using information at run time
+ optimizations ? from "Guarded Fresh Goals: Dependency-Directed Introduction of Fresh Logic Variables"
+ forall ? classical logic: `(forall (v) domain goal)` = `(not (exists (v) (conj domain (not goal))))` or "Universal Quantification and Implication in miniKanren"'s method?
