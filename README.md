# classical-logic

[![Test](https://github.com/dannypsnl/classical-logic/actions/workflows/test.yml/badge.svg)](https://github.com/dannypsnl/classical-logic/actions/workflows/test.yml)
[![Coverage Status](https://coveralls.io/repos/github/dannypsnl/classical-logic/badge.svg?branch=develop)](https://coveralls.io/github/dannypsnl/classical-logic?branch=develop)

### organization

1. [first-order-logic.rkt](./first-order-logic.rkt): The language defnition of classical first-order logic(KF)
2. [propositional-logic.rkt](./propositional-logic.rkt): The language definition of classical propositional logic(K)
3. [subformula.rkt](./subformula.rkt): Compute subformula for any valid logic for propositional logic

The second part is about conversion, in this case propositional logic is just a kind of surface of first-order logic, internal conversion will go through same way.

1. [KF-clausal.rkt](./KF-clausal.rkt): convert KF to clausal form
   1. convert to prenex form
   2. remove implication
   3. skolemize to remove existential quantifier
   4. remove universal quantifier
   5. apply distribute laws to get clausal form
2. [KF-canonical.rkt](./KF-canonical.rkt): convert KF clausal to canonical form
   1. fuse `A and (B and C)` to `A and B and C`, in nanopass corresponding is `(∧ A (∧ B C))` to `(∧ A B C)`, so we need to update language definition
   2. reduce to minimal form, like `(∨ (¬ A) A B)` to `B`, this is why it called canonical form
3. [KF-cnf.rkt](./KF-cnf.rkt): convert KF canonical to conjunction normal form
   1. check it's canonical form
   2. produce internal representation of CNF
4. [main.rkt](./main.rkt): resolution and unification
