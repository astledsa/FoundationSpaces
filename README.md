# Foundation Spaces

This repository is where I will learn and explore the various way one can write compilers and type theories. Here's the roadmap (which may or may not change later one). 

K0  Untyped λ-calculus
 │
K1  Simply Typed λ-calculus
 │
K2  System F
 │
K3  Dependent λ-calculus
 │
K4  MLTT core
 │
K5  MLTT + universes + identity
 │
 ├───────────────────────┬───────────────────────────────┐
 │                       │                               │
 │                       │                               │
CIC branch             HoTT branch                    Extensional branch
 │                       │                               │
C1 Inductives          H1 HoTT core                   E1 Equality reflection
 │                       │                               │
C2 Families            H2 Univalence                  E2 Extensional TT
 │                       │
C3 CIC                  ├───────────────┐
 │                       │               │
C4 Lean-ish          Cubical branch   2-level/modal branch
                     │
                     U1 Interval
                     │
                     U2 Path types
                     │
                     U3 Cofibrations
                     │
                     U4 Composition/filling
                     │
                     U5 Computational univalence
                     │
                     ├───────────────────────────┐
                     │                           │
                Cubical TT                  Simplicial branch
                                                 │
                                            S1 Shapes
                                                 │
                                            S2 Shape substitution
                                                 │
                                            S3 Extension types
                                                 │
                                            S4 Synthetic ∞-categories
                                                 │
                                            Rzk-like core
