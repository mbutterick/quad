#lang quadwriter/markdown

```
excl-middl→¬¬-elim : ∀ {A : Set} → A ⊎ ¬ A → (¬ ¬ A → A)
excl-middl→¬¬-elim (inj₁ a) = λ ¬¬a → a
excl-middl→¬¬-elim (inj₂ ¬a) = λ ¬¬a → ⊥-elim (¬¬a ¬a)
```