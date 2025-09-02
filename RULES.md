# HOUSE RULES

After almost a decade, we need some discipline in here.

- Things should be defined before they are used.

    - Prefer `let`-bindings to `where` clauses.

- Prefer first-order code to generic code.

    - Prefer collection-specific `map` to `fmap`.

        **Examples**
        - Prelude's `map`
        - `Vector.map`

- Avoid point-free code.

    Or, Parens are good, actually.

    **Exceptions**
    - Okay to use with lenses in restricted settings, like with `Lens.Family2.State`
    - Okay for simple compositions like `fail . show`, `not . null`

- If something looks terrible without operators and/or point-free style, break it up.

- Avoid `(.)`.

- Avoid `($)`.

    **Exceptions**
    - Okay to use before `do`
    - Okay to use with `forM` or `forM_`
    - Okay to use with `when`
    - Okay to use with `with`-style constructs
    - Okay when you would use `@@` in OCaml

- Avoid `(<$>)`.

    **Exceptions**
    - Okay to use for defining lenses
    - Okay to use in Applicative expressions

- Avoid `TupleSections`.

- Use `pure` instead of `return`.

- Eta reduction is okay (for now, subject to change).

- `(>>=)` and `(=<<)` are okay: this is Haskell, not LA.

- `NamedFieldPuns` are good.

- `case` is good.

- `PatternGuards` are okay, but only in small amounts, often in cases where you'd need chained conditionals.

- Functions from other modules should be prefixed with their module name.

    **Exceptions**
    - It's okay to import operators unqualified.
    - Functions from neighboring "common" modules may be imported and used unqualified.

- **When in doubt, ask "What would an OCaml programmer do?"**
