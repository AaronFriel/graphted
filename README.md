# graphted

Indexed type classes that track operations performed on them.
The index parameter then models control flow.

Implements graph-indexed type classes for:

- [X] `Pointed`
- [X] `Functor`
- [X] `Applicative`
- [ ] `Alternative`
  - TODO: `Alternative` vs `MonadOr`?
- [X] `Monad`
- [X] `MonadFail`
- [X] `MonadZero`
- [X] `MonadOr`
- [X] `MonadPlus`

May implement in the future, or may not make sense.

(Essentially: [`category-extras`](http://hackage.haskell.org/package/category-extras).)

- [ ] `Category`
  - [ ] `Kleisli`
  - [ ] `Cokleisli`
- [ ] `Arrow`
  - [ ] `ArrowZero`
  - [ ] `ArrowPlus`
- [ ] `Apply`, `Bind`, `Extend` a la [`semigroupoids`](http://hackage.haskell.org/package/semigroupoids)
- [ ] `Copointed`, `Comonad`
- [ ] `Foldable` (`Foldable1`)
- [ ] `Traversable` (`Traversable1`)
- [ ] `Traversable` (`Traversable1`)