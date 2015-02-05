Instance MFunctor : Functor M := {
  fmap A B f x := x' <- x; ret (f x')
}.

Instance MApplicative : Applicative M := {
  pure A x := ret x;
  app  A B f x := f' <- f; x' <- x; ret (f' x')
}.

Definition mLift (A : Type) (T : Type -> Type)
                 (f : forall B, B -> T B) (g : forall B, T B -> B)
                 (x : T (M A)) : M (T A) :=
  let x' := g (M A) x
  in  x'' <- x';
      ret (f A x'').

Definition mLiftPair {A B} (p : M A * M B) : M (A * B) :=
  match p with
    | (l, r) => l' <- l;
                r' <- r;
                ret (l', r')
  end.

Fixpoint mLiftList {A} (l : list (M A)) : M (list A) :=
  match l with
    | [] => ret []
    | x :: xs => x'  <- x;
                 xs' <- mLiftList xs;
                 ret (x' :: xs')
  end.

Definition liftMP {A} (x : list (M A * M A)) : M (list (A * A)) :=
  mLiftList (map mLiftPair x).      

