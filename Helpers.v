Require Import List.
Import ListNotations.

Class Functor T := {
  fmap : forall {A B}, (A -> B) -> T A -> T B
}.

Class Applicative T := {
  pure : forall {A}, A -> T A;
  app  : forall {A B}, T (A -> B) -> T A -> T B
}.

Fixpoint list_app {A B} (fs : list (A -> B)) (xs : list A) :=
  match fs with
    | []      => []
    | g :: gs => map g xs ++ list_app gs xs
  end.

Instance listApplicative : Applicative list := {
  pure A x       := [x];
  app  A B fs xs := list_app fs xs
}.

Infix "<*>" := app (at level 60).

Example sumElems : [plus 2; plus 3] <*> [7; 8; 9] = [9; 10; 11; 10; 11; 12].
  auto.
Qed.
