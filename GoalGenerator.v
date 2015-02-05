Require Import mtac.
Require Import Program.
Require Import List.
Require Import Bool.
Require Import EqNat.
Import ListNotations.
Require Import Helpers.

(* Define binary trees with leaves of some type *)

Inductive Tree (t : Type) :=
| Leaf : t -> Tree t
| Node : Tree t -> Tree t -> Tree t.

Definition leaf {t} x   := Leaf t x.
Definition node {t} x y := Node t x y.

Definition ll := leaf ().

Definition nn : Tree () -> Tree () -> Tree () := node.
                      
Example leafTen : leaf 10 = Leaf nat 10.
  auto.
Qed.

Example nestedNodes : node (leaf 1) (node (leaf 2) (leaf 3)) = Node nat (leaf 1)
                                                                        (node (leaf 2)
                                                                              (leaf 3)).
  auto.
Qed.

Fixpoint pairOff {A} (ls rs : list (Tree A)) :=
  match ls with
    | []    => []
    | x::xs => map (node x) rs ++ pairOff xs rs
  end.

Fixpoint leaves {A} (t : Tree A) := match t with
                                      | Leaf _   => 1
                                      | Node l r => leaves l + leaves r
                                    end.

Theorem leavesSum {A} (t1 t2 : Tree A) : leaves t1 + leaves t2 = leaves (node t1 t2).
  auto.
Qed.

Fixpoint treesOf n :=
  match n with
    | 0   => []
    | 1   => [leaf tt]
    | S m => let smaller := treesOf m
             in  filter (compose (beq_nat n) leaves) (pairOff smaller smaller)
  end.

Example treesOfZero : treesOf 0 = [].
  auto.
Qed.

Example treesOfOne : treesOf 1 = [leaf ()].
  auto.
Qed.

Example treesOfTwo : treesOf 2 = [node (leaf ()) (leaf ())].
  compute. auto.
Qed.

Example treesOfThree : treesOf 3 = [nn ll (nn ll ll); nn (nn ll ll) ll].
  compute.

Fixpoint upTo n := match n with
                     | 0   => nil
                     | S m => cons 0 (map S (upTo m))
                   end.

Example upToFive : upTo 5 = [0; 1; 2; 3; 4].
  auto.
Qed.

Definition treesUpTo n := flat_map treesOf (upTo (S n)).

Goal [] = treesUpTo 3. compute.

Example treesUpToFour : treesUpTo 4 = [leaf (); node (leaf ()) (leaf ());
                                       node (leaf ()) (node (leaf ()) (leaf ()));
                                       node (node (leaf ()) (leaf ())) (leaf ())].
  compute. auto.
Qed.

(* Now we enumerate lists, with elements from a finite set *)

Fixpoint prependAll {A} (elems : list A) xs := (map cons elems) <*> xs.

Example prependNats : prependAll [1;2;3] [[10;20]; []; [30]] = [[1;10;20]; [1]; [1;30];
                                                                [2;10;20]; [2]; [2;30];
                                                                [3;10;20]; [3]; [3;30]].
  auto.
Qed.

(* Next we enumerate all pairs of numbers less than some limit *)

Fixpoint pairUp {A B} (xs : list A) (ys : list B) :=
  match xs with
    | [] => []
    | z :: zs => map (pair z) ys ++ pairUp zs ys
  end.

Example pairUpTwo : pairUp (upTo 2) (upTo 2) = [(0,0); (0,1); (1,0); (1,1)].
  auto.
Qed.

Definition allPairs n := let ns := upTo n
                         in pairUp ns ns.

Example allPairsTwo : allPairs 2 = [(0,0); (0,1); (1,0); (1,1)].
  compute. auto.
Qed.

(* Now we enumerate all pairs of number which sum to some value *)

Definition sumPairs n := filter (compose (beq_nat n) (prod_curry plus))
                                (allPairs n).

Example sumPairs4 : sumPairs 4 = [(1,3); (2,2); (3,1)].
  compute. auto.
Qed.

Definition pair_map {A B} (f : A -> B) p := match p with
                                              | pair x y => (f x, f y)
                                            end.

Example pairMapPlus : pair_map S (10, 20) = (11, 21).
  auto.
Qed.

(*
 * (x, y) represents trees with (S x) leaves on the left and (S y) on the right.
 * We can't use pairs *as* trees, since their types vary. Instead we convert
 * them to Tree. First we turn (x, y) into (node (leaf x) (leaf y)).
 *)

Fixpoint toTree (p : nat * nat) := match p with
                                     | (x, y) => node (leaf x) (leaf y)
                                   end.

Example toTreePair : toTree (3, 16) = Node nat (Leaf nat 3) (Leaf nat 16).
  auto.
Qed.

(* We turn pairs-which-sum-to-n into trees-with-n-leaves *)
Definition genTrees' n := map toTree (sumPairs n).

Example genNumTrees : genTrees' 3 = [node (leaf 1) (leaf 2);
                                     node (leaf 2) (leaf 1)].
  auto.
Qed.

Fixpoint unfoldNonDet (t : Tree nat) : list (Tree nat) :=
  match t with
    | Node l r => let    ls := unfoldNonDet l
                  in let rs := unfoldNonDet r
                     in (map node ls) <*> rs
    | Leaf 1   => [leaf 1]
    | Leaf 0   => [leaf 1]
    | Leaf m   => genTrees' m
  end.

Example unfoldEmptyLeaf : (unfoldNonDet (leaf 1)) = [leaf 1].
  reflexivity.
Qed.

Example unfoldFullLeaf : (unfoldNonDet (leaf 3)) = [node (leaf 1) (leaf 2);
                                                    node (leaf 2) (leaf 1)].
  compute. auto.
Qed.

Example unfoldNode : (unfoldNonDet (node (leaf 3)
                                         (leaf 3))) = [node (node (leaf 1) (leaf 2))
                                                            (node (leaf 1) (leaf 2));
                                                       node (node (leaf 1) (leaf 2))
                                                            (node (leaf 2) (leaf 1));
                                                       node (node (leaf 2) (leaf 1))
                                                            (node (leaf 1) (leaf 2));
                                                       node (node (leaf 2) (leaf 1))
                                                            (node (leaf 2) (leaf 1))].
  auto.
Qed.

Fixpoint unfoldedLeaves t :=
  match t with
    | Leaf 0   => true
    | Leaf 1   => true
    | Leaf _   => false
    | Node l r => unfoldedLeaves l && unfoldedLeaves r
  end.

Example zeroTrue : unfoldedLeaves (node (leaf 0) (node (leaf 0) (leaf 0))) = true.
  auto.
Qed.

Example zeroFalse : unfoldedLeaves (node (leaf 0) (leaf 10)) = false.
  auto.
Qed.

Definition flat_mapM {A B} (g : A -> M (list B)) :=
  mfix f (xs : list A) : M (list B) :=
  match xs with
    | [] => ret []
    | y::ys => y'  <- g y;
               ys' <- f ys;
               ret (y' ++ ys')
  end.

Definition unfoldFull :=
  mfix f (t : Tree nat) : M (list (Tree nat)) :=
  if unfoldedLeaves t
  then ret [t]
  else flat_mapM f (unfoldNonDet t).  

Fixpoint unitTree (t : Tree nat) := match t with
                                      | Leaf _ => leaf tt
                                      | Node l r => node (unitTree l) (unitTree r)
                                    end.
Check unfoldFull.
Definition treesOf n := fmap unitTree (unfoldFull (leaf n)).
  
Example treesOfThree : (run (treesOf 3)) = [node (leaf 0) (node (leaf 0) (leaf 0));
                                            node (node (leaf 0) (leaf 0)) (leaf 0)].
  compute. auto.



Example 

Check (fmap leaves (treesOf 4)).

Theorem treesOfSized : forall n, map leaves (treesOf n) 

Definition unfoldTree :=
  mfix (t : Tree nat) : Tree unit :=
  match t with
    | Leaf _ n   => 
    | Node _ l r => l' <- unfoldTree l;
                    r' <- unfoldTree r;
                    ret (Node unit l' r')
  end.


Definition genTrees :=
  mfix f n :=
  match n with
    | 0   => ret [Leaf unit tt]
    | S m => match sumPairs n with
               | [] => 
             in ts <- liftMP (map (pair_map f) ns);
                ret (map (prod_curry (Node unit)) ts)
  end.

Definition genTree :=
  mfix f (l : list bool) : M (Tree unit) :=
    match l with
      | nil => Leaf tt
      | cons 
    

Definition any        := {t : Type & t}.
Definition mkAny (t : Type) (x : t) : any := existT (fun x => x) t x.
Definition triv : any := mkAny True I.

(* Serves as a special marker for our context *)
Inductive Context : Type
  := CTX : list Type -> Context.

Definition ctxCons x xs := match xs with
                             | CTX ys => CTX (cons x ys)
                           end.

Ltac contextToList'
  := match goal with
       | [ H : ?T, L : Context |- _ ] => clear L;
                                         clear H;
                                         let n := fresh
                                          in set (n := ctxCons T L);
                                         contextToList'
       | _                            => idtac
     end.

Ltac contextToList k := contextToList' nil k.

Ltac makeGoals lst
  := match lst with
       | nil         => idtac "Finished"
       | cons ?t ?ts => assert t; makeGoals ts
     end.

Goal True.
  assert nat. exact 5. assert bool. exact true.
  clear nat.

Ltac clearCtx lst
  := match lst with
       | nil => idtac
                  | cons ?t ?ts => clear t; clearCtx 

Goal True.
  let y := contextToList' (cons nat nil : list Set)
   in (clearCtx y; makeGoals y).
  assert (l := nil : list nat).
  case l; intros; [
    auto
      | idtac
  ].
  contextToList' (nil : list Type) .

Goal 0=0.
  showComb (nil : list any).