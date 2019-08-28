
<link rel="stylesheet" href="/pl2/css/friendly.css" />
<link rel="stylesheet" href="/pl2/css/lecture-style.css" />
<meta charset="utf-8" />


### More Functional Programming in OCaml

 * There are "nearly infinitely many" features we skipped in PL I and we are not going to cover all of them 
 * But let us cover a few, as a warm-up if nothing else
 
####  Pipelining

* In \*nix shell, each command can be viewed as a function from stdin to stdout
* ```blah | blahblah``` takes the output of ```blah``` and feeds it as input to ```blahblah```
* Reminder on how it is useful:

```bash
cat blah.tar.gz | gunzip | tar tf - | cat | grep .md
```

* OCaml version is ```|>```

```ocaml
# (|>);;
- : 'a -> ('a -> 'b) -> 'b = <fun>
```


```ocaml
# [] |> (List.cons 5) |> (List.cons 7) |> (fun x -> x) |> List.length;;
- : int = 2
```

 * ```cat``` in \*nix is like ```fun x -> x``` in OCaml
 * Associates left like shell pipe
 * Related to messsage chaining in OO languages

####  Folds and generalized folds

We will roughly follow the [Cornell notes](https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/hop/intro.html)

##### Fold right

* The function ```List.fold_right``` in OCaml "folds" a list to a single element via an operation.
* Example: 
```ocaml
List.fold_right ( * ) [2; 5; 7] 1
```

is ```2*(5*(7*(1)))```, i.e. ```70```.  The "right" refers to the zero (```1``` here) being on the right, not the left (it doesn't matter in the case of multiplication as it is associative).

Here is roughly how ```List.fold_right``` is implemented:

```ocaml
let rec fold_right op lst init = match lst with
  | [] -> init
  | h::t -> op h (fold_right op t init)

let summate lst = List.fold_right (+) lst 0
let concatenate lst = List.fold_right (^) lst ""
```

* Actually writing out ```summate``` as a recursive function isn't going to save many lines, but it gets the code on a "higher level" so is often good practice.
* It is similar in spirit to dot product in linear algebra: define things in terms of higher-level math operations, don't re-define from scratch.

##### Fold left

* Folding left is the same idea but the "zero" is on the left:

```ocaml
List.fold_left ( * ) 1 [2; 5; 7] (* note the zero and the list swapped in arg list vs folds_right *)
```

is ```(((1)*2)*5)*7```, i.e. ```70```.

```ocaml
let rec fold_left op accum lst = match lst with
  | [] -> accum
  | h::t -> fold_left op (op accum h) t

let summate lst = List.fold_left (+) 0 lst
let concatenate lst = List.fold_left (^) "" lst
```

 * Observe that since addition and concatenation are both associative that fold left and right give the same result in the above. 
 * But, use ```List.fold_left``` by default, it is more efficient
 * Why is it more efficient?  Observe all recursive calls don't need to return really, at the rec-call point the function is done
 * Such recursion can be turned into a loop by an optimizing compiler, so-called *tail-call elimination*.
 
##### length, map, reverse, and filter can be coded just with a fold!

Here are some pleasant examples mostly from the Cornell notes illustrating the power of fold.

```ocaml
let length l = List.fold_left (fun a _ -> a+1) 0 l
let rev l = List.fold_left (fun a x -> x::a) [] l (* e.g. rev [1;2;3] = (3::(2::(1::[]))) *)
let map f l = List.fold_right (fun x a -> (f x)::a) l []
let map_rev f l = List.fold_left (fun a x -> (f x)::a)  [] l
let filter f l = List.fold_right (fun x a -> if f x then x::a else a) l []
```


##### Fold on trees

* What is fold on lists doing?  Visit all the elements applying an accumulating operator
* The idea of Tree fold is to do the same thing on a tree structure
* Here is a simple example derived from the Cornell notes.

```ocaml
type 'a btree = 
| Leaf 
| Node of 'a * 'a btree * 'a btree
```

```ocaml
let rec treefold init op = function
  | Leaf -> init
  | Node (v,l,r) -> op v (treefold init op l) (treefold init op r)
```


```ocaml
let size t = treefold 0 (fun _ l r -> 1 + l + r) t
let depth t = treefold 0 (fun _ l r -> 1 + max l r) t
let preorder t = treefold [] (fun x l r -> [x] @ l @ r) t
let tex = Node (4, Node (5, Node (6, Leaf, Leaf), Leaf), Node (4, Leaf, Leaf))
size tex
```

##### Catamorphism

* This is a fancy word for a fold over any data type.
* Well it is actually a little cooler than that, there is some elegant math behind it which we are skipping.
* The general principle for any datatype is to accumulate results from the "rest" of the data type tree and "compose" with the current node.

Treefold re-cast slightly more generically

```ocaml
let rec generic_treefold (i,f) = function
  | Leaf -> i
  | Node (v,l,r) -> f v (generic_treefold (i,f) l) (generic_treefold (i,f) r)

let size t = generic_treefold (0, (fun _ l r -> 1 + l + r)) t
```

* In general, the tuple ```(i,f)``` can generalize to composers over all the cases of the datatype.

```ocaml
type expr = 
| Var of string
| Func of string * expr
| Appl of expr * expr
```

```ocaml
let rec exprfold (va,fu,ap) = function
  | Var(s) -> va s
  | Func(v,e) -> fu v (exprfold (va,fu,ap) e)
  | Appl(e,e') -> ap (exprfold (va,fu,ap) e) (exprfold (va,fu,ap) e')

let allvaroccs e = exprfold ((fun s -> [s]),(fun v fo -> v :: fo), (fun fo1 fo2 -> fo1 @ fo2)) e

let testexpr = Appl(Func("x",Var "x"),Var "y")

allvaroccs testexpr
```

* It is not worth getting *too* excited by this stuff as many functions on data structures are not just a simple fold.
* Example: determining if an expression in the above language is closed is impossible with just ```exprfold```

####  OCaml functors


 * A "function" from structures to structures
 * Allows a module to be parameterized and so instantiated in multiple ways
     - think of it as the ability to "plug in" a code module
 * General functors are found only in a few languages

Here is a kind of struct that we can take as a parameter

```ocaml
type comparison = LessThan | EqualTo | GreaterThan 
 
module type ORDERED_TYPE =
  sig
    type t
    val compare: t -> t -> comparison
  end;;
```

Here is a functor version of a set, you feed in a struct with the set element ordering defined on it

```ocaml
module FSetFunctor =
  functor (Elt: ORDERED_TYPE) ->
  struct
    type element = Elt.t (* import the type of elements from the structure *)
    type set = element list

    let empty = []

    let rec add x s =
      match s with
        [] -> [x]
      | hd::tl ->
          match Elt.compare x hd with
            EqualTo   -> s
          | LessThan    -> x :: s
          | GreaterThan -> hd :: add x tl

    let rec contains x s =
      match s with
        [] -> false
      | hd::tl ->
          match Elt.compare x hd with
            EqualTo   -> true
          | LessThan    -> false
          | GreaterThan -> contains x tl
  end;;
```

Here is a concrete ordering we can feed in, one over ints

```ocaml
module OrderedInt =
  struct
    type t = int
    let compare x y =
      if x = y then
    EqualTo
      else
    if x < y then
      LessThan
    else
      GreaterThan
  end;;
```

Here is how we feed it in, instantiating the functor to give a structure

```ocaml
module OrderedIntSet = FSetFunctor(OrderedInt);
```

Example of using the resulting module

```ocaml
let myOrderedIntSet = OrderedIntSet.add 5 OrderedIntSet.empty;;
OrderedIntSet.contains 3 myOrderedIntSet;;
```

We can do the same thing for a string comparison

```ocaml
module OrderedString =
struct
  type t = string
  let compare x y =
    if x = y then EqualTo
    else if x < y then LessThan
    else GreaterThan
end;;

module OrderedStringSet = FSetFunctor(OrderedString);; (* a DIFFERENT instantiation of same *)

let myOrderedStringSet = OrderedStringSet.add "abc" OrderedStringSet.empty;;
```

Functors also have signatures; there can also be type abstraction in a functor signature

```ocaml
module type SETFUNCTOR = (* below is the syntax for a signature of a functor *)
    functor (Elt: ORDERED_TYPE) ->
  sig
    type element = Elt.t      (* concrete *)
    type set                  (* abstract *)
    val empty : set
    val add : element -> set -> set
    val contains : element -> set -> bool
  end;;

module AbstractSet = (FSetFunctor : SETFUNCTOR);; (* slap that sig on a functor *)
module AbstractIntSet = AbstractSet(OrderedInt);;

AbstractIntSet.add 5 AbstractIntSet.empty;;
```

 *  ```Stdlib.Set``` uses functors, let us review it -- [manpage](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.html)

 * [Real World OCaml](https://dev.realworldocaml.org/functors.html) has more involved examples
 
 
 *  ```fbdk/src/application.ml``` in FbDK is yet another example; e.g. ```Fb/fb.ml``` uses the functor.
 
##### First-class modules

Idea: Lift modules to be true first-class data; supported in more recent OCaml versions.

 * See [the manual](http://caml.inria.fr/pub/docs/manual-ocaml/manual028.html) for documentation
 * Allows modules to be constructed and composed based on run-time data
 * Allows modules to be placed in data structures, e.g. a list of Set's.
 * Allows functors to be written which are ordinary functions (not as powerful as actual functors though)
 * [Example in OCaml manual](http://caml.inria.fr/pub/docs/manual-ocaml/manual028.html) shows how to build a Set-making function
 * See [Real-World OCaml](https://dev.realworldocaml.org/first-class-modules.html) for more info on first-class modules, the manual is pretty minimal

##### Background: Basic Type Theory

Before getting into functors and GADTs, review forall, exists, and higher kinded (types as values) 
types.

 * Polymorphic types inferred in OCaml are forall types
 * Parameters on type definitions are defining type-valued functions (which are not OCaml functions -- "not first-class")
 * Hidden / abstract types in module signatures are exists types: holds for some type
 * New features of OCaml covered here have other fancy types; each one is either a forall, exists, or type-as-value usage.
 
  
#### GADTs in OCaml

Lists manually using existing OCaml types

```ocaml
type 'a oldlist = Nil | Cons of 'a * 'a oldlist
```

Think of this as a type-valued function: given a type ```'a``` we get a type out: it is "really" 

```ocaml
type oldlist = typefun 'a -> (Nil | Cons of oldlist('a * 'a))
```

Equivalent way to do list type using new generic type syntax (but using it to do old thing)

```ocaml
type _ newlist = Nil : 'a newlist
| Cons : 'a * 'a newlist -> 'a newlist
```
 (the ```_``` here could also be ```'a```, you probably want to view it as ```'a``` in fact)


Observe how this is writing out the constructors with what their types are, viewing constructors as functions (they still are not functions in OCaml, but they are closer to being so)

Lets now show some added power of this new notation (toy example).

```ocaml
type _ newlist = Nil : 'a newlist
| IntNil : int newlist
| Cons : 'a * 'a newlist -> 'a newlist
```

We now have a new IntNil empty list, which is only consable on to integers:

```ocaml
# Cons(5,IntNil);;
- : int newlist = Cons (5, IntNil)
```

Note however that "IntNil" is just a name, the int newlist type could have been something else:

```ocaml
type _ newlist = Nil : 'a newlist
| IntNil : string newlist (* yes this is perfectly legal *)
| Cons : 'a * 'a newlist -> 'a newlist
```

```ocaml
# Cons(5,IntNil);;
Error: This expression has type string newlist
       but an expression was expected of type int newlist
       Type string is not compatible with type int
```

In other words, the "IntNil" is just a name.  It is *axiomatically* (by fiat) making certain constructors have certain newlist types.  Think of it as something like making an operational semantics rule set.  Here is an arbitrary example to illustrate.

```ocaml
type _ dough = 
  Coin : 'a dough (* base case (axiom) *)
| Add : 'a dough -> 'a dough (* rule: have dough, can make more same type of dough *)
| Bump : int dough -> float dough (* rule: can turn int dough to float dough *)
| Morph : 'a dough -> 'b dough (* rule: can turn any kind of dough to any other *)
| Intbux : int -> int dough (* rule: with a number some int dough can be made *)
| Intcomb : int * int dough -> int dough (* rule: use int and int dough to make int dough *)
```

You can still do whatever combinations will type, e.g.
```ocaml
# Bump(Add(Coin));;
- : float dough = Bump (Add Coin)
# Bump(Morph(Bump(Add(Intcomb(4,Coin)))));;
- : float dough = Bump (Morph (Bump (Add (Intcomb (4, Coin)))))
```

This particular type has no purpose: it is like making some random (well-formed) operational semantics rules, they are not likely to be useful.  As with an operational semantics rule set, with the right set of rules you can do interesting stuff.

Here is an example from the manual showing how some useful work can be done.

```ocaml
 type _ typ =
   | Int : int typ      (* axiom: make an int type *)
   | String : string typ (* axiom: can make a string typ *)
   | Pair : 'a typ * 'b typ -> ('a * 'b) typ (* rule: from any two typ's can make their product *)
```

Observe first that this GADT ```typ``` is not the same as the "old OCaml" equivalent,

```ocaml
type atype = OInt | OString | OPair of atype * atype
```
-- the old types are much simpler, there is no type parameter; compare:

```ocaml
# OPair(OInt,OString);; (* old *)
- : atype = OPair (OInt, OString)           (* no parameter on atype *)
# Pair(Int,String);;    (* new *)
- : (int * string) typ = Pair (Int, String) (* observe parameter on typ *)
```

The interesting and useful bit about this seemingly-useless difference is this type parameter on ```typ``` can be used to force typings to go a certain way:

```ocaml
 let rec to_string: type t. t typ -> t -> string =
   fun tv x ->
   match tv with
   | Int -> string_of_int x
   | String -> Printf.sprintf "%S" x
   | Pair(t1,t2) ->
       let (x1, x2) = x in
       Printf.sprintf "(%s,%s)" (to_string t1 x1) (to_string t2 x2)
```

Here ```typ``` is giving a run-time name, ```Int``` which in the type of ```to_string``` adds an implicit constraint that the second argument *must be an integer* (cool, eh?).  This allows run-time type dispatch to be encoded; this trick is in fact how the OCaml libraries implement printf.

```
# to_string Int 5;;
- : string = "5"
# to_string Int "oops";;
Error: This expression has type string but an expression was expected of type
         int
```

Notice also the special new ```type t.``` syntax for the type argument (which must be declared, inference is stupid for GADT types).  This shows what GADTs are useful for: the type ```t``` can be viewed as an (implicit) parameter to this function.  If the type were the more standard OCaml ```'a typ -> 'a -> string``` this would require ```'a``` to be *generic* and since the body de facto cases on that type this standard typing would not work.

Notice also that any cases from a GADT which do not type-check can be elided since the type system knows that case will never be matched -- the following typechecks:

```ocaml
let int_to_string: int typ -> int -> string =
   fun t x ->
   match t with
   | Int -> string_of_int x
```


Here is another simple example.

```ocaml
type _ wait =  Wait : 'arg * ('arg -> 'result) -> 'result wait
let foo (x : bool) = if x then 0 else 1
let suspended_value : int wait = Wait(true, foo)
let other_suspended_value : int wait = Wait("5", int_of_string)
let run_it w = match w with Wait(arg,func) -> func arg
let doit = run_it suspended_value
```

Observe that the type ```'arg``` is not an exposed parameter in the wait type like ```'result``` is, it is an *existential* type, there is *some* type that works there but it is from the ```wait``` type.

A practical library example: [Gmap](https://github.com/hannesm/gmap)

