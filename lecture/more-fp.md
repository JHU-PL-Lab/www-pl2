
<link rel="stylesheet" href="/pl2/css/friendly.css" />
<link rel="stylesheet" href="/pl2/css/lecture-style.css" />
<meta charset="utf-8" />


### More Functional Programming in OCaml

 * There are "nearly infinitely many" OCaml features we skipped in PL I and we are not going to cover all of them 
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
# 5 |> (fun x -> x - 20);; (* it is really just new syntax for function application *)
- : int = -15
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

is ```2*(5*(7*(1)))```, i.e. ```70```.  The "right" refers to the zero (```1``` here) being on the right, not the left (it doesn't matter in the case of multiplication as it is commutative and associative).

Here is roughly how ```List.fold_right``` is implemented:

```ocaml
let rec fold_right op lst init = match lst with
  | [] -> init
  | h::t -> op h (fold_right op t init)

let summate lst = List.fold_right (+) lst 0
let concatenate lst = List.fold_right (^) lst ""
```

* Actually writing out ```summate``` as a recursive function isn't going to be much longer, but it gets the code on a "higher level" so is often good practice.
* Analogy with e.g. dot product in linear algebra: define things in terms of higher-level math operations, don't re-define from scratch.

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

 * Observe that since addition and concatenation are both commutative and associative that fold left and right give the same result in the above. 
 * But, use ```List.fold_left``` by default, it is more efficient
 * Why is it more efficient?  Observe all recursive calls don't need to return really, at the rec-call point the function is done
 * Such recursion can be turned into a loop by an optimizing compiler, so-called *tail-call elimination*.

Some non-assoc/commut operator:
```ocaml
List.fold_right (-) [2; 5; 7] 0     (* 2-(5-(7-0))) = 4; not very useful *)
List.fold_left (-) 0 [2; 5; 7]      (* ((0-2)-5)-7 = -14 *)
```


##### length, map, reverse, and filter can be coded just with a fold!

Here are some pleasant examples mostly from the Cornell notes illustrating the power of fold.

```ocaml
let length l = List.fold_left (fun a _ -> a+1) 0 l
let rev l = List.fold_left (fun a x -> x::a) [] l (* e.g. rev [1;2;3] = (3::(2::(1::[]))) *)
let map f l = List.fold_right (fun x a -> (f x)::a) l []
let map_rev f l = List.fold_left (fun a x -> (f x)::a)  [] l (* to contrast left and right fold *)
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
* Well it is actually cooler than that, there is some elegant math behind it which can't easily be coded in in OCaml.
* The essential idea we can express in some OCaml pseudo-code; observe a recursve datatype is a fixed point of a type function:

```
type function btreefun('mytype)('a) = Leaf | Node of 'a * 'mytype('a) * 'mytype('a) (* takes self-type as arg *)
type 'a btree = btreefun btreefun 'a  (* type fixed point via self-type passing *)
```
By feeding in the "accumulator type" for ```'mytype``` here we can obtain the type of one layer of the fold

```
type summer = btreefun int int (* assume we are summating over an integer-node tree *)
```

Then we can write a function over this type which does that one layer:

```ocaml
let (treesummer : summer -> int) = function
  | Leaf -> 0
  | Node (v,laccum,raccum) -> v + laccum + raccum

```

And the folder can be generic over any such type -- it takes this non-recursive code to perform the fold operation.


* See [The Haskell Wiki](https://wiki.haskell.org/Catamorphisms) for more information

####  OCaml functors


 * A "function" from modules to modules
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
  end
```

Here is a functor version of a set, you feed in a struct with the set element ordering defined on it

```ocaml
module FSetFunctor =
  functor (Elt: ORDERED_TYPE) ->
  struct
    type element = Elt.t (* import the type of elements from the module *)
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
  end
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
  end
```

Here is how we feed it in, instantiating the functor to give a module

```ocaml
module OrderedIntSet = FSetFunctor(OrderedInt)
```

Example of using the resulting module

```ocaml
let myOrderedIntSet = OrderedIntSet.add 5 OrderedIntSet.empty
OrderedIntSet.contains 3 myOrderedIntSet
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
end

module OrderedStringSet = FSetFunctor(OrderedString) (* a DIFFERENT instantiation of same *)

let myOrderedStringSet = OrderedStringSet.add "abc" OrderedStringSet.empty
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
  end

module AbstractSet = (FSetFunctor : SETFUNCTOR) (* slap that sig on a functor *)
module AbstractIntSet = AbstractSet(OrderedInt)

AbstractIntSet.add 5 AbstractIntSet.empty
```

* Observe the internal structure is hidden since the type list was hidden
* This is called an *existential type*:  a type exists, we just don't know what it is.


##### Functors in the Standard Library

```Stdlib.Set``` uses functors similar to how our simplistic set works, let us review it -- [manpage](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.html)



Usage example

```ocaml
module Ints =
       struct
         type t = int
         let compare x0 x1 =
           Stdlib.compare x0 x1
       end

     module IntsSet = Set.Make(Ints) (* Set.Make is a functor *)

     let m = IntsSet.(empty |> add 3 |> add 22 |> add 76)
```


 * [Real World OCaml](https://dev.realworldocaml.org/functors.html) has more involved examples
 
 
 *  ```fbdk/src/application.ml``` in FbDK is yet another example; e.g. ```Fb/fb.ml``` uses a functor.
 
##### First-class modules

Idea: Lift modules to be true first-class data; supported in more recent OCaml versions.

 * See [the manual](http://caml.inria.fr/pub/docs/manual-ocaml/manual028.html) for documentation
 * Allows modules to be constructed and composed based on run-time data
 * Allows modules to be placed in data structures, e.g. a list of Set's.
 * Allows functors to be written which are ordinary functions (not as powerful as actual functors though)
 * [Example in OCaml manual](http://caml.inria.fr/pub/docs/manual-ocaml/manual028.html) shows how to build a Set-making function
 * See [Real-World OCaml](https://dev.realworldocaml.org/first-class-modules.html) for more info on first-class modules, the manual is pretty minimal

  
#### GADTs in OCaml

Generalized Abstract Data Types (GADT's) allow several more flexible uses of OCaml data types.  See [The Manual](https://caml.inria.fr/pub/docs/manual-ocaml/manual033.html) for the details.

##### New data type syntax for OCaml

Review: roll-your-own lists using existing OCaml types

```ocaml
type 'a oldlist = Nil | Cons of 'a * 'a oldlist
```

Equivalent way to do list type using new generic type syntax (but using it to do old thing)

```ocaml
type _ newlist = Nil : 'a newlist | Cons : 'a * 'a newlist -> 'a newlist
```

* Observe how this is writing out the constructors with what their types are, viewing constructors as functions (they still are not functions in OCaml, but they are closer to being so)
* This type will work at least as well as the original ```oldlist``` type.

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

* The above type has no purpose: it is like making some random (well-formed) operational semantics rules, they are not likely to be useful.  
* As with an operational semantics rule set, with the right set of rules you can do interesting stuff.


##### Explicit forall types

Along with the new syntax for type declarations is new syntax for polymorphic (for all / generic) types.

```ocaml
let id: type t. t -> t = fun x -> x;;
val id : 't -> 't = <fun>
```

* This type syntax is better in that it makes it more explicit that the type is "for all types t, t to t".
* But, a big downside of all this is you need to start declaring types for OCaml functions as we will see below.
* These types are a form of the [locally abstract types](https://caml.inria.fr/pub/docs/manual-ocaml/manual027.html) in OCaml.

##### Useful work from the new GADT's in OCaml


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

The interesting and useful bit about this seemingly-useless difference is this type parameter on ```typ``` can be used to type otherwise untypeable code:

```ocaml
 let rec to_string: type t. t typ -> t -> string = (* notice need to declare forall types! *)
   fun tv x ->
   match tv with
   | Int -> string_of_int x
   | String -> Printf.sprintf "%S" x
   | Pair(t1,t2) ->
       let (x1, x2) = x in
       Printf.sprintf "(%s,%s)" (to_string t1 x1) (to_string t2 x2);;
val to_string : 't typ -> 't -> string = <fun>
```

 * Here ```typ``` is giving a run-time name, ```Int``` which in the type of ```to_string``` adds an implicit constraint that the second argument *must be an integer* (cool, eh?).   
 * Notice that the whole ```match``` above cannot be typed at a fixed type for ```tv``` like ```int typ```
 * The type used in each branch clause _depends_ on which constuctor we are using
 * this is sometimes called a _weak dependent type_ but it is really a _path-sensitive type_: different paths through the function can have different types.
 * Additionally, notice that the recursive call for pairs is at a type different than the original parameter -- this is called a _polymorphic recursive type_ and is not supported in normal OCaml types.
 * This allows run-time type dispatch to be encoded
 * This trick is in fact how the OCaml libraries implement printf.

```
# to_string Int 5;; (* Int has type int typ and 5 is of type int -- a match *)
- : string = "5"
# to_string Int "oops";; (* Int has type int typ and "oops" is of type string -- mismatch *)
Error: This expression has type string but an expression was expected of type
         int
```

If the type of ```to_string``` were the more standard OCaml ```'a typ -> 'a -> string``` this path-sensitivity would not be allowed:

```ocaml
 let rec too_string:  'a typ -> 'a -> string =
   fun tv x ->
   match tv with
   | Int -> string_of_int x
   | String -> Printf.sprintf "%S" x
   | Pair(t1,t2) ->
       let (x1, x2) = x in
       Printf.sprintf "(%s,%s)" (to_string t1 x1) (to_string t2 x2);;
Error: This pattern matches values of type string typ
       but a pattern was expected which matches values of type int typ
       Type string is not compatible with type int 
```

* From the first match case ```'a``` must be an integer, but that conflicts with the second case.  GADTs allow each branch to have it's own type.
* Notice also that any cases from a GADT which do not type-check can be elided since the type system knows that case will never be matched
* For example the following typechecks in spite of missing cases:

```ocaml
let int_to_string: int typ -> int -> string =
   fun t x ->
   match t with
   | Int -> string_of_int x
```


Here is another simple example of suspended application.

```ocaml
type _ wait =  Wait : 'arg * ('arg -> 'result) -> 'result wait
let foo (x : bool) = if x then 0 else 1
let suspended_value : int wait = Wait(true, foo)
let other_suspended_value : int wait = Wait("5", int_of_string)
let run_it w = match w with Wait(arg,func) -> func arg
let doit = run_it suspended_value
```

* Observe that the type ```'arg``` is not an exposed parameter in the wait type like ```'result``` is
* It is an *existential* type, there is *some* type that works there but it is hidden in the ```wait``` type.
* Such types cannot be formed in the "old" OCaml type syntax
* The above code in fact would work with a regular OCaml ```Wait``` type but we could not make a list of the two ```int wait``` values above as their ```'arg``` types differ

```ocaml
type ('a, 'b) badwait =  BadWait of  'a * ('a -> 'b);; (* notice how BOTH 'a and 'b exposed here *)
```

A practical library example: [Gmap](https://github.com/hannesm/gmap)

