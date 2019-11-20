(*
  Assignment 6: Symbolic Interpreter
  JHU PL2, 2019 Fall, Shiwei Weng

  Z3 is at https://github.com/Z3Prover/z3 .
  The document of Z3 OCaml binding is http://pl.cs.jhu.edu/z3/index_modules.html .

*)

module Test_z3 = struct
  open Z3
  open Arithmetic
  
  (* 
    module Z3 contains Arithmetic and Boolean modules.
    Arithmetic contains Float and Integer modules.

    Noting Z3 has types for Symbol and Expression.
    We make Symbol from string, and make Expression from Symbol.
    See ex2 for details.
  *)

  let solving solver symbol_pairs =
    (* check has three possible return values: SATISFIABLE, UNSATISFIABLE, UNKNOWN *)
    match Solver.check solver [] with
    | Solver.SATISFIABLE -> (
      (* for SATISFIABLE case, we query the model from the solver *)
      match Solver.get_model solver with
      | None ->
        Printf.sprintf "SAT with no model"
      | Some model ->
        (* retrieve the value for each symbol in the model *)
        let string_of_sym_pair model (v_sym, v_exp) = 
          match Model.eval model v_exp true with
          | Some expr -> (
            Printf.sprintf "%s=%s"
              (Symbol.to_string v_sym)
              (Expr.to_string expr)
          )
          | None -> "..."
          in
        (Printf.sprintf "SAT %s"
          (List.fold_left (fun acc sym_pair -> acc ^ " " ^ (string_of_sym_pair model sym_pair)) "" symbol_pairs))
        (* ^ (Model.to_string model) *)
    )        
    | Solver.UNSATISFIABLE ->
      Printf.sprintf "UNSAT"
    | Solver.UNKNOWN ->
      Printf.sprintf "Unknown result in solve.check: %s" (Solver.get_reason_unknown solver)

  (* checking 1 = 1 *)
  let ex1 () =
    let ctx = Z3.mk_context [] in
    let solver = Solver.mk_solver ctx None in
    let formula1 = 
      (* i1 is the left expression for 1 *)
      let i1 = Integer.mk_numeral_i ctx 1 in
      (* i2 is the right expression for 1 *)
      let i2 = Integer.mk_numeral_i ctx 1 in
      (* eq12 is the equality i1 = i2 a.k.a. 1 = 1*)
      let eq12 = Boolean.mk_eq ctx i1 i2 in
      eq12 in
    Solver.add solver [formula1];
    print_endline (solving solver [])

  (* checking (x + 1 > 10) and (b && true) 
  Noting formula-level "and" is merely a list
  while logic "&&" is encoded as `Boolean.mk_and`
  *)
  let ex2 () =
    let ctx = Z3.mk_context [] in
    let solver = Solver.mk_solver ctx None in
    let x_sym = Symbol.mk_string ctx "x" in
    let x_exp = Integer.mk_const ctx x_sym in
    let b_sym = Symbol.mk_string ctx "b" in
    let b_exp = Boolean.mk_const ctx b_sym in
    (* x+1>10 *)
    let formula1 = 
      let i1 = Integer.mk_numeral_i ctx 1 in
      let s1 = Arithmetic.mk_add ctx [x_exp; i1] in
      let i2 = Integer.mk_numeral_i ctx 10 in
      let gt1 = Arithmetic.mk_gt ctx s1 i2 in
      gt1 in
    (* b && true *)
    let formula2 = 
      let z3true = Boolean.mk_true ctx in
      let and1 = Boolean.mk_and ctx [b_exp; z3true] in
      and1 in
    Solver.add solver [formula1; formula2];
    print_endline (solving solver [(x_sym, x_exp); (b_sym, b_exp)])
end
;;
Test_z3.ex1 ();;
Test_z3.ex2 ();;
assert false;;

open Fbast

let eval e = e
