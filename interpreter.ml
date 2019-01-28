open Core_kernel

module rec Spine : sig
  type t = Expr.t list [@@deriving sexp]
end = struct
  type t = Expr.t list [@@deriving sexp]
end

and Existential : sig
  type t = Var of string [@@deriving sexp]

  include Comparable.S with type t := t
end = struct
  module T = struct
    type t = Var of string [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

and Universal : sig
  type t = Var of string [@@deriving sexp]

  include Comparable.S with type t := t
end = struct
  module T = struct
    type t = Var of string [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

and Var : sig
  type t = U of Universal.t | E of Existential.t [@@deriving sexp]

  include Comparable.S with type t := t
end = struct
  module T = struct
    type t = U of Universal.t | E of Existential.t [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

and Sort : sig
  type t = Star | Nat [@@deriving sexp]
end = struct
  type t = Star | Nat [@@deriving sexp]
end

and Term_or_mono : sig
  type t =
    | Zero
    | Succ of t
    | One
    | Var of Universal.t
    | EVar of Existential.t
    | Fun of t * t
    | Sum of t * t
    | Prod of t * t
  [@@deriving sexp]
end = struct
  type t =
    | Zero
    | Succ of t
    | One
    | Var of Universal.t
    | EVar of Existential.t
    | Fun of t * t
    | Sum of t * t
    | Prod of t * t
  [@@deriving sexp]
end

and Prop : sig
  type t = Equation of Term_or_mono.t * Term_or_mono.t [@@deriving sexp]
end = struct
  type t = Equation of Term_or_mono.t * Term_or_mono.t [@@deriving sexp]
end

and Type : sig
  type t =
    | One
    | Fun of t * t
    | Sum of t * t
    | Prod of t * t
    | Var of Universal.t
    | EVar of Existential.t
    | Forall of {alpha: Universal.t; k: Sort.t; a: t}
    | Exists of {alpha: Universal.t; k: Sort.t; a: t}
    | Implies of Prop.t * t
    | With of t * Prop.t
    | Vec of Term_or_mono.t * t
  [@@deriving sexp]
end = struct
  type t =
    | One
    | Fun of t * t
    | Sum of t * t
    | Prod of t * t
    | Var of Universal.t
    | EVar of Existential.t
    | Forall of {alpha: Universal.t; k: Sort.t; a: t}
    | Exists of {alpha: Universal.t; k: Sort.t; a: t}
    | Implies of Prop.t * t
    | With of t * Prop.t
    | Vec of Term_or_mono.t * t
  [@@deriving sexp]
end

and Value : sig
  type t =
    | Var of string
    | Unit
    | Lam of {x: string; e: Expr.t}
    | Fix of {x: string; v: t}
    | Annot of {v: t; a: Type.t}
    | Tuple of t * t
    | In1 of t
    | In2 of t
    | Nil
    | Cons of t * t
  [@@deriving sexp]
end = struct
  type t =
    | Var of string
    | Unit
    | Lam of {x: string; e: Expr.t}
    | Fix of {x: string; v: t}
    | Annot of {v: t; a: Type.t}
    | Tuple of t * t
    | In1 of t
    | In2 of t
    | Nil
    | Cons of t * t
  [@@deriving sexp]
end

and Pattern : sig
  type t =
    | Var of string
    | Tuple of t * t
    | In1 of t
    | In2 of t
    | Wild
    | Nil
    | Cons of t * t
  [@@deriving sexp]
end = struct
  type t =
    | Var of string
    | Tuple of t * t
    | In1 of t
    | In2 of t
    | Wild
    | Nil
    | Cons of t * t
  [@@deriving sexp]
end

and Branch : sig
  type t = {rho: Pattern.t list; e: Expr.t} [@@deriving sexp]
end = struct
  type t = {rho: Pattern.t list; e: Expr.t} [@@deriving sexp]
end

and Expr : sig
  type t =
    | Val of Value.t
    | Let of {x: string; e1: t; e2: t}
    | Spine of {e: t; s: Spine.t}
    | Annot of {e: t; a: Type.t}
    | Tuple of t * t
    | Case of {e: t; pi: Branch.t list}
    | In1 of t
    | In2 of t
    | Nil
    | Cons of t * t
  [@@deriving sexp]
end = struct
  type t =
    | Val of Value.t
    | Let of {x: string; e1: t; e2: t}
    | Spine of {e: t; s: Spine.t}
    | Annot of {e: t; a: Type.t}
    | Tuple of t * t
    | Case of {e: t; pi: Branch.t list}
    | In1 of t
    | In2 of t
    | Nil
    | Cons of t * t
  [@@deriving sexp]
end

module Principality = struct
  type t = Bang | Not_bang [@@deriving sexp]
end

module Context = struct
  module Universal_decl = struct
    type t = Universal.t * Sort.t [@@deriving sexp]
  end

  module Expr_typing = struct
    type t = string * Type.t * Principality.t [@@deriving sexp]
  end

  module Unsolved_decl = struct
    type t = Existential.t * Sort.t [@@deriving sexp]
  end

  module Solved_decl = struct
    type t = Existential.t * Sort.t * Term_or_mono.t [@@deriving sexp]
  end

  module Equation = struct
    type t = Universal.t * Term_or_mono.t [@@deriving sexp]
  end

  module Entry = struct
    type t =
      | Univ of Universal_decl.t
      | Expr of Expr_typing.t
      | Unsolved of Unsolved_decl.t
      | Solved of Solved_decl.t
      | Equation of Equation.t
      | Marker of Var.t
    [@@deriving sexp]

    let fold t ~init ~f_univ ~f_expr ~f_unsolved ~f_solved ~f_equation
        ~f_marker =
      match t with
      | Univ decl -> f_univ init decl
      | Expr expr -> f_expr init expr
      | Unsolved decl -> f_unsolved init decl
      | Solved s -> f_solved init s
      | Equation e -> f_equation init e
      | Marker v -> f_marker init v
  end

  type t = Entry.t list [@@deriving sexp]

  let rec fold t ~init ~f_univ ~f_expr ~f_unsolved ~f_solved ~f_equation
      ~f_marker =
    List.fold t ~init ~f:(fun acc entry ->
        Entry.fold entry ~init:acc ~f_univ ~f_expr ~f_unsolved ~f_solved
          ~f_equation ~f_marker )

  let rec map t ~f_univ ~f_expr ~f_unsolved ~f_solved ~f_equation ~f_marker =
    List.map t ~f:(fun entry ->
        Entry.fold ~init:() ~f_univ:(Fn.const f_univ) ~f_expr:(Fn.const f_expr)
          ~f_unsolved:(Fn.const f_unsolved) ~f_solved:(Fn.const f_solved)
          ~f_equation:(Fn.const f_equation) ~f_marker:(Fn.const f_marker) )

  let update_at (ctx : t) ?(f_univ=(fun v -> Entry.Univ v)) ?(f_expr=(fun e -> Entry.Expr e)) ?(f_unsolved=(fun decl -> Entry.Unsolved decl)) ?(f_solved=(fun decl -> Entry.Solved decl)) ?(f_equation=(fun eqn -> Entry.Equation eqn))
      ?(f_marker=(fun v -> Entry.Marker v)) str =
    map ctx
      ~f_univ:(fun v ->
          match v with Universal.Var s, _ ->
            if s = str then f_univ v else Entry.Univ v )
      ~f_expr:(fun e ->
          match e with s, _, _ -> if s = str then f_expr e else Entry.Expr e )
      ~f_unsolved:(fun decl ->
          match decl with Existential.Var s, _ ->
            if s = str then f_unsolved decl else Entry.Unsolved decl )
      ~f_solved:(fun decl ->
          match decl with Existential.Var s, _, _ ->
            if s = str then f_solved decl else Entry.Solved decl )
      ~f_equation:(fun eq ->
          match eq with Universal.Var s, _ ->
            if s = str then f_equation eq else Entry.Equation eq )
      ~f_marker:(fun v ->
          match v with
          | Var.U (Universal.Var s) | Var.E (Existential.Var s) ->
            if s = str then f_marker v else Entry.Marker v )

  let skip _ = None

  let find_at (ctx : t) ?(f_univ = skip) ?(f_expr = skip) ?(f_unsolved = skip)
      ?(f_solved = skip) ?(f_equation = skip) ?(f_marker = skip) str =
    let wrap f acc x = match acc with None -> f x | Some v -> Some v in
    fold ctx ~init:None ~f_univ:(wrap f_univ) ~f_expr:(wrap f_expr)
      ~f_unsolved:(wrap f_unsolved) ~f_solved:(wrap f_solved)
      ~f_equation:(wrap f_equation) ~f_marker:(wrap f_marker)

  (* Figure 12: TODO: Apply RtL *)
  let rec handle_uvar (ctx : t) (a : string) =
    Option.map (find_at ctx ~f_equation:(fun (Universal.Var _, t) -> Some t) a)
      ~f:(fun tau -> apply_term_or_mono ctx tau)
  and handle_evar (ctx : t) (a_hat : string) =
    Option.map (
      find_at ctx ~f_solved:(fun (Existential.Var _, _, t) -> Some t) a_hat)
      ~f:(fun tau -> apply_term_or_mono ctx tau)
  and apply_prop (ctx : t) (Prop.Equation (t, t') : Prop.t) =
    Prop.Equation (apply_term_or_mono ctx t, apply_term_or_mono ctx t')
  and apply_term_or_mono (ctx : t) (tau : Term_or_mono.t) =
    match tau with
    | Term_or_mono.Zero -> tau
    | Term_or_mono.Succ _ -> tau
    | Term_or_mono.One -> tau
    | Term_or_mono.Var (Universal.Var a) -> (match handle_uvar ctx a with
        | Some tau' -> tau'
        | None -> tau)
    | Term_or_mono.EVar (Existential.Var a_hat) -> (match handle_evar ctx a_hat with
        | Some tau' -> tau'
        | None -> tau)
    | Term_or_mono.Fun (tau, sigma) ->
      Term_or_mono.Fun
        (apply_term_or_mono ctx tau, apply_term_or_mono ctx sigma)
    | Term_or_mono.Sum (tau, sigma) ->
      Term_or_mono.Sum
        (apply_term_or_mono ctx tau, apply_term_or_mono ctx sigma)
    | Term_or_mono.Prod (tau, sigma) ->
      Term_or_mono.Prod
        (apply_term_or_mono ctx tau, apply_term_or_mono ctx sigma)
  and apply (ctx : t) (typ : Type.t) =
    match typ with
    | Type.One -> typ
    | Type.Fun (a, b) -> Type.Fun (apply ctx a, apply ctx b)
    | Type.Sum (a, b) -> Type.Sum (apply ctx a, apply ctx b)
    | Type.Prod (a, b) -> Type.Prod (apply ctx a, apply ctx b)
    | Type.Var (Universal.Var a) -> let _ = handle_uvar ctx a in ??
    | Type.EVar (Existential.Var a_hat) -> let _ = handle_evar ctx a_hat in ??
    | Type.Forall {alpha; k; a} ->
      Type.Forall {alpha; k; a= apply ctx a}
    | Type.Exists {alpha; k; a} ->
      Type.Exists {alpha; k; a=apply ctx a}
    | Type.Implies (p, a) ->
      Type.Implies (apply_prop ctx p, apply ctx a)
    | Type.With (a, p) ->
      Type.With (apply ctx a, apply_prop ctx p)
    | Type.Vec (t, a) ->
      Type.Vec (apply_term_or_mono ctx t, apply ctx a)

  (* Figure 15 in POPL paper *)
  (*  module Extension = struct
                     let extends
                     end *)
end

module Polarity = struct
  type t = Plus | Minus | Neutral [@@deriving sexp]
end

let evar s = Expr.Val (Value.Var s)

let example : Value.t =
  Value.Annot
    { v=
        Value.Fix
          { x= "filter"
          ; v=
              Value.Lam
                { x= "p"
                ; e=
                    Expr.Val
                      (Value.Lam
                         { x= "xs"
                         ; e=
                             Expr.Case
                               { e= evar "xs"
                               ; pi=
                                   [ {Branch.rho= [Pattern.Nil]; e= Expr.Nil}
                                   ; { Branch.rho=
                                         [ Pattern.Cons
                                             (Pattern.Var "x", Pattern.Var "xs")
                                         ]
                                     ; e=
                                         Expr.Let
                                           { x= "tl"
                                           ; e1=
                                               Expr.Spine
                                                 { e= evar "filter"
                                                 ; s= [evar "p"; evar "xs"] }
                                           ; e2=
                                               Expr.Case
                                                 { e=
                                                     Expr.Spine
                                                       { e= evar "p"
                                                       ; s= [evar "xs"] }
                                                 ; pi=
                                                     [ { Branch.rho=
                                                           [ Pattern.In1
                                                               Pattern.Wild ]
                                                       ; e= evar "tl" }
                                                     ; { Branch.rho=
                                                           [ Pattern.In2
                                                               Pattern.Wild ]
                                                       ; e=
                                                           Expr.Cons
                                                             ( evar "x"
                                                             , evar "tl" ) } ]
                                                 } } } ] } }) } }
    ; a=
        Type.Forall
          { alpha= Universal.Var "n"
          ; k= Sort.Nat
          ; a=
              Type.Forall
                { alpha= Universal.Var "alpha"
                ; k= Sort.Star
                ; a=
                    Type.(
                      Fun
                        ( Fun (Var (Universal.Var "alpha"), Sum (One, One))
                        , Fun
                            ( Vec
                                ( Term_or_mono.Var (Universal.Var "n")
                                , Var (Universal.Var "alpha") )
                            , Exists
                                { alpha= Universal.Var "k"
                                ; k= Sort.Nat
                                ; a=
                                    Vec
                                      ( Term_or_mono.Var (Universal.Var "k")
                                      , Var (Universal.Var "alpha") ) } ) )) }
          } }

;;
printf !"%{sexp: Value.t}\n%!" example
