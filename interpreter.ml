open Core_kernel

module rec Spine : sig
  type t = Expr.t list [@@deriving sexp]
end = struct
  type t = Expr.t list [@@deriving sexp]
end

and Universal : sig
  type t = Var of string [@@deriving sexp]
end = struct
  type t = Var of string [@@deriving sexp]
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
  type t =
    | Empty
    | Cons_univ of Universal.t * Sort.t
    | Cons_var of string * Type.t * Principality.t
  [@@deriving sexp]
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
                        ( Fun (Var (Universal.Var "alpah"), Sum (One, One))
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
