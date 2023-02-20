open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)

let add_to_env env c v = (c,v)::env


let rec find env a =
  match env with
  | [] -> raise (UndefinedVar)
  | (b, v)::env_new -> if a = b then v else find env_new a



(* evaluate an arithmetic expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value =
    match e with
    | Number x -> Int_Val x
    | True -> Bool_Val true
    | False -> Bool_Val false
    | Var x -> find env x
    | Plus (x1,x2) -> (match (eval_expr x1 env), (eval_expr x2 env) with Int_Val num1, Int_Val num2 -> let num3 = num1+num2 in Int_Val num3
    | _, _ -> raise (TypeError))
    | Minus (x1,x2) -> (match (eval_expr x1 env), (eval_expr x2 env) with Int_Val num1, Int_Val num2 -> let num3 = num1-num2 in Int_Val num3
    | _, _ -> raise (TypeError))
    | Times (x1,x2) -> (match (eval_expr x1 env), (eval_expr x2 env) with Int_Val num1, Int_Val num2 -> let num3 = num1*num2 in Int_Val num3
    | _, _ -> raise (TypeError))
    | Div (x1,x2) -> (match (eval_expr x1 env), (eval_expr x2 env) with Int_Val num1, Int_Val num2 -> if num2=0 then raise (DivByZeroError) else let num3=num1/num2 in Int_Val num3
    | _, _ -> raise (TypeError))
    | Mod (x1,x2) -> (match (eval_expr x1 env), (eval_expr x2 env) with Int_Val num1, Int_Val num2 -> let num3 = num1 mod num2 in Int_Val num3
    | _, _ -> raise (TypeError))
    | Or (x1,x2) -> (match (eval_expr x1 env), (eval_expr x2 env) with Bool_Val num1, Bool_Val num2 -> let num3 = num1 || num2 in Bool_Val num3
    | _, _ -> raise (TypeError))
    | And (x1,x2) -> (match (eval_expr x1 env), (eval_expr x2 env) with Bool_Val num1, Bool_Val num2 -> let num3 = num1 && num2 in Bool_Val num3
    | _, _ -> raise (TypeError))
    | Not (x) -> (match eval_expr x env with Bool_Val a -> let num1 = not a in Bool_Val num1
    | _ -> raise (TypeError))
    | Lt (x1,x2) -> (match (eval_expr x1 env), (eval_expr x2 env) with Int_Val num1, Int_Val num2 -> if (num1<num2) then Bool_Val true else Bool_Val false
    | _, _ -> raise (TypeError))
    | Leq (x1,x2) -> (match (eval_expr x1 env), (eval_expr x2 env) with Int_Val num1, Int_Val num2 -> if (num1<=num2) then Bool_Val true else Bool_Val false
    | _, _ -> raise (TypeError))
    | Eq (x1,x2) -> (match (eval_expr x1 env), (eval_expr x2 env) with Int_Val num1, Int_Val num2 -> if (num1==num2) then Bool_Val true else Bool_Val false
    | Bool_Val num1, Bool_Val num2 -> if (num1==num2) then Bool_Val true else Bool_Val false
    | _, _ -> raise (TypeError))
    | App (x1,x2) -> (match (eval_expr x1 env), (eval_expr x2 env) with Closure (env1, s, e), Int_Val n -> let env2 = add_to_env env1 s (Int_Val n) in eval_expr e env2
    | Closure (env1, s, e), Bool_Val b -> let env2 = add_to_env env1 s (Bool_Val b) in eval_expr e env2
    | Closure (env1, s, e), Closure (env_1, s_1, e_1) -> let env2 = add_to_env env1 s (Closure (env_1, s_1, e_1)) in eval_expr e env2
    | _, _ -> raise (TypeError))
    | Fun (s,e) -> Closure (env, s, e)

(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
  match c with
 | Skip -> env
 | While (e,c) -> let rec loop e c env = (match (eval_expr e env) with Bool_Val b -> if b then loop e c (eval_command c env) else env
 |_ -> raise (TypeError)) in loop e c env
 | For (e,c) -> (match (eval_expr e env) with Int_Val n -> let rec loop2 n c env = if n=0 then env else loop2 (n-1) c (eval_command c env) in loop2 n c env
 |_ -> raise (TypeError))
 | Cond (e,c1,c2) -> (match (eval_expr e env) with Bool_Val b -> if b then eval_command c1 env else eval_command c2 env
 |_ -> raise (TypeError))
 | Comp (c1,c2) -> let env1 = eval_command c1 env in eval_command c2 env1
 | Assg (s,e) -> (match (find env s), (eval_expr e env) with Int_Val _ , Int_Val _ -> add_to_env env s (eval_expr e env)
 |Bool_Val _ , Bool_Val _ -> add_to_env env s (eval_expr e env)
 |Closure (_, _, _) , Closure (_, _, _) -> add_to_env env s (eval_expr e env)
 |_, _-> raise (TypeError))
 | Declare (t,s) -> (match t with Int_Type -> add_to_env env s (Int_Val 0)
 | Bool_Type -> add_to_env env s (Bool_Val false)
 | Lambda_Type -> add_to_env env s (Closure (env, "x", Var "x")))
