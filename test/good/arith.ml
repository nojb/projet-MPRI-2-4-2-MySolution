(* Arithmetic on positive integers, represented in binary. *)

type positive = 
  | H                (* 1 *)
  | O of positive    (* 2x *)
  | I of positive    (* 2x+1 *)

type 'a option =
  | None
  | Some of 'a 

program

let rec succ x =               (* x + 1 *)
  match x with
  | H -> O H    
  | O x' -> I x'               (* (2x') + 1 = 2x'+1 *)
  | I x' -> O (succ x')        (* (2x'+1) + 1 = 2(x' + 1) *)
  end
in

let rec double_minus_one x =   (* 2x - 1 *)
  match x with
  | H -> H
  | O x' -> I (double_minus_one x')   (* 2.(2x') - 1 = 2.(2x'-1) + 1 *)
  | I x' -> I (O x')                  (* 2.(2x'+1) - 1 = 2.(2x') + 1 *)
  end
in

let pred = fun x ->            (* x - 1, if defined *)
  match x with
  | H -> None
  | O x' -> Some(double_minus_one x')
  | I x' -> Some(O x')
  end
in

let rec plus x = fun y ->      (* x + y *)
  match x with
  | H -> succ y
  | O x' ->
      match y with
      | H -> succ x
      | O y' -> O (plus x' y')
      | I y' -> I (plus x' y')
      end
  | I x' ->
      match y with
      | H -> succ x
      | O y' -> I (plus x' y')
      | I y' -> O (succ (plus x' y'))
      end
  end
in

let rec mult x = fun y ->      (* x * y *)
  match x with
  | H -> y
  | O x' -> O (mult x' y)
  | I x' -> plus (O (mult x' y)) y
  end
in

let rec fact x =               (* x! *)
  match pred x with
  | None -> H
  | Some x' -> mult x (fact x')
  end
in

fact (O (I H)) (* 6 *)

(* Expected result: 720 = O (O (O (O (I (O (I (I (O H)))))))) *)
