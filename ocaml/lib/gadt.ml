type odd = |
type even = |

type _ number =
  | Zero : even number
  | ESucc : even number -> odd number
  | OSucc : odd number -> even number

let rec number_to_int : type a. a number -> int =
  function
  | Zero -> 0
  | ESucc n -> 1 + number_to_int n
  | OSucc n -> 1 + number_to_int n

let rec int_to_even_opt : int -> even number option =
  function
  | 0 -> Some Zero
  | n when n mod 2 = 0 ->
     let ( let+ ) opt f = Option.map f opt in
     let+ next = int_to_even_opt (n - 2)
     in OSucc (ESucc next)
  | _ -> None

let rec int_to_odd_opt : int -> odd number option =
  function
  | 1 -> Some (ESucc Zero)
  | n when n mod 2 <> 0 ->
     let ( let+ ) opt f = Option.map f opt in
     let+ next = int_to_odd_opt (n - 2)
     in ESucc (OSucc next)
  | _ -> None

type any_number = Any : 'a number -> any_number

let int_to_number_opt : int -> any_number option =
  function
  | 0 -> Some (Any Zero)
  | n when n mod 2 = 0 ->
     let ( let+ ) opt f = Option.map f opt in
     let+ next = int_to_even_opt (n - 1)
     in Any (ESucc next)
  | n ->
     let ( let+ ) opt f = Option.map f opt in
     let+ next = int_to_odd_opt (n - 1)
     in Any (OSucc next)
