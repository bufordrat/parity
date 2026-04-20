type number =
  | Odd of int
  | Even of int

let int_to_number n =
  if n mod 2 = 0
  then Even n
  else Odd n

let number_to_int =
  function
  | Odd n -> n
  | Even n -> n

let add n m =
  match n, m with
  | Odd n', Odd m' -> Even (n' + m')
  | Odd n', Even m' -> Odd (n' + m')
  | Even n', Even m' -> Even (n' + m')
  | Even n', Odd m' -> Odd (n' + m')

let sub n m =
  match n, m with
  | Odd n', Odd m' -> Even (n' - m')
  | Odd n', Even m' -> Odd (n' - m')
  | Even n', Even m' -> Even (n' - m')
  | Even n', Odd m' -> Odd (n' - m')

let mult n m =
  match n, m with
  | Odd n', Odd m' -> Odd (n' * m')
  | Odd n', Even m' -> Even (n' * m')
  | Even n', Even m' -> Even (n' * m')
  | Even n', Odd m' -> Even (n' * m')
