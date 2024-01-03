(* Advent of Code 2023, Day 18, part 1 - Lavaduct Lagoon. *)
(* OCaml *)

(* Direction to dig (viewed from above.) *)
type directionType = R | D | L | U

(* Direction to turn (viewed from above.) *)
type turnType = Clockwise | Anticlockwise | None

(* Define a single dig. *)
type dig = {
  direction  : directionType;
  length     : int;
  colourCode : string;
}

(* Convert hex colour for part 2 *)
let convertDig dig =
  let hexToDecimal hex_string =
    int_of_string ("0x" ^ hex_string) in  
  let length = 
    String.sub dig.colourCode 2 5
    |> hexToDecimal in
  let direction =
    match String.sub dig.colourCode 7 1 with
    | "0" -> R
    | "1" -> D
    | "2" -> L
    | "3" -> U
    | _ -> failwith "Invalid direction." in
  {dig with direction = direction; length = length}

(* Parse a single line of the dig plan. *)
let parseDigLine digLine =
  match String.split_on_char ' ' digLine with
  | [first; second; third] -> 
      let direction =
        match first with
        | "U" -> U
        | "L" -> L
        | "R" -> R
        | "D" -> D
        | _ -> failwith "Invalid direction." in
      let length = int_of_string second in
      let colourCode = third in
      {direction = direction; length = length; colourCode = colourCode}
  | _ -> failwith "Invalid dig line."

(* Parse the input into a list of dig types. *)
let parseInputFile filename =    
  (* Apply to the file. *)
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic in
      let tuple = parseDigLine line in
      loop (tuple :: acc)
    with
    | End_of_file -> close_in ic; List.rev acc
  in 
  loop []

(* Add two sets of coords passed as tuples *)
let addCoords coord1 coord2 =
  (fst coord1 + fst coord2, snd coord1 + snd coord2)

(* Subtract two sets of coords passed as tuples *)
let subtractCoords coord1 coord2 =
    (fst coord1 - fst coord2, snd coord1 - snd coord2)
  
(* Get determinant of two sets of coords passed as tuples *)
let detCoords coord1 coord2 =
  fst coord1 * snd coord2 - fst coord2 * snd coord1

(* Get next dig coord with offset *)
let getNextDigCoord (coords, digPair) =
  let ((cornerPoint, (x, y), offSetCoords), (dig1, dig2)) = coords, digPair in
  let turn =    
    match dig1.direction, dig2.direction with
    | R, D | D, L | L, U | U, R -> 
      Clockwise
    | L, D | D, R | R, U | U, L -> 
      Anticlockwise
    | _ -> failwith "Invalid turn"
  in
  let cornerPoint' =
    match cornerPoint with
    | 0, 0 -> if turn = Clockwise then 1, 0 else 0, 1
    | 1, 1 -> if turn = Clockwise then 0, 1 else 1, 0
    | 0, 1 -> if turn = Clockwise then 0, 0 else 1, 1
    | 1, 0 -> if turn = Clockwise then 1, 1 else 0, 0
    | _ -> (failwith "Invalid corner point.")
  in
  let coord =
    match dig1.direction with
    | U -> (0, -dig1.length)
    | D -> (0, dig1.length)
    | L -> (-dig1.length, 0)
    | R -> (dig1.length, 0)
  in
  let newCornerPoint = cornerPoint' in
  let cornerOffSet = subtractCoords cornerPoint' cornerPoint in
  let newCoords = addCoords (x, y) (addCoords coord cornerOffSet) in
  let offSetCoords = if turn = Clockwise then (0,0) else cornerOffSet in 
  ((newCornerPoint, newCoords, offSetCoords), (dig1, dig2))
  
(* Run a sliding window of pairs through coords *)
let rec pairwise lst =
  match lst with
  | [] | [_] -> []
  | x :: y :: rest -> (x, y) :: pairwise (y :: rest)

(* Get dig plan from input data. *)
let digPlanPairs = 
  parseInputFile "Day18Input.txt"
  |> List.map (convertDig)
  |> pairwise
  
(* Now process all dig pairs *)
let rec processDigPairs coords (digPlanPairs: (dig * dig) list) (n: int) acc =
  if n < List.length digPlanPairs then
    let digPlanPair = List.nth digPlanPairs n in
    let (coords', _) = getNextDigCoord (coords, digPlanPair) in
    processDigPairs coords' digPlanPairs (n+1) (coords' :: acc)
  else
    List.rev acc  (* Reverse the accumulator to maintain the correct order *)
      
(* Finally we have all the points accounting for rotation of outer point *)
let digCoords = 
  processDigPairs ((0,0),(0,0),(0,0)) digPlanPairs 0 []
  |> List.map (fun (_,b,c) -> subtractCoords b c)

(* Apply shoelace formula to get area *)
let area = 
  pairwise digCoords
  |> List.map (fun c -> detCoords (fst c) (snd c))
  |> List.fold_left (fun acc elem -> acc +elem) 0
  |> (fun a -> a/2)

let () = Printf.printf "Part 2 answer: %d\n" area