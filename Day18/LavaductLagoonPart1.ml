(* Advent of Code 2023, Day 18, part 1 - Lavaduct Lagoon. *)
(* OCaml *)

(* Direction to dig (viewed from above.) *)
type directionType = R | D | L | U

(* Define a single dig. *)
type dig = {
  direction  : directionType;
  length     : int;
  colourCode : string;
}

(* Parse the input into a list of dig types. *)
let parseInputFile filename =    
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
    | _ -> failwith "Invalid dig line." in
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

(* From a dig extract coordinate vector *)
let getDigCoord dig =
  match dig.direction with
  | U -> (0, -dig.length)
  | D -> (0, dig.length)
  | L -> (-dig.length, 0)
  | R -> (dig.length, 0)

(* Fold on a list and return intermediate results *)
let listScan f acc lst =
  let _, result =
    List.fold_left
      (fun (acc, result) x ->
         let newResult = f acc x in
         (newResult, result @ [newResult]))
      (acc, []) lst
      in result

(* Min list function *)
let findMin lst =
  match lst with
  | [] -> failwith "Empty list"
  | hd :: tl -> List.fold_left min hd tl

(* Max list function *)
let findMax lst =
  match lst with
  | [] -> failwith "Empty list"
  | hd :: tl -> List.fold_left max hd tl
  
(* Run a sliding window of pairs through coords *)
let rec pairwise lst =
  match lst with
  | [] | [_] -> []
  | x :: y :: rest -> (x, y) :: pairwise (y :: rest)

(* Add two sets of coords passed as tuples *)
let addCoords coord1 coord2 =
  (fst coord1 + fst coord2, snd coord1 + snd coord2)

(* Generate a range off cords along x or y *)
let generate_coords start_point end_point axis =
  let generate_coords_x start_point end_point =  
    let step = if fst start_point <= fst end_point then 1 else -1 in
    let rec loop acc value =
      if (if step > 0 then value > fst end_point else value < fst end_point) then List.rev acc
      else loop ((value, snd start_point) :: acc) (value + step)
    in
    loop [] (fst start_point) in  
    let generate_coords_y start_point end_point =
    let step = if snd start_point <= snd end_point then 1 else -1 in
    let rec loop acc value =
      if (if step > 0 then value > snd end_point else value < snd end_point) then List.rev acc
      else loop ((fst start_point, value) :: acc) (value + step)
    in
    loop [] (snd start_point) in  
  if axis = "x" then
    generate_coords_x start_point end_point 
  else
    generate_coords_y start_point end_point 

(* Get dig plan from input data. *)
let digPlan = parseInputFile "Day18Input.txt"

(* Get all coords to dig *)
let digCoords =
  let digCoords' = 
    let digCoords' = List.map getDigCoord digPlan in
    [(0,0)] @ listScan addCoords (0, 0) digCoords'
    |> pairwise in
  digCoords'
  |> List.map (fun (c1,c2) -> if fst c1 = fst c2 then generate_coords c1 c2 "y" else generate_coords c1 c2 "x")
  |> List.flatten

(* Obtain bounds for grid *)
let getDigBounds digCoords =
  let minx = List.map fst digCoords |> findMin in
  let maxx = List.map fst digCoords |> findMax in
  let miny = List.map snd digCoords |> findMin in
  let maxy = List.map snd digCoords |> findMax in
  let xbound = maxx - minx + 3 in
  let ybound = maxy - miny + 3 in
  let xoffset = if minx < 0 then 1-minx else 1 in
  let yoffset = if miny < 0 then 1-miny else 1 in
  (xbound, ybound, xoffset, yoffset, xbound*ybound)

(* Create grid  for blocks *)
let createGrid rows cols initValue =
  let grid = Array.make_matrix rows cols initValue in
  grid

(* Perform flood fill from a given point *)
let rec floodFill grid x y oldChar newChar =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  if x < 0 || x >= rows || y < 0 || y >= cols then
    ()  (* Outside the image boundaries *)
  else if grid.(x).(y) = oldChar then begin
    (* Fill current block with new char *)
    grid.(x).(y) <- newChar;
    (* Recursively fill adjacent blocks *)
    floodFill grid (x + 1) y oldChar newChar;
    floodFill grid (x - 1) y oldChar newChar;
    floodFill grid x (y + 1) oldChar newChar;
    floodFill grid x (y - 1) oldChar newChar;
  end
  else
    ()

(* Display current grid *)
let displayGrid grid =  
    Array.iter (fun row ->
      Array.iter (fun elem -> print_char elem) row;
      print_newline ()
    ) grid

(* Count chars in grid *)
let countGrid grid targetChar =
  let counts = 
    (Array.map (fun row ->
      Array.map (fun elem -> if elem = targetChar then 1 else 0) row) grid) in
  let counts1D = Array.fold_right Array.append counts [||] in
  Array.fold_left (fun acc c -> acc + c) 0 counts1D 

(* Get bounds etc. *)
let xbound, ybound, xoffset, yoffset, noBlocks = getDigBounds digCoords 

(* Create grid. *)
let grid = createGrid ybound xbound '.'

(* Dig holes. *)
let () = List.iter (fun c -> 
  grid.(yoffset + snd c).(xoffset + fst c) <- '#')
  digCoords

(* Flood fill outside. *)
let () = floodFill grid 0 0 '.' '-'

(* Finally count number of dug bocks and display result. *)
let noDugBlocks = noBlocks - (countGrid grid '-')
let () = Printf.printf "Part 1 answer: %d\n" noDugBlocks
