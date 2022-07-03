
let filename = "resources/day2.in"

let parse_line_part1 line = 
  match (String.split_on_char ' ' line) with
  | ["forward"; diff] -> fun (d, h) -> (d, h + (int_of_string diff))
  | ["down"; diff] -> fun (d, h) -> (d + (int_of_string diff), h)
  | ["up"; diff] -> fun (d, h) -> (d - (int_of_string diff), h)
  | _ -> fun (d, h) -> (d, h)

let parse parse_line filename = 
  let lines = Core.In_channel.read_lines filename in
  lines |> List.map parse_line

let fold_instructions start instructions =
  List.fold_left (|>) start instructions

let print_result_part1 (d, h) =
  Printf.printf "(%i, %i) -> %i\n" d h (d * h)

(* part 1 *)
let _ = filename |> parse parse_line_part1 |> fold_instructions (0, 0) |> print_result_part1


let parse_line_part2 line = 
  match (String.split_on_char ' ' line) with
  | ["forward"; diff] -> fun (d, h, a) -> (d + a * (int_of_string diff), h + (int_of_string diff), a)
  | ["down"; diff] -> fun (d, h, a) -> (d, h, a + (int_of_string diff))
  | ["up"; diff] -> fun (d, h, a) -> (d, h, a - (int_of_string diff))
  | _ -> fun (d, h, a) -> (d, h, a)

let print_result_part2 (d, h, a) =
  Printf.printf "(%i, %i, %i) -> %i\n" d h a (d * h)

(* part 2 *)
let _ = filename |> parse parse_line_part2 |> fold_instructions (0, 0, 0) |> print_result_part2
