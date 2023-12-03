let usage_msg = "advent_of_code_2023 -day <day_number> [-pt2] [-test]"

let day = ref 0
let test = ref false
let pt2 = ref false

let anon_fun _ = ()

let speclist = [
  ("-day", Arg.Set_int day, "Day Number to Run");
  ("-test", Arg.Set test, "Run Test Input");
  ("-pt2", Arg.Set pt2, "Run Part 2");
]

let () =
  Arg.parse speclist anon_fun usage_msg;
  let part1, part2 = match !day with
  | 1 -> Advent_of_code_2023.Day_01.(part1, part2)
  | 2 -> Advent_of_code_2023.Day_02.(part1, part2)
  | _ -> raise (Failure "Unknown Day") in
  let part = match !pt2 with
  | true -> part2
  | false -> part1
  in
  let path = if !test then
    Printf.sprintf "data/test/day%02d.txt" !day
  else
    Printf.sprintf "data/actual/day%02d.txt" !day in
  let input = In_channel.input_all (open_in path) in
  let sol = part input in
  Printf.printf "%d\n" sol 

  
