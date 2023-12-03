let (<<) f g x = f (g x)

type round = {
  mutable r: int;
  mutable g: int;
  mutable b: int;
}

type game = {
  id: int;
  rounds: round list;
}

let parse_game input =
  (* print_string input; print_newline(); *)
  let colon_idx = String.index input ':' in
  let parse_round input = 
    (* print_string input; print_newline(); *)
    let round = {r=0; g=0; b=0} in
    let parse_pull input = 
      (* print_string input; print_newline(); *)
      let space_index = String.index_from input 1 ' ' in
      let count = int_of_string (String.sub input 1 (space_index - 1)) and
      color = String.sub input (space_index + 1) ((String.length input) - space_index - 1) in
      (count, color)
    in
    let populate (count, color) = match color with
    | "red" -> round.r <- count
    | "green" -> round.g <- count
    | "blue" -> round.b <- count
    | _ -> failwith (Printf.sprintf "Unknown color %s" color)
    in
    List.iter (populate << parse_pull) (String.split_on_char ',' input);
    round
  in
  let id = int_of_string (String.sub input 5 (colon_idx - 5)) in
  let rounds_input = String.sub input (colon_idx + 1) (String.length input - colon_idx - 1) in
  let rounds = List.map parse_round (String.split_on_char ';' rounds_input) in
  {id ; rounds}

let part1 input =
  let is_valid_game game =
    let is_valid_round r = r.r <= 12 && r.g <= 13 && r.b <= 14 in
    List.fold_right ((&&) << is_valid_round) game.rounds true
  in
  let game_id game = game.id in
  let games = List.map parse_game (String.split_on_char '\n' input) in
  List.fold_right ((+) << game_id) (List.filter is_valid_game games) 0

let part2 input =
  let smallest_bag game = List.fold_left (fun curr round -> {
      r = max curr.r round.r;
      g = max curr.g round.g;
      b = max curr.b round.b;
    }) {r=0; g=0; b=0} game.rounds
  and
  power round = round.r * round.g * round.b in
  let games = List.map parse_game (String.split_on_char '\n' input) in
  List.fold_right ((+) << power << smallest_bag) games 0