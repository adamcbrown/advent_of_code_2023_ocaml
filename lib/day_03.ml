let (<<) f g x = f (g x)

let flip f y x = f x y

type symbol = {
  symbol: char;
  x: int;
  y: int;
}

type part = {
  number: int;
  x: int;
  y: int;
}

type parser = {
  i: int;
  x: int;
  y: int;
  input: string;
}
let curr parser = String.get parser.input parser.i
let eof parser = String.length parser.input == parser.i

let incr parser =
  if curr parser == '\n' then
    {parser with i = parser.i+1; y = parser.y+1; x = 0}
  else 
    {parser with i = parser.i+1; x = parser.x+1}

type map = {
  parts: part list;
  symbols: symbol list; 
}


let rec parse_map parser =
  if eof parser then
    {parts = []; symbols = []}
  else
  let parse_num parser =
    let rec helper parser =
      let c = curr parser in
      match c with 
      | '0' .. '9' ->
        let (rest, parser) = parser |> incr |> helper in
        ((c :: rest), parser)
      | _ -> [], parser
    in
    let num, parser = helper parser in
    (String.of_seq (List.to_seq num), parser)
  in
  let x = parser.x and y = parser.y in
  match curr parser with
  | '.' | '\n' -> parser |> incr |> parse_map
  | '0' .. '9' ->
    let (part_num, parser) = parse_num parser in
    let rest = parse_map parser in
    {rest with parts = {number = int_of_string part_num; x = x; y = y} :: rest.parts}
  | s -> 
    let rest = parser |> incr |> parse_map in
    {rest with symbols = {symbol = s; x = x; y = y} :: rest.symbols}
  
let touching part (symbol: symbol) =
  let part_len = String.length (string_of_int part.number) in
  match abs(part.y - symbol.y) with
  | 0 | 1 -> part.x >= symbol.x - part_len && part.x <= symbol.x+1
  | _ -> false

let part1 input =
  let map = parse_map {i = 0; x = 0; y = 0; input = input} in
  let touching_any part = List.fold_right ((||) << (touching part)) map.symbols false in
  let part_num part = part.number in
  List.fold_right ((+) << part_num) (List.filter touching_any map.parts) 0

let part2 input =
  let map = parse_map {i = 0; x = 0; y = 0; input = input} in
  let gear_ratio symbol =
    match symbol.symbol with
    | '*' -> (match List.filter ((flip touching) symbol) map.parts with
      | [p1; p2] -> Some(p1.number * p2.number)
      | _ -> None
    )
    | _ -> None
  in
  let cast_none x_opt = match x_opt with
  | None -> 0
  | Some(x) -> x
  in
  List.fold_right ((+) << cast_none << gear_ratio) map.symbols 0