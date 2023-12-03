let (<<) f g x = f (g x)

let part1 input =
  let solve input =
    let rec find i di = 
      if input.[i] >= '0' && input.[i] <= '9' then
        int_of_char input.[i] - int_of_char '0'
      else
        find (i + di) di
    in
      (10 * find 0 1) + (find (String.length input - 1) (-1))
  in
  let inputs = String.split_on_char '\n' input in
  List.fold_right ((+) << solve) inputs 0

let part2 input =
  let mapping = [
    ("0", 0);
    ("1", 1);
    ("2", 2);
    ("3", 3);
    ("4", 4);
    ("5", 5);
    ("6", 6);
    ("7", 7);
    ("8", 8);
    ("9", 9);
    ("zero", 0);
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
  ] in
  let solve input =
    let rec find i di = 
      let substr = String.sub input i (String.length input - i) in
      match List.find_opt (fun (a, _) -> String.starts_with ~prefix:a substr) mapping with
      | Some(_, n) -> n
      | None -> find (i + di) di
    in
      let ones = find (String.length input - 1) (-1) and
      tens = find 0 1 in
      (10 * tens) + (ones)
  in
  let inputs = String.split_on_char '\n' input in
  List.fold_right ((+) << solve) inputs 0
