open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Some 9
let%test "test_eval_2" = parse "10-8" |> eval = Some 2


(* YOUR TESTS HERE *)