open Card

class printer = object
  method print_card_list (l: card list) =
    List.iter (fun (c : card) -> c#print ()) l;
    print_newline ();
    flush stdout

  method repeat chr n =
    let rec aux chr n =
      if n <= 0 then print_newline ()
      else (Printf.printf "%s" chr; aux chr (n-1);)
    in
    aux chr n

  method clear_screen =
    print_string "\027[2J\027[H";
    flush stdout

  method delete_last_two_lines =
    (* \027[2A : remonte le curseur de 2 lignes (A = Up) *)
    (* \027[0J : efface du curseur jusqu'à la fin de l'écran (J = Erase) *)
    print_string "\027[2A\027[0J";
    flush stdout
end