open Board
open Printer

class main_game nb_joueur = object
  inherit printer as super

  val board = new board nb_joueur
  method get_board = board
  val k = 7
  val nb_joueurs = nb_joueur

  method start_game =
    super#clear_screen;
    Printf.printf "6-qui-prend : %d joueurs.\n" nb_joueur;
    board#init_card;
    board#draw_card k;

    (* K tours *)
    for i = 1 to k do
      Printf.printf "Manche numéros %d\n" i;
      board#print;

      let carte_choisie = ref [] in
      for i = 0 to nb_joueur - 1 do
        let current_player = (board#get_joueur i) in
        Printf.printf "Au tour du joueur %d, voici sa main : \n" i;
        current_player#get_hand#print;
        Printf.printf "carte a jouer : ";
        let j = read_int () in
        let card = current_player#get_hand#play (j - 1) in
        carte_choisie := (current_player, card) :: !carte_choisie;
        super#delete_last_two_lines;
      done;
      (* On trie les cartes choisies par les joueurs en gardant les joueurs*)
      carte_choisie := List.fast_sort (fun (_, a) (_, b) -> compare (a#get_value ()) (b#get_value ())) !carte_choisie;

      (* On joue les cartes *)
      List.iter (fun (j, card) ->
        let game_res = board#add_on_stack card in
        match game_res with
          (* Si ma carte est plus petite que celle de tous les stacks *)
          | CHOOSE -> (
              Printf.printf "Joueur %d, " j#get_id;
              Printf.printf "Votre carte est trop petite. Choisir un tas a prendre (1-4) :";
              let choose = read_int () in
              let choosed =
                match choose with
                  | 1 -> board#get_s1
                  | 2 -> board#get_s2
                  | 3 -> board#get_s3
                  | 4 -> board#get_s4
                  | _ -> failwith "error start_game : wrong stack choosed."
              in
              board#add_all_hand_point card j choosed;
              Printf.printf "Cartes dans la main de défausse du "; j#print;
              j#get_hand_point#print;
              print_newline();
          )
          | ALL s -> (j#print; print_endline "prend le tas."; j#get_hand_point#add_all_card s)
          | STACK -> ()
      ) !carte_choisie;

      (* Pour passer a la manche suivante *)
      print_endline "Appuyez sur une touche pour passer a la manche suivante !";
      let _ = read_line () in
      super#clear_screen;
    done;

    (* Résultats *)
    print_endline "Fin du jeu voici les scores de chaque joueurs : ";
    for i = 0 to nb_joueur - 1 do
      let current_j = board#get_joueur i in
      Printf.printf "Résultats pour : "; current_j#print;
      let res = 
        current_j#get_hand_point#get_hand 
        |> List.fold_left (fun r e -> (e#get_nombre_tete ()) + r) 0
      in
      Printf.printf "Nombre de tête = %d\n" res;
    done;
    let _ = read_line () in
    ()
end