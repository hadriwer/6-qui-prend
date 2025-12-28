open FactoryCard
open Card
open Printer
open Joueur
type game_result = STACK | CHOOSE | ALL of card list ref

class board (nb_joueur : int) = object
  inherit printer as super

  val nb_joueur = nb_joueur
  val mutable joueurs = [||]
  method get_joueur i = joueurs.(i)

  val s1 = ref []
  method get_s1 : card list ref = s1
  val s2 = ref []
  method get_s2 : card list ref = s2
  val s3 = ref []
  method get_s3 : card list ref = s3
  val s4 = ref []
  method get_s4 : card list ref = s4

  method print =
    print_endline "BOARD";
    super#repeat "-" 40;
    Printf.printf "1 = "; super#print_card_list (List.rev !s1);
    Printf.printf "2 = "; super#print_card_list (List.rev !s2);
    Printf.printf "3 = "; super#print_card_list (List.rev !s3);
    Printf.printf "4 = "; super#print_card_list (List.rev !s4);
    super#repeat "-" 40

  val pioche = new factory_card
  method get_pioche = pioche

  method init_card =
    List.iter (fun stack ->
      stack := (pioche#pop) :: !stack;
    ) [s1; s2; s3; s4]

  method add_all_hand_point (card : card) (joueur : joueur) (s : card list ref) =
    joueur#get_hand_point#add_all_card s;
    s := [card]

  method add_on_stack (c : card) =
    let best = ref None in
    List.iter (fun (e : card list ref) ->
      try
        let hd = List.hd !e in
        let diff = abs (hd#get_value () - c#get_value ()) in
        if hd#get_value () < c#get_value ()
        then (
          match !best with
            | None -> best := Some (diff, e);
            | Some (b, _) -> if diff < b then best := Some (diff, e); 
        )
      with 
        Failure _ -> failwith "error add_card : liste vide."
    ) [s1; s2; s3; s4];
    match !best with
      | None -> CHOOSE
      | Some (_, s) -> (
        if List.length !s = 5
        then (let tmp = ref !s in s := [c]; ALL tmp)
        else (
            s := c :: !s;
            STACK
          )
        )

    method draw_card n =
      for i = 0 to nb_joueur - 1 do
        for _ = 0 to n - 1 do
          joueurs.(i)#add_card (pioche#pop);
        done;
      done;

    initializer
      joueurs <- Array.init nb_joueur (fun i -> new joueur i)
end