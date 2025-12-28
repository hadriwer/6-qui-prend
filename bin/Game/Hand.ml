open Card

class hand = object(self)
  val mutable hand : card list = []
  method get_hand = hand

  method add_card c = hand <- c :: hand

  method add_all_card (l : card list ref) = 
    List.iter (self#add_card) !l

  method play j =
    let rec take_card j acc = function
        | [] -> failwith "error take_card : hand empty."
        | hd::tl when j = 0 -> (hd, (acc |> List.rev) @ tl)
        | hd::tl -> take_card (j-1) (hd::acc) tl
    in
    let (card_to_play, new_hand) = take_card j [] hand in
    hand <- new_hand;
    card_to_play

  method print = List.iter (fun c -> c#print ()) hand
end