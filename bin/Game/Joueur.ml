open Hand

class joueur id = object 
  val id : int = id
  method get_id : int = id

  val hand = new hand
  method get_hand = hand

  val hand_point = new hand
  method get_hand_point = hand_point

  method add_card c = hand#add_card c

  method play j = hand#play j

  method print = Printf.printf "Joueur n°%d " id
end