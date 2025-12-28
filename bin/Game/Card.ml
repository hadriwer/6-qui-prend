class card nombre_tete value = object
  val nombre_tete = nombre_tete 
  method get_nombre_tete () = nombre_tete
  val value = value
  method get_value () = value

  method print () = Printf.printf "{%d ; 𓄀 = %d} " value nombre_tete
end