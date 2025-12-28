open Card
open Printer

class factory_card = object(self)
  inherit printer

  val pioche = ref []
  method get_pioche = pioche

  method pop =
    let tmp = List.hd !pioche in
    pioche := List.tl !pioche;
    tmp

  method private env numero =
    match numero with
    | 55 -> 7
    | n when (n mod 11) = 0 -> 5
    | n when (n mod 10) = 0 -> 3
    | n when (n mod 5) = 0 -> 2
    | _ -> 1

  method shuffle_pioche =
    let new_pioche = List.fast_sort (fun _ _ -> if Random.bool () then 1 else -1) !pioche in
    pioche := new_pioche;

  method private remplir_pioche =
    for i = 1 to 104 do
      let carte = new card (self#env i) i in
      pioche := carte :: !pioche
    done;

  initializer
    self#remplir_pioche;
    self#shuffle_pioche;
end