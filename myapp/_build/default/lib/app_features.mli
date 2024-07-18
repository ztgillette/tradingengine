(* lib/my_classes.mli *)
class button : int -> int -> int -> int -> int -> int -> int -> string -> object
    method draw : unit
    method check_hover : bool
    method check_press : bool
    method execute : unit
    method get_hover_color_shift : int
end

class clock : object
    method update_time : unit
    method get_current_time : float
    method get_previous_time : float
    method set_current_time : float -> unit
    method set_previous_time : float -> unit
end
  