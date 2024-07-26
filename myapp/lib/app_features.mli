(* lib/my_classes.mli *)

class text_box : int -> int -> int -> int -> string -> object
    method set_text : string -> unit
    method set_background_color : int -> int -> int -> unit
    method draw : unit
    method set_justification : string -> unit
end 

class button : int -> int -> int -> int -> int -> int -> int -> string -> object
    method draw : unit
    method check_hover : bool
    method check_press : bool
    method set_active : bool -> unit
    method execute : unit
    method get_hover_color_shift : int
end

class clock : object
    method update_time : unit
    method get_current_time : float
    method get_previous_time : float
    method set_current_time : float -> unit
    method set_previous_time : float -> unit
    method get_fps : float
    method get_frame_counter : float
    method set_frame_counter : float -> unit
    method get_seconds_per_second : float
    method set_seconds_per_second : float -> unit
    method get_pretty_seconds_per_second : string
end

class time_change_button : int -> int -> int -> int -> int -> int -> int -> string -> clock -> bool -> object
    inherit button
    method draw : unit
    method check_hover : bool
    method check_press : bool
    method set_active : bool -> unit
    method execute : unit
    method get_hover_color_shift : int
end

type row = {
  timestamp : string;
  emini : float;
  nasdaq : float;
  russell : float;
  spy : float;
  qqq : float;
  apple : float;
  microsoft : float;
  nvidia : float;
}
  
class data : string -> string -> object
    method init_data : unit
    method load_next_data : unit
    method get_last_key : unit
    method get_last_timestamp : string
    method get_row : string -> row option
    method get_current_timestamp : string
    method set_current_timestamp : string -> unit
    method get_next_timestamp : int -> unit
    method get_current_price : string -> float
    method get_pretty_timestamp : float -> float -> float -> string
    method get_day_of_week : string -> string
    method is_during_market_hours : bool
    method get_pretty_after_hours : string
end

class stock_graph : data -> object
    method draw : unit
    method set_current_stock : string -> unit
    method set_current_value : float -> unit
    method get_current_stock : string
    method get_current_value : float

end

class stock_value_viewer : string -> int -> int -> object
    method get_stock_name : string 
    method set_current_value : float -> unit
    method draw : unit

end