(* lib/my_classes.mli *)

class text_box : int -> int -> int -> int -> string -> object
    method set_text : string -> unit
    method set_background_color : int -> int -> int -> unit
    method draw : unit
    method set_justification : string -> unit
end 

class button : int -> int -> int -> int -> int -> int -> int -> string -> object
    method draw : unit
    method set_light_text_color : unit
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
    method get_cpu_helper : int
end

class time_change_button : int -> int -> int -> int -> int -> int -> int -> string -> clock -> bool -> object
    inherit button
    method draw : unit
    method set_light_text_color : unit
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
    method get_next_timestamp : int -> int -> unit
    method get_current_price : string -> float
    method get_pretty_timestamp : float -> float -> float -> int -> string
    method get_day_of_week : string -> string
    method is_during_market_hours : bool
    method get_pretty_after_hours : string
end

class stock_graph : data -> object
    method draw : unit
    method get_data : string -> string -> float option
    method add_timestamp : string -> unit
    method add_data : string -> string -> float -> unit
    method set_current_stock : string -> unit
    method set_current_value : float -> string -> unit
    method get_current_stock : string
    method get_current_value : float
    method get_timeline : int
    method set_timeline : int -> unit
    method get_pretty_timeframe : string
    method draw_segment : string -> int -> int -> string -> string -> unit
    method draw_data : unit
    method draw_lines : unit
    method get_x : int -> int -> float
    method get_y : string -> float -> int
    method get_low_high_point : string -> bool -> float
    method get_min_max : string -> string -> float option
    method init_min_max : string -> float -> string -> unit
    method get_bound : bool -> int
    method set_segment_color : unit
    method get_currently_up : string -> bool
    method set_sigfigs : string -> int -> string

end

class change_graph_timeframe : int -> int -> int -> int -> int -> int -> int -> string -> stock_graph -> bool -> object
    inherit button
    method set_light_text_color : unit
    method draw : unit
    method check_hover : bool
    method check_press : bool
    method set_active : bool -> unit
    method execute : unit
    method get_hover_color_shift : int
end

class stock_value_viewer : string -> int -> int -> object
    method get_stock_name : string 
    method set_current_value : float -> unit
    method draw : unit
    method set_currently_up : bool -> unit
end

class change_graph_stock : int -> int -> int -> int -> int -> int -> int -> string -> stock_graph -> string -> object
    inherit button
    method draw : unit
    method set_light_text_color : unit
    method check_hover : bool
    method check_press : bool
    method set_active : bool -> unit
    method execute : unit
    method get_hover_color_shift : int

end