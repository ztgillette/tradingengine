open Graphics
open Core
(* open Csv *)

class button x y width height r g b text = object (self)
  val x1 = x
  val y1 = y
  val x2 = x + width
  val y2 = y + height
  val width = width
  val height = height

  (*colors*)
  val r = r
  val g = g
  val b = b

  val text = text
  val mutable currently_pressed = false

  method draw =
    (*button background*)

    let color_shift = self#get_hover_color_shift in

    if self#check_hover then
      set_color (rgb (r-color_shift) (g-color_shift) (b-color_shift))
    else
      set_color (rgb r g b);
    fill_rect x1 y1 width height;

    (*button text*)
    set_color black;
    let font = Printf.sprintf "-*-fixed-medium-r-semicondensed--%d-*-*-*-*-*-iso8859-1" 20 in
    set_font font;
    let size_x, size_y = text_size text in

    moveto (((width - size_x) / 2) + x1) (((height - size_y) / 2) + y1);
    draw_string text;

  method check_hover = 
    let x, y = mouse_pos () in
    x >= x1 && x <= x2 && y >= y1 && y <= y2

  method check_press =
    if not (button_down ()) then
      currently_pressed <- false;

    if self#check_hover && button_down () && not currently_pressed then (
      currently_pressed <- true;
      true
    ) else
      false

  method execute =
    if self#check_press then
      print_endline "Pressed!"

  method get_hover_color_shift = 

    let min = 60 in

    if r <= g && r <= b && r <= min then
      r
    else if g <= r && g <= b && g <= min then
      g
    else if b <= r && b <= g && b <= min then
      b
    else 
      min;

end

class clock = object
  val mutable previous_time = 0.0
  val mutable current_time = 0.0

  method update_time = 
    current_time <- (Core_unix.gettimeofday () *. 1000.0);

  method get_current_time = 
    current_time

  method get_previous_time = 
    previous_time

  method set_current_time t = 
    current_time <- t

  method set_previous_time t = 
    previous_time <- t
end

module StringHash = Hashtbl.Make(String)

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

let parse_row row =
  match row with
  | [timestamp; emini; nasdaq; russell; spy; qqq; apple; microsoft; nvidia] ->
    {
      timestamp = timestamp;
      emini = float_of_string emini;
      nasdaq = float_of_string nasdaq;
      russell = float_of_string russell;
      spy = float_of_string spy;
      qqq = float_of_string qqq;
      apple = float_of_string apple;
      microsoft = float_of_string microsoft;
      nvidia = float_of_string nvidia;
    }
  | _ -> failwith "Invalid row format"

class data filepath = object
  val filepath = filepath
  val data : (string, row) Hashtbl.t = Hashtbl.create (module String)

  val mutable current_timestamp = "undefined"

  method init_data =
    let csv = Csv.load filepath in
    let process_row row =
      let parsed_row = parse_row row in
      Hashtbl.set data ~key:parsed_row.timestamp ~data:parsed_row
    in
    match csv with
    | [] -> failwith "Empty CSV file"
    | _header :: rows -> List.iter ~f:process_row rows

  method get_row timestamp =
    Hashtbl.find data timestamp

  method get_current_timestamp =
    current_timestamp

  method set_current_timestamp timestamp = 
    current_timestamp <- timestamp

  method get_next_timestamp = 

    (* 2020-01-26-17-59-02 *)

    let second = ref ((int_of_string (String.sub current_timestamp ~pos:17 ~len:2)) + 3) in
    let minute = ref (int_of_string (String.sub current_timestamp ~pos:14 ~len:2)) in
    let hour = ref (int_of_string (String.sub current_timestamp ~pos:11 ~len:2)) in
    let day = ref (int_of_string (String.sub current_timestamp ~pos:8 ~len:2)) in
    let month = ref (int_of_string (String.sub current_timestamp ~pos:5 ~len:2)) in
    let year = ref (int_of_string (String.sub current_timestamp ~pos:0 ~len:4)) in
    
    
    if !second > 59 then (
      second := !second - 60;
      minute := !minute + 1;
    );

    if !minute > 59 then (
      minute := 0;
      hour := !hour + 1;
    );

    if !hour > 23 then (
      hour := 0;
      day := !day + 1;
    );

    if !month = 2 then (
      if (!year mod 4 = 0 && !year mod 100 <> 0) || (!year mod 400 = 0) then (
        (* leap year *)
        if !day > 29 then (
          day := 1;
          month := !month + 1;
        )
      ) else (
        (* non leap year *)
        if !day > 28 then (
          day := 1;
          month := !month + 1;
        )
      )
    ) else if !month = 4 || !month = 6 || !month = 9 || !month = 11 then (
      (* 30 days *)
      if !day > 30 then (
        day := 1;
        month := !month + 1;
      )
    ) else (
      (* 31 days *)
      if !day > 31 then (
        day := 1;
        month := !month + 1;
      )
    );

    if !month > 12 then (
      month := 1;
      year := !year + 1;
    );

    
    (* adjust string formatting *)
    let str_second = ref (string_of_int !second) in
    let str_minute = ref (string_of_int !minute) in
    let str_hour = ref (string_of_int !hour) in
    let str_day = ref (string_of_int !day) in
    let str_month = ref (string_of_int !month) in
    let str_year = ref (string_of_int !year) in

    if !second < 10 then
      str_second := "0" ^ !str_second;

    if !minute < 10 then
      str_minute := "0" ^ !str_minute;

    if !hour < 10 then
      str_hour := "0" ^ !str_hour;

    if !day < 10 then
      str_day := "0" ^ !str_day;

    if !month < 10 then
      str_month := "0" ^ !str_month;

    current_timestamp <- !str_year ^ "-" ^ !str_month ^ "-" ^ !str_day ^ "-" ^ !str_hour ^ "-" ^ !str_minute ^ "-" ^ !str_second;

    print_endline current_timestamp;



end