open Graphics
open Core
(* open Csv *)

class text_box x y width height text = object
  val x1 = x
  val y1 = y
  val x2 = x + width
  val y2 = y + height
  val width = width
  val height = height
  val mutable text = text

  val mutable justification = "center";

  (*colors*)
  val mutable br = 20
  val mutable bg = 20
  val mutable bb = 20
  val tr = 255
  val tg = 255
  val tb = 255

  method set_text t =
    text <- t

  method set_background_color r g b =
    br <- r;
    bg <- g;
    bb <- b;

  method draw =
    (*text background*)
    set_color (rgb br bg bb);
    fill_rect x1 y1 width height;

    (*button text*)
    set_color (rgb tr tg tb);
    let font = Printf.sprintf "-*-fixed-medium-r-semicondensed--%d-*-*-*-*-*-iso8859-1" 20 in
    set_font font;
    let size_x, size_y = text_size text in

    (*justification*)
    if String.equal "center" justification then
      moveto (((width - size_x) / 2) + x1) (((height - size_y) / 2) + y1)
    else if String.equal "left" justification then
      moveto x1 y1
    else if String.equal "right" justification then
      moveto (x2-size_x) y1;

    draw_string text;

  method set_justification j =
    justification <- j;
  
  end


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
  val mutable active = true

  method draw =
    (*button background*)

    if not active then ();

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
    if not active then ();
    let x, y = mouse_pos () in
    x >= x1 && x <= x2 && y >= y1 && y <= y2

  method check_press =
    if not active then ();

    if not (button_down ()) then
      currently_pressed <- false;

    if self#check_hover && button_down () && not currently_pressed then (
      currently_pressed <- true;
      true
    ) else
      false

  method set_active b = 
    active <- b;

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
  val fps : float = 30.0
  val mutable frame_counter : float = 0.0

  (*allow time to speed up*)
  val mutable seconds_per_second = 1.0;

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

  method get_fps = 
    fps

  method get_frame_counter = 
    frame_counter

  method set_frame_counter f = 
    frame_counter <- f;

  method get_seconds_per_second = 
    seconds_per_second

  method set_seconds_per_second s = 
    seconds_per_second <- s

  method get_pretty_seconds_per_second = 

  let mult = match seconds_per_second with
      | 1.0 -> "1 s"
      | 3.0 -> "3 s"
      | 30.0 -> "30 s"
      | 60.0 -> "1 m"
      | 300.0 -> "5 m"
      | 900.0 -> "15 m"
      | 1800.0 -> "30 m"
      | 3600.0 -> "1 h"
      | 10800.0 -> "3 h"
      | 86400.0 -> "1 d"
      | _ -> "error" in

    mult;

end

(*button sub classes*)
class time_change_button x y width height r g b text clock_obj inc = object (self)
  inherit button x y width height r g b text as super

  val clock_obj = clock_obj
  val increase = inc

  method! execute =
    if self#check_press then (

      let current_multiplier = clock_obj#get_seconds_per_second in

      if inc then (

        let new_multiplier = match current_multiplier with
        | 1.0 -> 3.0
        | 3.0 -> 30.0
        | 30.0 -> 60.0
        | 60.0 -> 300.0
        | 300.0 -> 900.0
        | 900.0 -> 1800.0
        | 1800.0 -> 3600.0
        | 3600.0 -> 10800.0
        | 10800.0 -> 86400.0
        | 86400.0 -> 86400.0
        | _ -> 0.0 in

        clock_obj#set_seconds_per_second new_multiplier;

      ) else (

        let new_multiplier = match current_multiplier with
        | 1.0 -> 1.0
        | 3.0 -> 1.0
        | 30.0 -> 3.0
        | 60.0 -> 30.0
        | 300.0 -> 60.0
        | 900.0 -> 300.0
        | 1800.0 -> 900.0
        | 3600.0 -> 1800.0
        | 10800.0 -> 3600.0
        | 86400.0 -> 10800.0
        | _ -> 0.0 in

        clock_obj#set_seconds_per_second new_multiplier;

      );

      super#execute;
      
    );
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

class data path filename = object (self)

  val path = path
  val mutable filename = filename
  val data : (string, row) Hashtbl.t = Hashtbl.create (module String)

  val mutable current_timestamp = "undefined"
  val mutable last_timestamp = "undefined"

  method init_data =
    let csv = Csv.load (path ^ filename ^ ".csv") in
    let process_row row =
      let parsed_row = parse_row row in
      Hashtbl.set data ~key:parsed_row.timestamp ~data:parsed_row
    in
    match csv with
    | [] -> failwith "Empty CSV file"
    | _header :: rows -> List.iter ~f:process_row rows;

    (*establish final timestamp*)
    self#get_last_key;

  method load_next_data = 
    let year = ref (int_of_string (String.sub filename ~pos:0 ~len:4)) in
    let month = ref (int_of_string (String.sub filename ~pos:5 ~len:2)) in

    month := !month + 1;
    if !month > 12 then (
      month := 1;
      year := !year + 1
    );

    let str_month = ref (string_of_int !month) in
    if !month < 10 then
      str_month := "0" ^ !str_month;
    
    let str_year = (string_of_int !year) in

    filename <- str_year ^ "-" ^ !str_month;

    (*ensure we dont try and access dataset too far in the future*)
    if !year < 2024 || (!year = 2024 && !month <= 7) then
      self#init_data
    else
      print_endline "Error: End of Dataset Reached!";


  method get_last_key = 
    let keys = Hashtbl.fold data ~init:[] ~f:(fun ~key ~data:_ acc -> key :: acc) in
    let last_key =
      match List.sort keys ~compare:String.compare |> List.rev with
      | [] -> None
      | last_key :: _ -> Some last_key
    in
    match last_key with
    | Some key -> last_timestamp <- key
    | None -> ()



  method get_last_timestamp = 
    last_timestamp;

  method get_row timestamp =
    Hashtbl.find data timestamp

  method get_current_timestamp =
    current_timestamp

  method set_current_timestamp timestamp = 
    current_timestamp <- timestamp

  method get_next_timestamp step = 

    (*check and see if we're on last timestamp*)
    if (String.equal current_timestamp last_timestamp) || (String.compare current_timestamp last_timestamp > 0) then (
      self#load_next_data;
      ()
    );

    let second = ref ((int_of_string (String.sub current_timestamp ~pos:17 ~len:2)) + step) in
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

    let found_next_price = ref false in
    while not !found_next_price do
      if Hashtbl.mem data (self#get_current_timestamp) then
        found_next_price := true
      else
        self#get_next_timestamp 1
    done;

  method get_current_price stock_name =

    let price = ref 0.0 in

    if (String.equal stock_name "emini") then (
      match self#get_row (self#get_current_timestamp) with
        | Some row -> (price := row.emini)
        | None -> ();
    )
    else if (String.equal stock_name "nasdaq") then (
      match self#get_row (self#get_current_timestamp) with
        | Some row -> (price := row.nasdaq)
        | None -> ();
    )
    else if (String.equal stock_name "russell") then (
      match self#get_row (self#get_current_timestamp) with
        | Some row -> (price := row.russell)
        | None -> ();
    )
    else if (String.equal stock_name "spy") then (
    match self#get_row (self#get_current_timestamp) with
      | Some row -> (price := row.spy)
      | None -> ();
    )
    else if (String.equal stock_name "qqq") then (
    match self#get_row (self#get_current_timestamp) with
      | Some row -> (price := row.qqq)
      | None -> ();
    )
    else if (String.equal stock_name "apple") then (
    match self#get_row (self#get_current_timestamp) with
      | Some row -> (price := row.apple)
      | None -> ();
    )
    else if (String.equal stock_name "microsoft") then (
    match self#get_row (self#get_current_timestamp) with
      | Some row -> (price := row.microsoft)
      | None -> ();
    )
    else if (String.equal stock_name "nvidia") then (
    match self#get_row (self#get_current_timestamp) with
      | Some row -> (price := row.nvidia)
      | None -> ();
    );


    !price

  method get_pretty_timestamp (fps : float) (num_frames : float) (mult : float) = 
    let second = ref (String.sub current_timestamp ~pos:17 ~len:2) in
    let minute = ref (String.sub current_timestamp ~pos:14 ~len:2) in
    let hour = ref (String.sub current_timestamp ~pos:11 ~len:2) in
    let day = ref (String.sub current_timestamp ~pos:8 ~len:2) in
    let month = ref (String.sub current_timestamp ~pos:5 ~len:2) in
    let year = ref (String.sub current_timestamp ~pos:0 ~len:4) in

    let month_in_text = match !month with
    | "01" -> "January"
    | "02" -> "February"
    | "03" -> "March"
    | "04" -> "April"
    | "05" -> "May"
    | "06" -> "June"
    | "07" -> "July"
    | "08" -> "August"
    | "09" -> "September"
    | "10" -> "October"
    | "11" -> "November"
    | "12" -> "December"
    | _ -> "Unknown Month"
    in


    let ampm = ref ("PM") in

    (*am or pm*)
    if ((int_of_string !hour) < 12 ) then
      ampm := "AM";

    let hour_adjusted = ref (string_of_int ((int_of_string !hour) mod 12)) in
      if String.equal "0" !hour_adjusted then
        hour_adjusted := "12";

    (*micro adjust seconds for inbetween 3 seconds*)
    let int_second = ref (int_of_string !second) in
    if (int_of_float num_frames) >= (int_of_float (fps /. mult)) then
      int_second := !int_second + 1;
    if (int_of_float num_frames) >= ((int_of_float (fps /. mult)) * 2) then
      int_second := !int_second + 1;

    second := string_of_int !int_second;
    if !int_second < 10 then
      second := "0" ^ !second;

    (self#get_day_of_week current_timestamp) ^ ", " ^ month_in_text ^ " " ^ !day ^ ", " ^ !year ^ " " ^ !hour_adjusted ^ ":" ^ !minute ^ ":" ^ !second ^ " " ^ !ampm

  method get_day_of_week timestamp =
    
    let day = ref (int_of_string (String.sub timestamp ~pos:8 ~len:2)) in
    let month = ref (int_of_string (String.sub timestamp ~pos:5 ~len:2)) in
    let year = ref (int_of_string (String.sub timestamp ~pos:0 ~len:4)) in

    (*jan 26th 2020 = Sunday*)
    let daycount = ref (((!year - 2020) * 365) + (!day-5)) in

    (*handle months*)
    if !month = 1 then
      daycount := (!daycount + 0)
    else if !month = 2 then
      daycount := (!daycount + 31)
    else if !month = 3 then
      daycount := (!daycount + 31 + 28)
    else if !month = 4 then
      daycount := (!daycount + 31 + 28 + 31)
    else if !month = 5 then
      daycount := (!daycount + 31 + 28 + 31 + 30)
    else if !month = 6 then
      daycount := (!daycount + 31 + 28 + 31 + 30 + 31)
    else if !month = 7 then
      daycount := (!daycount + 31 + 28 + 31 + 30 + 31 + 30)
    else if !month = 8 then
      daycount := (!daycount + 31 + 28 + 31 + 30 + 31 + 30 + 31)
    else if !month = 9 then
      daycount := (!daycount + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31)
    else if !month = 10 then
      daycount := (!daycount + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30)
    else if !month = 11 then
      daycount := (!daycount + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31)
    else if !month = 12 then
      daycount := (!daycount + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30);
    
    (*handle leap years*)
    if (!year >= 2020 && !month >= 3) then
      daycount := !daycount + 1;
    
    if (!year >= 2024 && !month >= 3) then
      daycount := !daycount + 1;

    daycount := !daycount mod 7;

    let day_of_week = match !daycount with
    | 0 -> "Sunday"
    | 1 -> "Monday"
    | 2 -> "Tuesday"
    | 3 -> "Wednesday"
    | 4 -> "Thursday"
    | 5 -> "Friday"
    | 6 -> "Saturday"
    | _ -> "error_month" in

    day_of_week

  method is_during_market_hours = 

    (*note, this method does not consider the extensive list of holidays and early closures*)

    let day_of_week = (self#get_day_of_week current_timestamp) in
    let minute = ref (int_of_string (String.sub current_timestamp ~pos:14 ~len:2)) in
    let hour = ref (int_of_string (String.sub current_timestamp ~pos:11 ~len:2)) in

    if (String.equal day_of_week "Saturday") || (String.equal day_of_week "Sunday") then
      false
    else 

      let num_minutes = !minute + (60 * !hour) in

      if (num_minutes >= 570) && (num_minutes < 960) then
        true
      else
        false;


  method get_pretty_after_hours =

    if not self#is_during_market_hours then
      "(After Hours)"
    else
      "";
      
      

end



class stock_graph mydata = object

  (*visualization*)
  val x1 = 50;
  val y1 = 125;
  val width = 900;
  val height = 600;

  (*stock info*)
  val mutable current_stock = "";
  val mutable current_value = 0.0;

  (*step size in seconds (min 3 seconds)*)
  val mutable step_size = 3;
  val mutable mydata = mydata;

  method set_current_stock s =
    current_stock <- s;

  method set_current_value v = 
    current_value <- v;

  method get_current_stock =
    current_stock;

  method get_current_value = 
    current_value;

  

  method draw =
    set_color (rgb 50 50 50);
    fill_rect x1 y1 width height;

  

end


class stock_value_viewer nname x y = object

  val stock_name = nname
  val x1 = x
  val y1 = y
  val width = 100
  val height = 40

  val mutable value = 0.0;

  method get_stock_name : string = stock_name

  method set_current_value v = 
    value <- v;

  method draw =
    set_color (rgb 200 200 200);
    fill_rect x1 y1 width height;

    (*value text*)
    let text = string_of_float value in 
    set_color black;
    let font = Printf.sprintf "-*-fixed-medium-r-semicondensed--%d-*-*-*-*-*-iso8859-1" 20 in
    set_font font;
    let size_x, size_y = text_size text in

    moveto (((width - size_x) / 2) + x1) (((height - size_y) / 2) + y1);
    draw_string text;



end
