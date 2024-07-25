open Graphics
open App_features

(* Sets the default program dark-gray background *)
let clear_background () = 
  let my_gray = rgb 20 20 20 in
  set_color my_gray;
  fill_rect 0 0 (size_x ()) (size_y ())

let () =

  (*set up stock data*)
  let stock_data = new data "/Users/ztgillette/Documents/CS/Personal\ Projects/Other/tradingengine/data/2020-01.csv" in
  stock_data#init_data;
  stock_data#set_current_timestamp "2020-01-26-17-59-02";


  (* Open a graphics window *)
  open_graph " 1200x800";

  (* Set window title *)
  set_window_title "Trading Engine";

  (* Button objects *)
  let buy_button = new button 50 50 100 40 100 255 100 "Buy" in
  let sell_button = new button 165 50 100 40 255 100 100 "Sell" in
  let button_list = [buy_button ; sell_button] in

  (* Clock object *)
  let myclock = new clock in


  (* Main loop *)
  while true do

    
    (*limit actions framerate*)
    if ((myclock#get_current_time -. myclock#get_previous_time) > (1000.0 /. 60.0)) then (
      
      (*BEGIN: execute main actions*)
      clear_background ();

      (*button functions*)
      List.iter (fun button -> button#draw) button_list;
      List.iter (fun button -> button#execute) button_list;

      stock_data#get_next_timestamp;

      match stock_data#get_row (stock_data#get_current_timestamp) with
      | Some row -> Printf.printf "EMini value: %f\n" row.emini
      | None -> Printf.printf "No data found for timestamp\n";

      (*END: execute main actions*)

      (*previous time becomes current time*)
      myclock#set_previous_time (myclock#get_current_time)
    );

    (*get new updated time*)
    myclock#update_time;
  done;
  close_graph ()