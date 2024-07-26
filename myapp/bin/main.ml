open Graphics
open App_features

(* Sets the default program dark-gray background *)
let clear_background () = 
  let my_gray = rgb 20 20 20 in
  set_color my_gray;
  fill_rect 0 0 (size_x ()) (size_y ())

let () =
  (* Set up stock data *)
  let path = "/Users/ztgillette/Documents/CS/Personal\ Projects/Other/tradingengine/data/" in
  let current_file = "2020-01" in
  let stock_data = new data path current_file in
  stock_data#init_data;
  stock_data#set_current_timestamp "2020-01-26-17-59-02";

  (* Open a graphics window with double buffering *)
  let width = 1200 in
  let height = 800 in
  open_graph (" " ^ (string_of_int width) ^ "x" ^ (string_of_int height));
  auto_synchronize false;  (* Turn off auto synchronization for manual buffer swapping *)

  (* Set window title *)
  set_window_title "Trading Engine";

  (* Clock object *)
  let myclock = new clock in

  (* Graph object *)
  let main_graph = new stock_graph stock_data in

  (*text boxes*)
  let clock_tb = new text_box 50 735 300 40 "" in
  clock_tb#set_justification "left";
  let after_hours_tb = new text_box 800 735 150 40 "" in
  after_hours_tb#set_justification "right";
  let current_multiplier_tb = new text_box 490 735 100 40 "" in
  current_multiplier_tb#set_justification "left";
  let emini_tb = new text_box 975 685 100 40 "Emini: " in
  let nasdaq_tb = new text_box 975 635 100 40 "Nasdaq: " in
  let russell_tb = new text_box 975 585 100 40 "Russell: " in
  let spy_tb = new text_box 975 535 100 40 "SPY: " in
  let qqq_tb = new text_box 975 485 100 40 "QQQ: " in
  let apple_tb = new text_box 975 435 100 40 "Apple: " in
  let microsoft_tb = new text_box 975 385 100 40 "Microsoft: " in
  let nvidia_tb = new text_box 975 335 100 40 "Nvidia: " in
  let text_box_list = [clock_tb ; after_hours_tb ; current_multiplier_tb ; emini_tb ; nasdaq_tb ; russell_tb ; spy_tb ; qqq_tb ; apple_tb ; microsoft_tb ; nvidia_tb] in

  (* stock value display *)
  let emini_display = new stock_value_viewer "emini" 1075 685 in
  let nasdaq_display = new stock_value_viewer "nasdaq" 1075 635 in
  let russell_display = new stock_value_viewer "russell" 1075 585 in
  let spy_display = new stock_value_viewer "spy" 1075 535 in
  let qqq_display = new stock_value_viewer "qqq" 1075 485 in
  let apple_display = new stock_value_viewer "apple" 1075 435 in
  let microsoft_display = new stock_value_viewer "microsoft" 1075 385 in
  let nvidia_display = new stock_value_viewer "nvidia" 1075 335 in
  let viewer_list = [emini_display ; nasdaq_display ; russell_display ; spy_display ; qqq_display ; apple_display ; microsoft_display ; nvidia_display] in

  (* Button objects *)
  let buy_button = new button 50 50 100 40 100 255 100 "Buy" in
  let sell_button = new button 165 50 100 40 255 100 100 "Sell" in
  let increase_time = new time_change_button 430 735 20 20 255 255 255 "+" myclock true in
  let decrease_time = new time_change_button 460 735 20 20 255 255 255 "-" myclock false in
  let button_list = [buy_button ; sell_button ; increase_time ; decrease_time] in

  (* Main loop *)
  while true do
    (* Limit actions framerate *)
    if ((myclock#get_current_time -. myclock#get_previous_time) > (1000.0 /. myclock#get_fps)) then (
      (* BEGIN: execute main actions *)
      
      (* Clear and draw to the back buffer *)
      clear_background ();

      (* Draw graph *)
      main_graph#draw;

      (*update time on top of screen*)
      clock_tb#set_text (stock_data#get_pretty_timestamp (myclock#get_fps) (myclock#get_frame_counter) (myclock#get_seconds_per_second));
      after_hours_tb#set_text (stock_data#get_pretty_after_hours);
      current_multiplier_tb#set_text ("1 s = " ^ myclock#get_pretty_seconds_per_second);

      (*draw values of stocks on right*)
      List.iter (fun stock_value_viewer -> 
        (
          stock_value_viewer#draw;
          )
      ) viewer_list;
  
      (* Draw buttons *)
      List.iter (fun button -> button#draw) button_list;

      (*draw text boxes*)
      List.iter (fun text_box -> text_box#draw) text_box_list;

      (* Handle button presses *)
      List.iter (fun button -> button#execute) button_list;

      (* Swap buffers *)
      synchronize ();

      (* END: execute main actions *)

      (* Previous time becomes current time *)
      myclock#set_previous_time (myclock#get_current_time);
      myclock#set_frame_counter (myclock#get_frame_counter +. 1.0)
    );

    (*get new stock updates*)
    if (myclock#get_frame_counter >= ((3.0 *. (myclock#get_fps)) /. myclock#get_seconds_per_second)) then (

      myclock#set_frame_counter 0.0;

      (*go forward in time*)
      stock_data#get_next_timestamp 3;

      List.iter (fun stock_value_viewer -> 
        (
          let price = (stock_data#get_current_price (stock_value_viewer#get_stock_name)) in
          stock_value_viewer#set_current_value price;
          )
      ) viewer_list;

    );
      


    (* Get new updated time *)
    myclock#update_time;
  done;
  close_graph ()