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
  let current_timeframe_tb = new text_box 850 95 100 20 "" in
  current_timeframe_tb#set_justification "right";
  let current_graphed_stock_tb = new text_box 850 700 100 20 "emini" in
  current_graphed_stock_tb#set_justification "right";
  current_graphed_stock_tb#set_background_color 50 50 50;
  let text_box_list = [clock_tb ; after_hours_tb ; current_multiplier_tb ; current_timeframe_tb ; current_graphed_stock_tb] in

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
  let increase_time = new time_change_button 460 735 20 20 255 255 255 "+" myclock true in
  let decrease_time = new time_change_button 430 735 20 20 255 255 255 "-" myclock false in
  let increase_timeline = new change_graph_timeframe 830 95 20 20 255 255 255 "+" main_graph true in
  let decrease_timeline = new change_graph_timeframe 800 95 20 20 255 255 255 "-" main_graph false in
  let emini_gs = new change_graph_stock 955 685 120 40 20 20 20 "Emini: " main_graph "emini" in
  let nasdaq_gs = new change_graph_stock 955 635 120 40 20 20 20  "Nasdaq: " main_graph "nasdaq" in
  let russell_gs = new change_graph_stock 955 585 120 40 20 20 20 "Russell: " main_graph "russell" in
  let spy_gs = new change_graph_stock 955 535 120 40 20 20 20 "SPY: " main_graph "spy" in
  let qqq_gs = new change_graph_stock 955 485 120 40 20 20 20 "QQQ: " main_graph "qqq" in
  let apple_gs = new change_graph_stock 955 435 120 40 20 20 20 "Apple: " main_graph "apple" in
  let microsoft_gs = new change_graph_stock 955 385 120 40 20 20 20 "Microsoft: " main_graph "microsoft" in
  let nvidia_gs = new change_graph_stock 955 335 120 40 20 20 20 "Nvidia: " main_graph "nvidia" in
  let button_list = [buy_button ; sell_button ; increase_time ; decrease_time ; increase_timeline ; decrease_timeline ; emini_gs ; nasdaq_gs ; russell_gs ; spy_gs ; qqq_gs ; apple_gs ; microsoft_gs ; nvidia_gs] in

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
      clock_tb#set_text (stock_data#get_pretty_timestamp (myclock#get_fps) (myclock#get_frame_counter) (myclock#get_seconds_per_second) (myclock#get_cpu_helper));
      after_hours_tb#set_text (stock_data#get_pretty_after_hours);
      current_multiplier_tb#set_text ("1 s = " ^ myclock#get_pretty_seconds_per_second);
      current_timeframe_tb#set_text (main_graph#get_pretty_timeframe);
      current_graphed_stock_tb#set_text (main_graph#get_current_stock);

      (*draw values of stocks on right*)
      List.iter (fun stock_value_viewer -> 
        (
          let stock_name = stock_value_viewer#get_stock_name in
          let currently_up = main_graph#get_currently_up stock_name in
          stock_value_viewer#set_currently_up currently_up;

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
    if (myclock#get_frame_counter >= ((3.0 *. (float_of_int (myclock#get_cpu_helper)) *. (myclock#get_fps)) /. myclock#get_seconds_per_second)) then (

      myclock#set_frame_counter 0.0;

      (*go forward in time*)
      stock_data#get_next_timestamp 3 myclock#get_cpu_helper;

      main_graph#add_timestamp (stock_data#get_current_timestamp);
      List.iter (fun stock_value_viewer -> 
        (
          let price = (stock_data#get_current_price (stock_value_viewer#get_stock_name)) in
          stock_value_viewer#set_current_value price;
          main_graph#set_current_value price (stock_value_viewer#get_stock_name);
          main_graph#add_data (stock_value_viewer#get_stock_name) (stock_data#get_current_timestamp) price;
          )
      ) viewer_list;


    );
      


    (* Get new updated time *)
    myclock#update_time;
  done;
  close_graph ()