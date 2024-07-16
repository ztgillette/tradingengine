open Graphics

let () =
  (* Open a graphics window *)
  open_graph " 400x400";
  
  (* Set the background color to white *)
  set_color white;
  fill_rect 0 0 400 400;

  (* Set the drawing color to red *)
  set_color red;

  (* Draw a red circle *)
  fill_circle 200 200 100;

  (* Wait for a key press *)
  ignore (read_key ());
  
  (* Close the graphics window *)
  close_graph ()
