open Graphics

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
    current_time <- (Unix.gettimeofday () *. 1000.0);

  method get_current_time = 
    current_time

  method get_previous_time = 
    previous_time

  method set_current_time t = 
    current_time <- t

  method set_previous_time t = 
    previous_time <- t
end
