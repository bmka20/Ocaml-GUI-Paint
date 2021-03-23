(** The main paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES, and PROGRAM STATE   *)
(******************************************)

(** A location in the paint_canvas widget *)
type point = position (* from Gctx *)

type shape = 
  | Line of {color: color; thickness: thickness; p1: point; p2: point}
  | Points of {color : color; thickness: thickness; points: point list}
  | Ellipse of {color: color; thickness: thickness; point: point; rx: int; 
                ry: int}

type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode
  | EllipseStartMode
  | EllipseEndMode of point 

(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;

  mutable preview: shape option; 
  
  mutable thickness : thickness; 
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  preview = None;
  thickness = thin_line;
}

let with_params (g: gctx) (c: color) (t: thickness) : gctx =
  let g = with_thickness (with_color g c) t in
  g


(*********************************)
(**    MAIN CANVAS REPAINTING    *)
(*********************************)

let repaint (g: gctx) : unit =
let cl : color = {r = bgc.br; g = bgc.bg; b = bgc.bb} in 
fill_rect (with_color g cl) (0, 0) (600, 350);
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line l -> 
          draw_line (with_params g l.color l.thickness) l.p1 l.p2
      | Points ps -> 
          draw_points (with_params g ps.color ps.thickness) ps.points
      | Ellipse e -> 
          draw_ellipse (with_params g e.color e.thickness) e.point e.rx e.ry
    end in
  Deque.iterate draw_shape paint.shapes;
  begin match paint.preview with 
    | None -> () 
    | Some s -> draw_shape s 
  end 

(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint
  
let make_ellipse (p1: point) (p2: point) :  point*int*int = 
  let (x1, y1) = p1 in
  let (x2, y2) = p2 in
  let rady = abs (y2 - y1) / 2 in
  let radx = abs (x2 - x1) / 2 in 
  let centr = if x1 < x2 && y1 < y2 then (x1 + radx, y1 + rady)
              else if x1 < x2 && y1 >= y2 then (x1 + radx, y1 - rady)
              else if x1 >= x2 && y1 < y2 then (x1 - radx, y1 + rady)
              else (x1 - radx, y1 - rady) in 
  (centr, radx, rady)

(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
      (begin match paint.mode with 
         | LineStartMode ->
           paint.mode <- LineEndMode p
         | LineEndMode p1 ->
           ()
        | PointMode -> paint.preview <- 
            Some (Points {color=paint.color; thickness=paint.thickness; 
                  points=[p]})
        | EllipseStartMode -> paint.mode <- EllipseEndMode p 
        | EllipseEndMode e -> ()  
       end)
    | MouseDrag ->
       (begin match paint.mode with 
         | LineStartMode -> paint.preview <- None 
         | LineEndMode p1 -> 
             paint.preview <- Some (Line {color=paint.color; 
                                    thickness=paint.thickness; p1=p1; p2=p})
         | PointMode -> 
             let points_list = 
               begin match paint.preview with 
                 | Some (Points ps) -> ps.points
                 | _ -> []
               end in 
               paint.preview <- 
                 Some (Points {color=paint.color; thickness = paint.thickness;
                       points = points_list @ [p]})
         | EllipseStartMode -> paint.preview <- None 
         | EllipseEndMode e -> 
               let (centr, radx, rady) = make_ellipse e p in 
               paint.preview <- Some (Ellipse {color=paint.color; 
                                      thickness=paint.thickness;
                                      point =centr; rx=radx; ry=rady})
        end) 
         
    | MouseUp ->
      (begin match paint.mode with 
         | LineStartMode -> paint.preview <- None 
         | LineEndMode p1 -> 
           Deque.insert_tail
             (Line {color=paint.color; thickness=paint.thickness; 
              p1=p1; p2=p}) paint.shapes;
           paint.mode <- LineStartMode; 
           paint.preview <- None 
         | PointMode -> 
             begin match paint.preview with 
               | Some (Points ps) ->
                   Deque.insert_tail
                     (Points {color=paint.color; thickness=paint.thickness; 
                      points = ps.points}) 
                   paint.shapes; 
                   paint.preview <- None
               | _ -> ()
             end 
         | EllipseStartMode -> paint.preview <- None 
         | EllipseEndMode e -> 
             let (centr, radx, rady) = make_ellipse e p in 
               Deque.insert_tail
                 (Ellipse {color=paint.color; thickness=paint.thickness; 
                  point =centr; rx=radx; ry=rady})
               paint.shapes;
               paint.mode <-EllipseStartMode;
               paint.preview <- None 
       end)

    | _ -> ()
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action


(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"

(** This function runs when the Undo button is clicked.
    It simply removes the last shape from the shapes deque. *)
let undo () : unit =
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes);
    paint.preview <- None

;; nc_undo.add_event_listener (mouseclick_listener undo)

let (w_line, lc_line, nc_line) = button "Line"
;; nc_line.add_event_listener (mouseclick_listener 
   (fun () -> paint.mode <- LineStartMode))
   
let (w_point, lc_point, nc_point) = button "Point"
;; nc_point.add_event_listener (mouseclick_listener
   (fun () -> paint.mode <- PointMode))
   
let (w_ellipse, lc_ellipse, nc_ellipse) = button "Ellipse"
;; nc_ellipse.add_event_listener (mouseclick_listener
   (fun () -> paint.mode <- EllipseStartMode))
   
let make_thiccc (g: gctx) : widget = 
  let (ne, cb) = checkbox false "Change Thickness" in 
  cb.add_change_listener (fun x -> if x then paint.thickness <- thick_juicy_line
                          else paint.thickness <- thin_line);
  ne 

let red_background : widget =
  let (ne, cb) = color_slider 255 "Background Color (R)" Gctx.red in 
  cb.add_change_listener (fun x -> bgc.br <- x);
  ne
  
let green_background : widget =
  let (ne, cb) = color_slider 255 "Background Color (G)" Gctx.green in 
  cb.add_change_listener (fun x -> bgc.bg <- x);
  ne
  
let blue_background : widget =
  let (ne, cb) = color_slider 255 "Background Color (B)" Gctx.blue in 
  cb.add_change_listener (fun x -> bgc.bb <- x);
  ne
  

(** A spacer widget *)
let spacer : widget = space (10,10)


(** The mode toolbar, initially containing just the Undo button. *)
let mode_toolbar : widget = hlist [border w_point; border w_line; 
                                   border w_ellipse; 
                                   border (make_thiccc top_level); spacer;
                                   border w_undo]

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color
   and some buttons for changing it. Both the indicator and the buttons
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
  : widget * notifier_controller =
  let repaint_square (gc:gctx) =
    let c = get_color () in
    fill_rect (with_color gc c) (0, 0) (width-1, width-1) in
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected
    color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created
    with. They are also installed with a mouseclick listener
    that changes the selected color of the paint app to their color. *)
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c ));
  w

(** The color selection toolbar. Contains the color indicator and
    buttons for several different colors. *)
let color_toolbar : widget =
  hpair (hpair color_indicator spacer)
    (hpair (hpair (color_button black) spacer)
       (hpair (hpair (color_button white) spacer)
          (hpair (hpair (color_button red) spacer)
             (hpair (hpair (color_button green) spacer)
                (hpair (hpair (color_button blue) spacer)
                   (hpair (hpair (color_button yellow) spacer)
                      (hpair (hpair (color_button cyan) spacer)
                         (color_button magenta))))))))

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets.
*)
let paint_widget = vlist [paint_canvas; mode_toolbar; spacer; 
                          (hpair (hpair color_toolbar spacer) red_background); 
                          spacer; (hpair (hpair green_background spacer) 
                          blue_background)]


(**************************************)
(**      Start the application        *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
