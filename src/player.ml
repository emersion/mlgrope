open Graphics
open Util
open Math2d
open Mlgrope

exception TouchedGoalException of game_state * vec

exception OutOfBoundsException of game_state

type game =
  { size : vec
  ; time : float
  ; paused : bool
  ; score : int
  ; state : game_state
  }

let star_score = 100

(* Adds zeros before a number n to reach padding digits *)
let pad_int padding n =
  let s = string_of_int n in
  let len = String.length s in
  let start_at = padding - len in
  if start_at < 0 then
    s
  else
    String.init padding (fun i ->
      if i >= start_at then
        s.[i - start_at]
      else
        '0')

let draw_score size score =
  let x, y = ints_of_vec size in
  let s = pad_int 5 score in
  Graphics.set_color Graphics.black;
  let w, h = Graphics.text_size s in
  Graphics.moveto (x - w) (y - h);
  Graphics.draw_string s

let is_bubble_at pos ball e =
  match e with
  | Bubble bubble -> Collide.circle_point ball.position bubble.radius pos
  | _ -> false

let is_rope_at lastpos pos ball e =
  match e with
  | Rope { position; _ }
   |Elastic { position; _ } ->
    is_some (Collide.segments position ball.position lastpos pos)
  | _ -> false

let is_in_bounds size b =
  let has_bubble, has_rope =
    List.fold_left
      (fun (has_bubble, has_rope) e ->
        match e with
        | Bubble _ -> (true, has_rope)
        | Rope _
         |Elastic _ ->
          (has_bubble, true)
        | _ -> (has_bubble, has_rope))
      (false, false) b.links
  in
  if has_bubble && b.position.y <= 0. then
    true
  else if has_rope then
    true
  else
    Collide.box_point vec0 size b.position

let compute_score gs =
  List.fold_left
    (fun (score, gs) e ->
      match e with
      | Ball b ->
        let score, links =
          List.fold_left
            (fun (score, links) e ->
              match e with
              | Star _ -> (score + star_score, links)
              | _ -> (score, e :: links))
            (score, []) b.links
        in
        (score, Ball { b with links } :: gs)
      | _ -> (score, e :: gs))
    (0, []) gs

let step g =
  let t = get_time () in
  let dt = t -. g.time in
  let state =
    if g.paused then
      g.state
    else
      try Backend.move g.state dt
      with Backend.TouchedGoalException pos ->
        raise (TouchedGoalException (g.state, pos))
  in
  let state, in_bounds =
    List.fold_left
      (fun (l, n) e ->
        match e with
        | Ball b ->
          if is_in_bounds g.size b then
            (e :: l, n + 1)
          else
            (l, n)
        | _ -> (e :: l, n))
      ([], 0) state
  in
  if in_bounds = 0 then
    raise (OutOfBoundsException state)
  else
    let score, state = compute_score state in
    let g = { g with time = t; score = g.score + score; state } in
    Frontend.draw g.state;
    draw_score g.size g.score;
    g

let handle_click ball lastpos pos =
  let is_bubble = is_bubble_at pos ball in
  let is_rope = is_rope_at lastpos pos ball in
  { ball with
    links =
      List.filter (fun e -> (not (is_bubble e)) && not (is_rope e)) ball.links
  }

let handle_event g s s' =
  match s' with
  | { button = true; _ } ->
    let pos = Frontend.mouse_of_status s' in
    let lastpos =
      match s with
      | { button = true; mouse_x; mouse_y; _ } ->
        { x = float_of_int mouse_x; y = float_of_int mouse_y }
      | _ -> pos
    in
    let state =
      List.map
        (fun e ->
          match e with
          | Ball b -> Ball (handle_click b lastpos pos)
          | _ -> e)
        g.state
    in
    { g with state }
  | { keypressed = true; key = '\027'; _ } -> raise Exit
  | { keypressed = true; key = ' '; _ } -> { g with paused = not g.paused }
  | { keypressed = true; key = 'f'; _ } ->
    let g = step { g with paused = false } in
    { g with paused = true }
  | _ -> g

let run size state =
  let g = { size; time = get_time (); paused = false; score = 0; state } in
  Printf.printf "Press Esc to quit, space to pause\n%!";
  Frontend.run step handle_event size g
