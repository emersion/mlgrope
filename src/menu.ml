open Graphics
open Util
open Math2d
open Mlgrope
open Frontend

exception PlayLevel of game_state

exception EditLevel of string

type menu =
  { size : vec
  ; levels : string array
  ; selected_index : int
  ; selected_level : game_state
  }

let levels_dir = "levels"

let levels_ext = ".csv"

let arrow_right_img = Image.get (Image.Ppm_file "img/arrow-right.ppm")

let arrow_left_img =
  Image.get (Image.Mirror (Image.Ppm_file "img/arrow-right.ppm"))

let arrow_size = { x = 132.; y = 165. }

let sunglasses_img = Image.get (Image.Ppm_file "img/sunglasses.ppm")

(* Removes suffix from s, if present *)
let strip_suffix suffix s =
  let len = String.length s in
  let suffix_len = String.length suffix in
  if len < suffix_len then
    s
  else if String.sub s (len - suffix_len) suffix_len <> suffix then
    s
  else
    String.sub s 0 (len - suffix_len)

let arrow_corner size dir =
  let x =
    if dir < 0 then
      -0.2 *. arrow_size.x
    else
      size.x -. (0.8 *. arrow_size.x)
  in
  { x; y = 0.5 *. (size.y -. arrow_size.y) }

let collide_arrow size dir pos =
  let corner = arrow_corner size dir in
  Collide.box_point corner (corner +: arrow_size) pos

let draw_arrow size dir =
  let x, y = ints_of_vec (arrow_corner size dir) in
  Graphics.draw_image
    ( if dir < 0 then
      arrow_left_img ()
    else
      arrow_right_img () )
    x y

let draw_level _ name state =
  Frontend.draw state;
  Graphics.set_color Graphics.black;
  Graphics.moveto 0 0;
  Graphics.draw_string ("Level " ^ name)

let draw_sunglasses size pos1 t =
  let pos0 = { pos1 with y = size.y } in
  let corner = mix_vec pos1 pos0 t -: (ball_radius *: vec1) in
  let x, y = ints_of_vec corner in
  Graphics.draw_image (sunglasses_img ()) x y

let step m =
  draw_level m.size m.levels.(m.selected_index) m.selected_level;
  draw_arrow m.size (-1);
  draw_arrow m.size 1;
  m

let level_path name = levels_dir ^ "/" ^ name ^ levels_ext

let load_level name =
  let ch = open_in (level_path name) in
  Level.input ch

let switch_level m delta =
  let selected_index = m.selected_index + delta in
  let selected_index =
    if selected_index < 0 then
      0
    else if selected_index >= Array.length m.levels then
      Array.length m.levels - 1
    else
      selected_index
  in
  let selected_level = load_level m.levels.(selected_index) in
  { m with selected_index; selected_level }

let handle_event m _ s' =
  match s' with
  | { button = true; _ } ->
    let pos = mouse_of_status s' in
    let delta =
      if collide_arrow m.size (-1) pos then
        -1
      else if collide_arrow m.size 1 pos then
        1
      else
        raise (PlayLevel m.selected_level)
    in
    switch_level m delta
  | { keypressed = true; key = 'p'; _ } -> raise (PlayLevel m.selected_level)
  | { keypressed = true; key = 'e'; _ } ->
    raise (EditLevel (level_path m.levels.(m.selected_index)))
  | { keypressed = true; key = '<'; _ } -> switch_level m (-1)
  | { keypressed = true; key = '>'; _ } -> switch_level m 1
  | { keypressed = true; key = '\027'; _ } -> raise Exit
  | _ -> m

let rec run size =
  let files = Sys.readdir levels_dir in
  let levels = Array.map (strip_suffix levels_ext) files in
  Array.sort String.compare levels;
  if Array.length levels = 0 then
    raise (Failure "No level available")
  else
    Printf.printf "Press < or > to select a level, p to play, e to edit\n%!";
  let m =
    { size
    ; levels
    ; selected_index = 0
    ; selected_level = load_level levels.(0)
    }
  in
  try Frontend.run step handle_event size m with
  | PlayLevel state -> (
    try Player.run size state with
    | Player.TouchedGoalException (state, pos) ->
      let t0 = get_time () in
      let step () =
        let dt = get_time () -. t0 in
        let t = min (dt /. 1.) 1. in
        Frontend.draw state;
        draw_sunglasses m.size pos t
      in
      let handle_event () _ s' =
        match s' with
        | { button = true; _ } -> run size
        | { keypressed = true; key = '\027'; _ } -> raise Exit
        | _ -> ()
      in
      Frontend.run step handle_event size ()
    | Player.OutOfBoundsException _ -> run size )
  | EditLevel path -> ( try Editor.run size path with Exit -> run size )
