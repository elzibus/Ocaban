(*
 * Simple sokoban game
 *)

open OcamlCanvas.V1 ;;

(*
 *  -- Format of the compressed levels ( RLE style )
 *  -- Prolog
 *         char size_x
 *         char size_y
 *  -- Elements
 *         counter (bits)
 *                 0                - 1 symbol
 *                 1 D3 D2 D1       - 2+D3*4+D2*2+D1 symbols (9 max)
 *         char (bits)
 *                 0 0              - an empty space              -> 0
 *                 0 1              - the wall                    -> 1
 *                 1 0              - the box                     -> 2
 *                 1 1 1            - the box already in place    -> 3
 *                 1 1 0            - the goal for a box          -> 4
 *  -- Epilog
 *         char man_x
 *         char man_y
 *)

type tile = Empty
          | Wall
          | Box
          | Placed_box
          | Goal
          | P_up
          | P_down
          | P_right
          | P_left ;;

(* grid2d
 * origin is top, left
 * bottom right tile is gri2d.(width-1).(height-1)
 *)

type level = {
    width : int ;
    height : int ;
    player_x : int ;
    player_y : int ;
    grid2d : tile array array ;
  } ;;

(*
# byte_to_booleans 11 ;;
- : bool array = [|false; false; false; false; false; true; false; true|]
 *)

let byte_to_booleans b =
  let ones = Array.make 8 1 in
  let shifted = Array.mapi ( fun i b -> Int.shift_left b (7-i)) ones in
  Array.map ( fun elt -> Int.logand elt b > 0) shifted ;;

let decompress_level lvl =
  let level_data = List.nth Levels.levels lvl in
  let length = Array.length level_data in
  let width = level_data.(0) in
  let height = level_data.(1) in
  let player_x = level_data.(length-2) in
  let player_y = level_data.(length-1) in
  let temp = Array.map byte_to_booleans (Array.sub level_data 2 (length-4)) in
  let bits = Array.(concat (to_list temp)) in
  let index = ref 0 in
  let i = ref 0 in
  let counter = ref 0 in
  let tile = ref Empty in
  let grid1d = Array.make (width*height) Empty in
  while !index < width*height do
    if not bits.(!i) then
      begin
        counter := 1 ;
        i := !i + 1
      end
    else
      begin
        counter := 2 ;
        if bits.(!i+1) then counter := !counter + 4 ;
        if bits.(!i+2) then counter := !counter + 2 ;
        if bits.(!i+3) then counter := !counter + 1 ;
        i := !i + 4
      end ;
    if not bits.(!i) then
      begin
        if not bits.(!i+1) then tile := Empty else tile := Wall ;
        i := !i + 2
      end
    else
      begin
        if not bits.(!i+1) then
          begin
            tile := Box ;
            i := !i + 2
          end
        else
          begin
            if bits.(!i+2) then tile := Placed_box else tile := Goal ;
            i := !i + 3
          end
      end ;
    Array.fill grid1d !index !counter !tile ;
    index := !index + !counter ;
  done ;
  let grid2d = Array.make_matrix width height Empty in
  for j = 0 to height-1 do
    for i = 0 to width-1 do
      grid2d.(i).(j) <- grid1d.(j*width+i)
    done ;
  done ;
  { width = width ;
    height = height ;
    player_x = player_x ;
    player_y = player_y ;
    grid2d = grid2d
  } ;;
   
let maxLevels = List.length Levels.levels ;;

let win_width = 1600/2
let win_height = 900/2
               
let events = ref []
let retain_event e = events := e :: !events
let clear_events () = events := []

let sprite_sheet = ref None

type move = Up
          | Down
          | Right
          | Left ;;

type state = {
    mutable width : int ;
    mutable height : int ;
    mutable player_x : int ;
    mutable player_y : int ;
    mutable player_sprite : tile ;
    mutable factor : float ;
    mutable ofsx : float ;
    mutable ofsy : float ;
    mutable curLev : int ;
    mutable grid2d : tile array array ;
    mutable moves : move Queue.t ;
    mutable has_won : bool ;
  } ;;

let state = {
    curLev = 0 ;
    width = 0 ;
    height = 0 ;
    factor = 1.0 ;
    ofsx = 0.0 ;
    ofsy = 0.0 ;
    player_x = 0 ;
    player_y = 0 ;
    player_sprite = P_down ;
    grid2d = [| [| Empty |] |] ;
    moves = Queue.create () ;
    has_won = false ;
  } ;;

let window_title = "Sokoban"

let get_tilenum tile =
  match tile with
  | Empty -> if state.has_won then 88 else 89
  | Wall -> 98
  | Box -> 6
  | Placed_box -> 9
  | Goal -> 102
  | P_up -> if state.has_won then 72 else 68
  | P_down -> if state.has_won then 72 else 65
  | P_right -> if state.has_won then 72 else 91
  | P_left -> if state.has_won then 72 else 94 ;;

let draw_sprite c ss x y tile =
  let tilenum = get_tilenum tile in
  let tx = tilenum mod 13 in
  let ty = tilenum / 13 in
  Canvas.blit ~dst:c ~dpos:(x*64, y*64) ~src:ss ~spos:(tx*64, ty*64) ~size:(64,64);;

let clr_screen c =
  let grey = Color.of_argb 255 117 140 142 in
  Canvas.setFillColor c grey ;
  let (winW, winH) = Canvas.getSize c in
  Canvas.fillRect c ~pos:(0.0, 0.0) ~size:(float_of_int winW, float_of_int winH) ;;

let load_current_level () =
  let lvl =  decompress_level state.curLev in
  state.width <- lvl.width ;
  state.height <- lvl.height ;
  state.player_x <- lvl.player_x ;
  state.player_y <- lvl.player_y ;
  state.player_sprite <- P_down ;
  state.grid2d <- lvl.grid2d ;;

let adjust_to_window c =
  let (winW, winH) = Canvas.getSize c in 
  let w64 = state.width * 64 in
  let h64 = state.height * 64 in
  let tempw = float_of_int winW /. float_of_int w64 in
  let temph = float_of_int winH /. float_of_int h64 in
  state.factor <- min tempw temph ;
  state.ofsx <- max 0. ((float_of_int winW -. state.factor *. float_of_int w64) /. 2.0) ;
  state.ofsy <- max 0. ((float_of_int winH -. state.factor *. float_of_int h64) /. 2.0) ;
  Canvas.restore c ;
  clr_screen c ;
  Canvas.save c ;
  Canvas.translate c (state.ofsx, state.ofsy) ;
  Canvas.scale c (state.factor, state.factor) ;;

let do_move (dx, dy) =
  let moveOnce = state.grid2d.(state.player_x + dx).(state.player_y + dy) in
  if List.mem moveOnce [Empty; Goal] then
    begin
      state.player_x <- state.player_x + dx ;
      state.player_y <- state.player_y + dy
    end
  else if List.mem moveOnce [Box; Placed_box] then
    begin
      let moveTwice = state.grid2d.(state.player_x+2*dx).(state.player_y+2*dy) in
      let saveTile = if moveOnce == Placed_box then Goal else Empty in
      if moveTwice == Empty then
        begin
          state.grid2d.(state.player_x + dx).(state.player_y + dy) <- saveTile ;
          state.grid2d.(state.player_x + 2*dx).(state.player_y + 2*dy) <- Box ;
          state.player_x <- state.player_x + dx ;
          state.player_y <- state.player_y + dy
        end
      else if moveTwice == Goal then
        begin
          state.grid2d.(state.player_x + dx).(state.player_y + dy) <- saveTile ;
          state.grid2d.(state.player_x + 2*dx).(state.player_y + 2*dy) <- Placed_box ;
          state.player_x <- state.player_x + dx ;
          state.player_y <- state.player_y + dy
        end 
    end ;;

let has_won () =
  let n_boxes = ref 0 in
  for i = 0 to state.width-1 do
    for j = 0 to state.height-1 do
      if state.grid2d.(i).(j) == Box then n_boxes := !n_boxes + 1
    done ;
  done ;
  !n_boxes == 0 ;;

let handle_move direction =
  if not state.has_won then
    begin
      match direction with
      | Right -> do_move(1, 0)
      | Left -> do_move(-1, 0)
      | Up -> do_move(0, -1)
      | Down -> do_move(0, 1)
    end ;
  state.has_won <- has_won ()

let get_player_tile_from_dir dir =
  match dir with
  | Up -> P_up
  | Down -> P_down
  | Left -> P_left
  | Right -> P_right ;;

let undo_move c =
  if not ( Queue.is_empty state.moves) then
    begin
      load_current_level () ;
      adjust_to_window c ;
      let moves = Queue.create () in
      for _ = 1 to (Queue.length state.moves) - 1 do
        let move = Queue.pop state.moves in
        Queue.add move moves ;
        handle_move move ;
        state.player_sprite <- get_player_tile_from_dir move 
      done ;
      state.moves <- moves
end ;;

let () =
  Backend.init ();
  let c = Canvas.createOnscreen ~title:window_title ~pos:(300, 200) ~size:(win_width, win_height) () in
  Canvas.setFillColor c Color.black ;
  Canvas.setFont c "Liberation Sans" ~size:20.0 ~slant:Font.Roman ~weight:Font.bold ;
  Canvas.save c ;
  Canvas.show c ;
  load_current_level () ;
  adjust_to_window c ;

  let event_sprite_sheet =
    Canvas.createOffscreenFromPNG "./assets/sokoban_tilesheet.png" in
  retain_event @@
    React.E.map (fun sprite ->
        sprite_sheet := Some (sprite)
      ) event_sprite_sheet;
  
  retain_event @@
    React.E.map (fun _ ->

        clr_screen c ;
        
        match !sprite_sheet with
        | None ->
           ()
        | Some (ss) ->
           begin
                 
             for i = 0 to state.width-1 do
               for j = 0 to state.height-1 do
                 draw_sprite c ss i j state.grid2d.(i).(j) ;
               done ;
             done ;
             
           end ;
           
           draw_sprite c ss state.player_x state.player_y state.player_sprite ;

      ) Event.frame;

  (* re-calculate how to display the level when the window is resized *)
  retain_event @@
    React.E.map ( fun { Event.canvas = _; timestamp = _; data = (_, _) } ->
                  adjust_to_window c ;
      ) Event.resize ;
  
  retain_event @@
    React.E.map (fun { Event.canvas = _; timestamp = _; data = () } ->
        Backend.stop ()
      ) Event.close ;
    
  retain_event @@
    React.E.map (fun { Event.canvas = _; timestamp = _;
                       data = { Event.key; char = _; flags = _ } } ->
        let change_level = ref false in
        if key = KeyEscape then
          Backend.stop ()
        else if key = KeyPageUp then
          begin
            state.curLev <- state.curLev + 1 ;
            change_level := true
          end
        else if key = KeyPageDown then
          begin
            state.curLev <- state.curLev - 1 ;
            change_level := true
          end
        else if key = KeyRightArrow && not state.has_won then
          begin
            handle_move Right ;
            Queue.add Right state.moves ;
            state.player_sprite <- P_right
          end
        else if key = KeyLeftArrow && not state.has_won then
          begin
            handle_move Left ;
            Queue.add Left state.moves ;
            state.player_sprite <- P_left
          end
        else if key = KeyUpArrow && not state.has_won then
          begin
            handle_move Up ;
            Queue.add Up state.moves ;
            state.player_sprite <- P_up
          end
        else if key = KeyDownArrow && not state.has_won then
          begin
            handle_move Down ;
            Queue.add Down state.moves ;
            state.player_sprite <- P_down
          end
        else if key = KeyBackspace && not state.has_won then
          begin
            undo_move c ;
          end ;
        if !change_level then
          begin
            state.curLev <- (maxLevels + state.curLev) mod maxLevels ;
            load_current_level () ;
            adjust_to_window c ;
            state.moves <- Queue.create () ;
            state.has_won <- false
          end
      ) Event.key_down ;
  
  Backend.run (fun () ->
      clear_events ()) ;
