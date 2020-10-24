(* determine if a particular subsequence of two arrays are equal *)
let rec subseq_eq a1 a2 start len =
  if len = 0 then true else
  if a1.(start) <> a2.(start) then false else
    subseq_eq a1 a2 (start + 1) (len - 1)

type tile =
  | Upstair
  | Downstair
  | Wall
  | Blank

module type Room_type = sig
  type t
  val width: int
  val height: int

  val get: int -> int -> t -> tile option
  val upstair:   t -> int * int
  val downstair: t -> int * int

  val gen: Random.State.t -> t
end

module Room: Room_type = struct
  type t = {
    tiles: tile array;
    upstair: int * int;
    downstair: int * int;
  }
  let width = 30
  let height = 15

  let get x y room =
    if x < 0 || width <= x || y < 0 || height <= y
    then None
    else Some (room.tiles.(y * width + x))
  let upstair { upstair } = upstair
  let downstair { downstair } = downstair

  let gen rng = 
    (* generate a position in the room *)
    let gen_pos () =
      let open Random.State in
      (int rng (width - 2) + 1, int rng (height - 2) + 1)
    in

    let upstair = gen_pos () in

    (* generate the downstair, ensuring it is distinct *)
    let rec gen_downstair () =
      let pos = gen_pos () in
      if pos = upstair
      then gen_downstair ()
      else pos
    in
    let downstair = gen_downstair () in

    (* generate each tile *)
    let tiles = Array.init (width * height) (fun idx ->
        let x = idx mod width in
        let y = idx / width in
        if x = 0 || y = 0 || x + 1 = width || y + 1 = height
        then Wall else
          let pos = (x, y) in
          if pos = upstair then Upstair else
          if pos = downstair then Downstair else
          if Random.State.int rng 5 = 0 then Wall else
            Blank
      )
    in { tiles; upstair; downstair }
end

(* represents the physical world *)
type world = {
  state: Random.State.t;
  mutable room: Room.t;
  mutable prev_rooms: Room.t list;
  mutable next_rooms: Room.t list;
}

(* initialize the world *)
let init_world () =
  let state = Random.State.make_self_init () in
  {
    room = Room.gen state;
    prev_rooms = [];
    next_rooms = [];
    state;
  }

(* represents the game state *)
type game_state = {
  world: world;
  mutable px: int;
  mutable py: int;
}

(* initialize the game state *)
let init_state () =
  let world = init_world () in
  let (px, py) = Room.upstair world.room in
  { world; px; py }

(* represents an entity *)
type entity = | Player

(* a map with points as keys *)
module PosMap = Map.Make (struct
    type t = int * int
    let compare = compare
  end)

(* represents a command sent by the user *)
type command =
  | Quit
  | Move of int * int
  | Up
  | Down

(* respond to a command *)
let handle_command state = function
  | Quit -> false
  | Move (dx, dy) ->
    let new_px = state.px + dx in
    let new_py = state.py + dy in
    (match Room.get new_px new_py state.world.room with
     | Some Wall | None -> ()
     | _ ->
       state.px <- new_px;
       state.py <- new_py);
    true
  | Up ->
    let world = state.world in
    (* is the player standing on the upstair? *)
    (match Room.get state.px state.py world.room with
     | Some Upstair ->
       (match world.prev_rooms with
        | [] -> false (* escaped *)
        | r::rs ->
          world.next_rooms <- world.room :: world.next_rooms;
          world.room <- r;
          world.prev_rooms <- rs;
          let (new_px, new_py) = Room.downstair world.room in
          state.px <- new_px;
          state.py <- new_py;
          true)
     | _ -> true)
  | Down ->
    let world = state.world in
    (* is the player standing on the downstair? *)
    (match Room.get state.px state.py world.room with
     | Some Downstair ->
       world.prev_rooms <- world.room :: world.prev_rooms;
       world.room <-
         (match world.next_rooms with
          | r::rs -> world.next_rooms <- rs; r
            (* if the next level does not yet exist, generate it *)
          | [] -> Room.gen world.state);
       let (new_px, new_py) = Room.upstair world.room in
       state.px <- new_px;
       state.py <- new_py;
       true
     | _ -> true)

(* call `f` and then `final`, even if `f` errors *)
let finalize f final =
  let result =
    try Result.Ok (f ())
    with e -> Result.Error e
  in
  final ();
  match result with
  | Ok a -> a
  | Error e -> raise e

let with_raw_stdout f =
  (* save existing terminal attributes *)
  let old_io = Unix.tcgetattr Unix.stdout in

  (* set raw terminal attributes *)
  Unix.tcsetattr Unix.stdout Unix.TCSANOW {
    old_io with
    c_ignbrk = false;
    c_brkint = false;
    c_parmrk = false;
    c_istrip = false;
    c_inlcr = false;
    c_igncr = false;
    c_icrnl = false;
    c_ixon = false;
    c_opost = false;
    c_echo = false;
    c_echonl = false;
    c_icanon = false;
    c_isig = false;
    c_parenb = false;
    (* we would set `c_iexten = false;` if it existed *)
    c_csize = 8;
  };

  finalize f (fun () ->
      (* restore original terminal attributes *)
      Unix.tcsetattr Unix.stdout Unix.TCSANOW old_io)

let csi s = "\x1b[" ^ s

let go_to chan x y =
  output_string chan (csi "");
  Printf.fprintf chan "%d;%dH" (y + 1) (x + 1)

let with_alt_screen chan f =
  (* switch to alternate screen *)
  output_string chan (csi "?1049h");

  finalize f (fun () ->
      (* restore main screen *)
      output_string chan (csi "?1049l"))

module type Screen_type = sig
  type state
  val make: unit -> state
  val render: entity PosMap.t -> Room.t -> state -> out_channel -> unit
end

module Screen: Screen_type = struct
  (* `None` represents the lack of a previous state *)
  type old_state = {
    tiles: char array;
    cursor_x: int;
    cursor_y: int;
  }
  type state = old_state option ref
  let make () = ref None
  let render entities room old_state chan =
    (* get the list of characters to display *)
    let new_tiles = Array.init Room.(width * height) (fun i ->
        let x = i mod Room.width in
        let y = i / Room.width in
        match PosMap.find_opt (x, y) entities with
        | Some Player -> '@'
        | None -> match Room.get x y room |> Option.get with
          | Upstair   -> '<'
          | Downstair -> '>'
          | Blank     -> ' '
          | Wall      ->
            let is_wall dx dy =
              match Room.get (x + dx) (y + dy) room with
              | Some Wall -> true
              | _         -> false
            in
            let hori = is_wall 1 0 || is_wall (-1) 0 in
            let vert = is_wall 0 1 || is_wall 0 (-1) in
            if vert && not hori then '|' else '-'
      )
    in

    let old_tiles_opt = !old_state |> Option.map (fun s -> s.tiles) in

    (* redraw every line with idx >= n if it is different *)
    let rec redraw ~cursor_x ~cursor_y n =
      if n < Room.height then (
        (* put the cursor in the correct location *)
        if n = cursor_y + 1
        then output_string chan "\r\n"
        else go_to chan 0 n;
        (* determine if the line needs to be redrawn *)
        let do_redraw =
          match old_tiles_opt with
          | None -> true
          | Some tiles -> not (
              subseq_eq tiles new_tiles (n * Room.width) Room.width)
        in
        (* compute the new position of the cursor *)
        let cursor_y = n in
        let cursor_x =
          if do_redraw then
            let row = String.init Room.width (fun i ->
                new_tiles.(n * Room.width + i)) in
            print_string row;
            Room.width
          else 0
        in
        redraw ~cursor_x ~cursor_y (n + 1))
    in

    (match !old_state with
     | Some { cursor_x; cursor_y } ->
       redraw cursor_x cursor_y 0;
     | None ->
       go_to chan 0 0;
       redraw 0 0 0);

    (* place the cursor on an arbitrary entity
     * TODO: ensure it is the player *)
    let ((player_x, player_y), _) = PosMap.min_binding entities in
    go_to chan player_x player_y;
    flush chan;

    (* update the state *)
    old_state := Some {
        tiles = new_tiles;
        cursor_x = player_x;
        cursor_y = player_y;
      }
end

let game_loop in_chan out_chan =
  let state = init_state () in
  let screen = Screen.make () in
  let rec loop () =
    let entities = PosMap.singleton (state.px, state.py) Player in
    Screen.render entities state.world.room screen out_chan;
    let c =
      try Some (input_char in_chan)
      with End_of_file -> None
    in
    let cmd =
      match c with
      | None -> Some Quit
      | Some c -> match c with
        | 'y' -> Some (Move (-1, -1))
        | 'u' -> Some (Move (1, -1))
        | 'h' -> Some (Move (-1, 0))
        | 'j' -> Some (Move (0, 1))
        | 'k' -> Some (Move (0, -1))
        | 'l' -> Some (Move (1, 0))
        | 'b' -> Some (Move (-1, 1))
        | 'n' -> Some (Move (1, 1))
        | '<' -> Some Up
        | '>' -> Some Down
        (* ^C or ^D *)
        | '\x03' | '\x04' -> Some Quit
      | _ -> None
    in
    match cmd with
    | None -> loop ()
    | Some cmd ->
      if handle_command state cmd
      then loop ()
      else ()
  in
  loop ()

let main () =
  with_raw_stdout (fun () ->
      with_alt_screen stdout (fun () ->
          game_loop stdin stdout));
  print_endline "You left the dungeon."

let () = main ()
