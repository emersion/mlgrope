open Math2d

let size = { x = 800.; y = 800. }

let play path =
  let ch = open_in path in
  let gs = Level.input ch in
  Player.run size gs

let edit path = Editor.run size path

let menu () = Menu.run size

let () =
  Arg.parse
    [ ("-play", Arg.String play, "Play a level")
    ; ("-edit", Arg.String edit, "Edit a level")
    ]
    play "Usage: mlgrope [-play|-edit] level.csv";
  menu ()
