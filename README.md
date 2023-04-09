# Ocaban

To execute the program:  

```
dune exec Ocaban
```

![Alt text](/assets/sokoban_screenshot.png "Sokoban screenshot")

To play:

* Cursor keys : move the character
* Page Up / Page Down : change level
* Backspace key : undo moves

# How I got this ready for upload

* dune init project Ocaban
* Copied main.ml, levels.ml in the ./bin sub-folder
* added libraries ocaml-canvas and react to the ./bin/dune file
* created ./assets and copied the spritesheet PNG file

# Credits

OCaml language : https://ocaml.org/
ocaml-canvas library : https://ocamlpro.github.io/ocaml-canvas/
Sprites : https://kenney.nl/  
Sokoban levels : https://github.com/begoon/sokoban-maps  
