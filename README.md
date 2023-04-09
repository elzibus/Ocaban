# Ocaban

To execute the program:  

```
dune exec Ocaban
```

![Alt text](/assets/sokoban_screenshot.png "Sokoban screenshot")

To play:

* cursor keys move the character
* Page Up / Page Down to change the level
* Backspace to undo moves

# How I got this ready for upload

* dune init project Ocaban
* dune exec Ocaban works as expected
* Copied main.ml, level.ml in the ./bin sub-folder
* added ocaml-canvas and react to ./bin/dune file
* created ./assets and copied the PNG file in there

# Credits

OCaml language : https://ocaml.org/
ocaml-canvas library : https://ocamlpro.github.io/ocaml-canvas/
Sprites : https://kenney.nl/  
Sokoban levels : https://github.com/begoon/sokoban-maps  
