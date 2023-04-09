# Ocaban

To execute the program:  

```
dune exec Ocaban
```
# Credits

OCaml language : https://ocaml.org/

ocaml-canvas library : https://ocamlpro.github.io/ocaml-canvas/

Sprites : https://kenney.nl/  

Sokoban levels : https://github.com/begoon/sokoban-maps  

# How I got this ready for upload

* dune init project Ocaban
* dune exec Ocaban works as expected
* Copied main.ml, level.ml in the ./bin sub-folder
* added ocaml-canvas and react to ./bin/dune file
* created ./assets and copied the PNG file in there
