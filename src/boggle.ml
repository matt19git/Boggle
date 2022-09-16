type tile_rec = {id : string; pos: int * int; letter : char}

type tile_grid = tile_rec list

(** [new grid] is a list of tile records*)
let new_grid = [{id = "first"; pos = (0,0); letter = char_of_int ((Random.int 26) + 65)};
  {id = "sec"; pos = (1,0); letter = char_of_int ((Random.int 26) + 65)};
  {id = "third"; pos = (2,0); letter = char_of_int ((Random.int 26) + 65)};
  {id = "fourth"; pos = (3,0); letter = char_of_int ((Random.int 26) + 65)};
  {id = "fifth"; pos = (0,1); letter = char_of_int ((Random.int 26) + 65)};
  {id = "sixth"; pos = (1,1); letter = char_of_int ((Random.int 26) + 65)};
  {id = "seventh"; pos = (2,1); letter = char_of_int ((Random.int 26) + 65)};
  {id = "eighth"; pos = (3,1); letter = char_of_int ((Random.int 26) + 65)};
  {id = "ninth"; pos = (0,2); letter = char_of_int ((Random.int 26) + 65)};
  {id = "tenth"; pos = (1,2); letter = char_of_int ((Random.int 26) + 65)};
  {id = "eleventh"; pos = (2,2); letter = char_of_int ((Random.int 26) + 65)};
  {id = "twelfth"; pos = (3,2); letter = char_of_int ((Random.int 26) + 65)};
  {id = "thirteenth"; pos = (0,3); letter = char_of_int ((Random.int 26) + 65)};
  {id = "fourteenth"; pos = (1,3); letter = char_of_int ((Random.int 26) + 65)};
  {id = "fifteenth"; pos = (2,3); letter = char_of_int ((Random.int 26) + 65)};
  {id = "sixteenth"; pos = (3,3); letter = char_of_int ((Random.int 26) + 65)};
  
  ]