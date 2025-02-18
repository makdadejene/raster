open! Core

let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let masking = 3 in
    let red = Int.shift_left (masking land r) 6 in
    let green = Int.shift_left (masking land g) 6 in
    let blue = Int.shift_left (masking land b) 6 in
    red, green, blue)
;;

(* r%4*64 *)

(* - need to go from int to binary for (r,g,b) - zero first 6 - Int.leftshift
   6 - convert back to numbers and pass as (r,g.b)*)

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mystery.ppm")]
;;
