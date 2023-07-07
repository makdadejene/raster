open! Core

let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let threshold = Float.to_int (0.3 *. Int.to_float Int.max_value) in
    let red = if r > threshold then Int.max_value - r else r in
    let green = if g > threshold then Int.max_value - r else r in
    let blue = if b > threshold then Int.max_value - r else r in
    red, green, blue)
;;

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
