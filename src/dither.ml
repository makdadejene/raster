open Core

(* This should look familiar by now! *)
let transform image =
  let curr_image = Grayscale.transform image in
  Image.foldi
    curr_image
    ~init:curr_image
    ~f:(fun ~x ~y curr_image (r, _g, _b) ->
    let max = Image.max_val curr_image in
    let ratio = Int.to_float r /. Int.to_float max in
    let error =
      if Float.compare ratio 0.5 > 0
      then (
        Image.set ~x ~y curr_image (max, max, max);
        ratio -. 1.0)
      else (
        Image.set ~x ~y curr_image (0, 0, 0);
        ratio)
    in
    let () =
      match Image.get curr_image ~x:(x + 1) ~y with
      | exception _ -> ()
      | _ ->
        let right =
          Pixel.red (Image.get curr_image ~x:(x + 1) ~y)
          + Float.to_int (error *. (7.0 /. 16.0) *. Int.to_float max)
        in
        Image.set curr_image ~x:(x + 1) ~y (right, right, right)
    in
    let () =
      match Image.get curr_image ~x:(x - 1) ~y:(y + 1) with
      | exception _ -> ()
      | _ ->
        let down_diag =
          Pixel.red (Image.get curr_image ~x:(x - 1) ~y:(y + 1))
          + Float.to_int (error *. (3.0 /. 16.0) *. Int.to_float max)
        in
        Image.set
          curr_image
          ~x:(x - 1)
          ~y:(y + 1)
          (down_diag, down_diag, down_diag)
    in
    let () =
      match Image.get curr_image ~x ~y:(y + 1) with
      | exception _ -> ()
      | _ ->
        let below =
          Pixel.red (Image.get curr_image ~x ~y:(y + 1))
          + Float.to_int (error *. (5.0 /. 16.0) *. Int.to_float max)
        in
        Image.set curr_image ~x ~y:(y + 1) (below, below, below)
    in
    let () =
      match Image.get curr_image ~x:(x + 1) ~y:(y + 1) with
      | exception _ -> ()
      | _ ->
        let down_right =
          Pixel.red (Image.get curr_image ~x:(x + 1) ~y:(y + 1))
          + Float.to_int (error *. (1.0 /. 16.0) *. Int.to_float max)
        in
        Image.set
          curr_image
          ~x:(x + 1)
          ~y:(y + 1)
          (down_right, down_right, down_right)
    in
    curr_image)
;;

let command =
  Command.basic
    ~summary:"Dither an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
