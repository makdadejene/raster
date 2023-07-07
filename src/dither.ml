open Core

let transform_helper
  image
  x_position
  y_position
  fraction
  max
  r_error
  g_error
  b_error
  =
  match Image.get image ~x:x_position ~y:y_position with
  | exception _ -> ()
  | _ ->
    let r_right =
      Pixel.red (Image.get image ~x:x_position ~y:y_position)
      + Float.to_int (r_error *. fraction *. Int.to_float max)
    in
    let b_right =
      Pixel.blue (Image.get image ~x:x_position ~y:y_position)
      + Float.to_int (b_error *. fraction *. Int.to_float max)
    in
    let g_right =
      Pixel.green (Image.get image ~x:x_position ~y:y_position)
      + Float.to_int (g_error *. fraction *. Int.to_float max)
    in
    Image.set image ~x:x_position ~y:y_position (r_right, g_right, b_right)
;;

(* This should look familiar by now! *)

let transform image =
  Image.foldi image ~init:image ~f:(fun ~x ~y image (r, g, b) ->
    let max = Image.max_val image in
    let r_ratio = Int.to_float r /. Int.to_float max in
    let b_ratio = Int.to_float b /. Int.to_float max in
    let g_ratio = Int.to_float g /. Int.to_float max in
    let seven = 7.0 /. 16.0 in
    let five = 5.0 /. 16.0 in
    let three = 3.0 /. 16.0 in
    let one = 1.0 /. 16.0 in
    let r_val, r_error =
      (* if Float.compare ratio *)
      if Float.compare r_ratio 0.5 > 0
      then max, r_ratio -. 1.0
      else 0, r_ratio
    in
    let b_val, b_error =
      (* if Float.compare ratio *)
      if Float.compare b_ratio 0.5 > 0
      then max, b_ratio -. 1.0
      else 0, b_ratio
    in
    let g_val, g_error =
      (* if Float.compare ratio *)
      if Float.compare g_ratio 0.5 > 0
      then max, g_ratio -. 1.0
      else 0, g_ratio
    in
    Image.set ~x ~y image (r_val, g_val, b_val);
    let () =
      transform_helper image (x + 1) y seven max r_error g_error b_error
    in
    let () =
      transform_helper
        image
        (x - 1)
        (y + 1)
        three
        max
        r_error
        g_error
        b_error
    in
    let () =
      transform_helper image x (y + 1) five max r_error g_error b_error
    in
    let () =
      transform_helper image (x + 1) (y + 1) one max r_error g_error b_error
    in
    image)
;;

(* match Image.get image ~x:(x + 1) ~y with | exception _ -> () | _ -> let
   right = Pixel.red (Image.get image ~x:(x + 1) ~y) + Float.to_int (error *.
   (7.0 /. 16.0) *. Int.to_float max) in Image.set image ~x:(x + 1) ~y
   (right, right, right) in *)

(* let () = match Image.get image ~x:(x - 1) ~y:(y + 1) with | exception _ ->
   () | _ -> let down_diag = Pixel.red (Image.get image ~x:(x - 1) ~y:(y +
   1)) + Float.to_int (error *. (3.0 /. 16.0) *. Int.to_float max) in
   Image.set image ~x:(x - 1) ~y:(y + 1) (down_diag, down_diag, down_diag)
   in *)
(* let () = match Image.get image ~x ~y:(y + 1) with | exception _ -> () | _
   -> let below = Pixel.red (Image.get curr_image ~x ~y:(y + 1)) +
   Float.to_int (error *. (5.0 /. 16.0) *. Int.to_float max) in Image.set
   curr_image ~x ~y:(y + 1) (below, below, below) in *)
(* let () = match Image.get curr_image ~x:(x + 1) ~y:(y + 1) with | exception
   _ -> () | _ -> let down_right = Pixel.red (Image.get curr_image ~x:(x + 1)
   ~y:(y + 1)) + Float.to_int (error *. (1.0 /. 16.0) *. Int.to_float max) in
   Image.set curr_image ~x:(x + 1) ~y:(y + 1) (down_right, down_right,
   down_right) in *)

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
