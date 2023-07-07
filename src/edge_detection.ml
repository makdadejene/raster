(* open Core

let transform image radius= 

let g_x = [-1, 0, 1, -2, 0, 2, -1, 0, 1] in
let g_y = [-1, -2, -1, 0, 0, 0, 1, 2, 1] in

Image.foldi image ~init:image ~f:(fun ~x ~y image (r,g,b) ->
  let x_start = if x - radius < 0 then 0 else x - radius in
  let x_end =
    if x + radius > Image.width image
    then Image.width image
    else x + radius
  in
  let y_start = if y - radius < 0 then 0 else y - radius in
  let y_end =
    if y + radius > Image.height image
    then Image.height image
    else y + radius
  in 
  let rad = Image.slice image ~x_start ~x_end ~y_start ~y_end in

  

  (* Array.iter2_exn rad g_x ~f:(fun rad (r,g,b) ->  ) *)



  )



;;


(* - slice 3x3 from the image - create variables for each kernel value - then
   multiply kernel value by the actual position and sum -calculate final
   gradient - then if gradient exceeds thershold (maybe 0.5) then it is black
   if not white *)

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
;; *)
