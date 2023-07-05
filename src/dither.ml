open Core

(* This should look familiar by now! *)
let transform image = 


Image.foldi image ~init:image ~f:(fun ~x ~y (r,g,b) ->
  let max = Image.max_val image in
 let val = (Int.of_float r)/.(Int.of_float max) in
 if(val > 0.5 ) then 
 (let error = Int.of_float val-1.0)
 val = 1.0 
else 
  (let error = Int.of_float val-0.5)
 val = 0.5

 let seven = Int.of_float error*(7/16) in
 let three = Int.of_float error*(3/16) in
 let five = Int.of_float error*(5/16) in
 let one = Int.of_float error*(1/16) in

  let right = Image.set ~x: x + 1 ~y:y image (seven, seven, seven) 
  let diag_left = Image.set ~x: x -1 ~y:y-1 image (three, three, three)
  let bottom = Image.set ~x: x ~y;y-1 image (five, five, five)
  let diag_right = Image.set ~x:x+1 ~y:y-1 image (one, one, one)

    

)
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
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
