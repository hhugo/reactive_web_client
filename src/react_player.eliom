{shared{
open Eliom_lib
open Eliom_content
open Html5
open Format
}}

module React_Player_app =
  Eliom_registration.App (
  struct
    let application_name = "react_player"
  end)


let media_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

{shared{
  type action = Play | Pause | Seek of float
  type vmessage = BPlay | BPause | BSeek of float deriving (Json)

  let string_of_message = function
    | BPlay -> "play"
    | BPause -> "pause"
    | BSeek f -> sprintf "seek %f" f

  let action_of_message = function
    | BPlay -> Play
    | BPause -> Pause
    | BSeek f -> Seek f
}}

let bus = Eliom_bus.create Json.t<vmessage>

{client{

   (* Dom_html.window##alert (Js.string (Format.sprintf "%f %f" a b)); *)

  let bc_video_signal, set_bc_video_signal = React.S.create BPause
  let video_signal, set_video_signal = React.S.create Pause
  let progress_signal, set_progress_signal = React.S.create (0., 0.)
  let block_progress_signal, set_block_progress_signal = React.S.create false

  let react_bc_video : unit React.signal =
    let react m = ignore (Eliom_bus.write %bus m)
    in React.S.map react bc_video_signal


  let _ =
    Lwt.async (fun () ->
        Lwt_stream.iter
          (fun m ->
             Firebug.console##log (Js.string (string_of_message m));
             set_video_signal (action_of_message m))
          (Eliom_bus.stream %bus))
}}

let progress_bar =
  let value = {float React.signal{React.S.map (fun (current, duration) ->
      if duration = 0. then 0. else current /. duration *. 100.)
      progress_signal}} in
  let duration_s = {float React.signal{React.S.map snd progress_signal}} in
  let oninput = {{fun ev ->
      let target : Dom_html.inputElement Js.t = Js.Unsafe.coerce (Dom_html.eventTarget ev) in
      let value_input = Js.parseFloat (target##value) in
      let video_time = (value_input /. 100. *. (React.S.value %duration_s)) in
      set_bc_video_signal (BSeek video_time)}}
  in
  D.(float_input ~input_type:`Range ()
       ~a:[a_oninput oninput; a_input_min 0.; a_input_max 100.;
           a_onmousedown {{fun _ -> set_block_progress_signal true}};
           a_onmouseup {{fun _ -> set_block_progress_signal false}};
           C.attr {{R.a_value (React.S.map (sprintf "%.0f") %value)}}])
{shared{
   let media_test_url =
     "http://download.blender.org/durian/trailer/sintel_trailer-480p.mp4"

  let media_uri =
    Html5.D.make_uri
      ~service:(Eliom_service.static_dir ())
      ["hb.mp3"]
}}
let media_tag () =
  let vid = D.(audio
       ~src:(media_uri)
       ~a:[
         a_ontimeupdate {{fun ev ->
             let vid : Dom_html.audioElement Js.t = Js.Unsafe.coerce (Dom_html.eventTarget ev) in
             if vid##currentTime = 0. || not (React.S.value block_progress_signal)
             then set_progress_signal (vid##currentTime, vid##duration)}};
       ]
    )[D.pcdata "alt"] in
  let _ = {unit{
    let vid : Dom_html.audioElement Js.t = To_dom.of_audio (Obj.magic (%vid)) in
    let react = function
      | Play -> vid##play ()
      | Pause -> vid##pause ()
      | Seek f -> vid##currentTime <- f
    in ignore (React.S.map react video_signal);
  }} in
  vid

let client_style () = {{
    R.a_style (React.S.map (fun (current,duration) ->
        let v = if duration = 0. then 0 else int_of_float (mod_float (current /. duration *. 2000.) 360.) in
        Printf.sprintf "color:hsl(%d,100%%,50%%)" v
      ) progress_signal )
  }}

let pause_button () =
  D.(string_input ~input_type:`Submit () ~value:"Pause"
       ~a:[a_onmousedown {{ fun _ -> set_bc_video_signal BPause }}])

let play_button () =
  D.(string_input ~input_type:`Submit () ~value:"Play"
       ~a:[a_onclick {{ fun _ -> set_bc_video_signal BPlay }}])

(* ; css_link ~uri:css_uri () *)
let () =
  React_Player_app.register
    ~service:media_service
    (fun name () ->
       Lwt.return D.(
           Eliom_tools.D.html
             ~title:"Media"
             ~css:[]
             (body [
                 h2 ~a:[C.attr (client_style ())] [pcdata "Media"];
                 (media_tag ());
                 div [
                   play_button ();
                   pause_button ();
                   progress_bar ]
               ])
         ))
