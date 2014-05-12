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

  let append = Manip.appendChild

  let action_of_message = function
    | BPlay -> Play
    | BPause -> Pause
    | BSeek f -> Seek f

  let _ =
    Lwt.async (fun () -> Lwt_stream.iter
      (fun m ->
        Firebug.console##log (Js.string (string_of_message m));
        set_video_signal (action_of_message m))
      (Eliom_bus.stream %bus))

  let bind_event event_string html_elt f =
    Lwt.async (fun () ->
      let ev = Dom_html.Event.make event_string in
      Lwt_js_events.seq_loop
        (Lwt_js_events.make_event ev) ~use_capture:true
        html_elt f)

  let disable_event event_string html_elt =
    Lwt.async (fun () ->
      let event = Lwt_js_events.make_event (Dom_html.Event.make event_string) in
      Lwt_js_events.seq_loop event ~use_capture:true html_elt
        (fun ev _ -> Dom_html.stopPropagation ev; Lwt.return ()))


  let disable_event event_string html_elt =
    let event = Lwt_js_events.make_event (Dom_html.Event.make event_string) in
    (Js.Unsafe.coerce html_elt)##removeEventListener event

  let get_target ev =
    Js.Optdef.get (ev##target) (fun _ -> assert false)


  (* let stop_update_range vid _ = *)
  (*   disable_event "timeupdate" vid (fun _ _ -> *)
  (*     Firebug.console##log ("Hello"); *)
  (*     Lwt.return ()) *)

  (* let reload_update_range vid _ = *)
  (*   bind_event "timeupdate" vid progress *)

  let progress_bar : Html5_types.input elt =
    let value = React.S.map (fun (current, duration) ->
        if duration = 0. then 0. else current /. duration *. 100.)
        progress_signal in
    let duration_s = React.S.map snd progress_signal in
    let oninput ev =
      let target : Dom_html.inputElement Js.t = Js.Unsafe.coerce (get_target ev) in
      let value_input = Js.parseFloat (target##value) in
      let video_time = (value_input /. 100. *. (React.S.value duration_s)) in
      set_bc_video_signal (BSeek video_time)
    in
    D.(float_input ~input_type:`Range ()
         ~a:[a_oninput oninput; a_input_min 0.; a_input_max 100.;
             a_onmousedown (fun _ -> set_block_progress_signal true);
             a_onmouseup (fun _ -> set_block_progress_signal false);
             R.a_value (React.S.map (sprintf "%f") value)])
}}


{client{

class type audio = object
  inherit Dom_html.element
  method currentTime : float Js.prop
  method duration : float Js.prop
  method play : unit Js.meth
  method pause : unit Js.meth
end

}}

let media_test_url =
  "http://download.blender.org/durian/trailer/sintel_trailer-480p.mp4"

let media_uri = (Html5.D.make_uri
              ~service:(Eliom_service.static_dir ())
              ["hb.mp3"])

let media_tag () =
  let vid = D.(audio ~src:(media_uri))[D.pcdata "alt"] in
  let _ = {unit{
    Lwt_js_events.async (fun () ->
      let vid : audio Js.t = Js.Unsafe.coerce (To_dom.of_element %vid) in
      let timeupdate _ _ =
        set_progress_signal (vid##currentTime, vid##duration);
        Lwt.return ()
      in bind_event "timeupdate" vid timeupdate;

      let react = function
        | Play -> vid##play ()
        | Pause -> vid##pause ()
        | Seek f -> vid##currentTime <- f
      in ignore (React.S.map react video_signal);
      let react block = if block then disable_event "timeupdate" vid
      in ignore (React.S.map react block_progress_signal);
      Lwt.return ())
  }} in
  vid


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
      let progress_div = Html5.D.div [] in
      let media = media_tag () in
      Lwt.return D.(
        let _ = {unit{append %progress_div progress_bar}} in
        Eliom_tools.D.html
          ~title:"Media"
          ~css:[]
          (body [
            h2 [pcdata "Media"];
            media;
            div [
              play_button (); pause_button (); progress_div]
          ])
      ))
