{shared{
  open Eliom_lib
  open Eliom_content
  open Html5
}}

module Web_orchestration_app =
  Eliom_registration.App (
    struct
      let application_name = "web_orchestration"
    end)


let media_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()



{shared{
  type vmessages = Play | Pause | Seek of float | Time_update deriving (Json)
}}
let bus = Eliom_bus.create Json.t<vmessages>





{client{
    (* Dom_html.window##alert (Js.string (Format.sprintf "%f %f" a b)); *)

  let bc_video_signal, set_bc_video_signal = React.S.create Pause
  let video_signal, set_video_signal = React.S.create Pause

  let react_bc_video : unit React.signal =
    let react m = ignore (Eliom_bus.write %bus m)
    in React.S.map react bc_video_signal

  let react_video vid : unit React.signal =
    let vid = (Js.Unsafe.coerce vid) in
    let react = function
      | Play -> vid##play ()
      | Pause -> vid##pause ()
      | Seek f -> vid##currentTime <- f
      | Time_update -> ()
    in React.S.map react video_signal

  let append d elmt =
    Dom.appendChild (To_dom.of_element d)
      (To_dom.of_element elmt)

  let _ =
    Lwt.async (fun () -> Lwt_stream.iter (set_video_signal)
      (Eliom_bus.stream %bus))

  let seeking ev =
    let target = Js.Optdef.get (ev##target) (fun _ -> assert false) in
    let t = ((Js.Unsafe.coerce target)##currentTime) in
    set_bc_video_signal (Seek t);
    set_video_signal (Seek t)

  let play env =
    set_bc_video_signal Play;
    set_video_signal Play

  let pause env =
    (* Firebug.console##log (Js.string "pause !"); *)
    set_bc_video_signal Pause;
    set_video_signal Pause

  let progress env =
    set_video_signal Time_update


  let progress_bar vid : Html5_types.progress elt React.signal =
    let vid = (Js.Unsafe.coerce (To_dom.of_element vid)) in
    let react up =
      let value = vid##currentTime /. vid##duration *. 100. in
      D.(progress ~a:[a_max 100.; a_onseeked seeking; a_float_value value]  [])
    in
    React.S.map react video_signal


}}

(* let video_test_url = "http://download.blender.org/peach/bigbuckbunny_movies/BigBuckBunny_320x180.mp4" *)

let video_test_url =
  "http://download.blender.org/durian/trailer/sintel_trailer-480p.mp4"

let video_uri = (Html5.D.make_uri
              ~service:(Eliom_service.static_dir ())
              ["sin.mp4"])






let vtest () =
  let vid =
    D.(video
         ~a:[(* a_controls `Controls; *)
             a_onseeking {{ seeking }};
             a_onplay {{ play }};
             a_onpause {{ pause }};
             a_ontimeupdate {{ progress }}
            ]
         ~src:(video_uri)
         (* ~src:(Xml.uri_of_string video_test_url) *)
    )[ (*D.source ~src:video_uri ~a:[D.a_mime_type "video/mp4"] ; *)
     D.pcdata "alt"]
  in
  let _ = {unit{
    Lwt_js_events.async (fun () ->
      ignore (react_video (To_dom.of_element %vid));
      Lwt.return ())
  }} in
  vid


let pause_button () =
  let open D in
  let button = string_input ~input_type:`Submit () ~value:"Pause" in
  let _ = {unit{
    Lwt_js_events.(
      async (fun () -> clicks (To_dom.of_element %button)
        (fun _ _ -> pause (); Lwt.return ())))
  }}
  in button

let play_button () =
  let open D in
  let button = string_input ~input_type:`Submit () ~value:"Play" in
  let _ = {unit{
    Lwt_js_events.(
      async (fun () -> clicks (To_dom.of_element %button)
        (fun _ _ -> play (); Lwt.return ())))
  }}
  in button



(* ; css_link ~uri:css_uri () *)
let () =
  Web_orchestration_app.register
    ~service:media_service
    (fun name () ->
      let debug_div = Html5.D.div [] in
      let vtest = vtest () in
      let _ = {unit{append %debug_div (R.node (progress_bar %vtest))}} in
      Lwt.return D.(
        Eliom_tools.D.html
          ~title:"Media"
          ~css:[]
          (body [
            debug_div;
            h2 [pcdata "Media"];
            vtest;
            play_button (); pause_button ();
            debug_div;

          ])

      ))
