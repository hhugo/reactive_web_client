{shared{
  open Eliom_lib
  open Eliom_content
  open Html5
  open Format
}}

module Web_orchestration_app =
  Eliom_registration.App (
    struct
      let application_name = "web_orchestration"
    end)


let media_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()



{shared{
  type action = Play | Pause | Seek of float | Progress of float
  type vmessages = BPlay | BPause | BSeek of float deriving (Json)
}}
let bus = Eliom_bus.create Json.t<vmessages>



{client{
    (* Dom_html.window##alert (Js.string (Format.sprintf "%f %f" a b)); *)

  let bc_video_signal, set_bc_video_signal = React.S.create BPause
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
      | Progress f -> ()
    in React.S.map react video_signal

  let append d elmt =
    Dom.appendChild (To_dom.of_element d)
      (To_dom.of_element elmt)

  let action_of_message = function
    | BPlay -> Play
    | BPause -> Pause
    | BSeek f -> Seek f



  let _ =
    Lwt.async (fun () -> Lwt_stream.iter
      (fun x -> set_video_signal (action_of_message x))
      (Eliom_bus.stream %bus))

  let bind_event event_string html_elt f =
    Lwt.async (fun () ->
      let ev = Dom_html.Event.make event_string in
      Lwt_js_events.seq_loop
        (Lwt_js_events.make_event ev) ~use_capture:true
        (To_dom.of_element html_elt) f)


  let seeking = fun ev ->
    let target = Js.Optdef.get (ev##target) (fun _ -> assert false) in
    let t = ((Js.Unsafe.coerce target)##value) in
    Firebug.console##log (Js.string (string_of_float t));
    (* set_bc_video_signal (BSeek t); *)
    set_video_signal (Seek t)

  let play _ =
    set_bc_video_signal BPlay;
    set_video_signal Play

  let pause _ =
    set_bc_video_signal BPause;
    set_video_signal Pause


  let progress2 ev =
    let target = Js.Optdef.get (ev##target) (fun _ -> assert false) in
    let t = ((Js.Unsafe.coerce target)##currentTime) in
    set_video_signal (Progress t)


  let progress ev _ = Firebug.console##log (Js.string "test"); Lwt.return ()

  let progress_bar vid : Html5_types.input elt React.signal =
    let vid = (Js.Unsafe.coerce (To_dom.of_element vid)) in
    let react up =
      let value = vid##currentTime /. vid##duration *. 100. in
      D.(float_input ~input_type:`Range ()
           ~a:[a_onchange seeking]
           ~value:value)

      (* let button = string_input ~input_type:`Submit () ~value:"Pause" in *)
      (* D.(progress ~a:[a_max 100.;
         a_onchange seeking; a_float_value value] []) *)

    in
    React.S.map react video_signal


  (* let timeupdate = *)


}}



(* let video_test_url = "http://download.blender.org/peach/bigbuckbunny_movies/BigBuckBunny_320x180.mp4" *)

let video_test_url =
  "http://download.blender.org/durian/trailer/sintel_trailer-480p.mp4"

let video_uri = (Html5.D.make_uri
              ~service:(Eliom_service.static_dir ())
              ["sin.webm"])






let vtest () =
  let vid =
    D.(video
         ~a:[(* a_controls `Controls; *)
             a_onseeking {{ seeking }};
             a_onplay {{ play }};
             a_onpause {{ pause }};
             a_ontimeupdate {{ progress2 }}
            ]
         ~src:(video_uri)
         (* ~src:(Xml.uri_of_string video_test_url) *)
    )[ (*D.source ~src:video_uri ~a:[D.a_mime_type "video/mp4"] ; *)
     D.pcdata "alt"]
  in
  let _ = {unit{

    bind_event "timeupdate" %vid progress;
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
      let progress_div = Html5.D.div [] in
      let vtest = vtest () in
      Lwt.return D.(
        let _ = {unit{append %progress_div (R.node (progress_bar %vtest))}} in
        Eliom_tools.D.html
          ~title:"Media"
          ~css:[]
          (body [
            h2 [pcdata "Media"];
            vtest;
            div [
              play_button (); pause_button (); progress_div]
          ])
      ))
