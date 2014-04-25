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




{client{

  let seeking_signal, set_seeking_signal = React.S.create 0.


  (* let seeking_event vid _ _ = *)
  (*   (\* set_seeking_signal (vid##currentTime); *\) *)
  (*   Lwt.return () *)


  (* let seeking ?cancel_handler ?use_capture t = *)
  (*   let open Lwt_js_events in *)
  (*   let ev ?use_capture target = *)
  (*     make_event (Dom_html.Event.make "seeking") ?use_capture target *)
  (*   in *)
  (*   seq_loop ev ?cancel_handler ?use_capture t *)


  (* let react_seeking_div : Html5_types.div elt React.signal = *)
  (*   let react f = *)
  (*     D.(div [ *)
  (*       pcdata (string_of_float f) *)
  (*     ]) *)
  (*   in *)
  (*   React.S.map react seeking_signal *)

}}




let video_test_url =
  "http://media.w3.org/2010/05/sintel/trailer.mp4"


let vtest () =
  let vid = D.(video ~a:[a_controls `Controls]
                 ~src:(Xml.uri_of_string video_test_url) [pcdata "alt"])
  in
  (* let _ = {unit{ *)
  (*   Lwt_js_events.(async (fun () -> *)
  (*     let v = To_dom.of_element %vid in *)
  (*     seeking v (seeking_event v))) *)

  (* }} in *)
  vid


(* ; css_link ~uri:css_uri () *)
let () =
  Web_orchestration_app.register
    ~service:media_service
    (fun name () ->
      Lwt.return D.(
        Eliom_tools.D.html
          ~title:"Media"
          ~css:[]
          (body [
            h2 [pcdata "Media"];
            vtest ()
          ])

      ))
