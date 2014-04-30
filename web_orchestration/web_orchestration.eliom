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
    (* Dom_html.window##alert (Js.string (Format.sprintf "%f %f" a b)); *)

  let seeking_signal, set_seeking_signal = React.S.create 0.


  let seeking_event vid _ =
    let ct = Js.Opt.get (vid##getAttribute (Js.string "currentTime"))
      (fun _ -> raise Not_found)
    in
    set_seeking_signal (float_of_string (Js.to_string ct));
    Lwt.return ()


  let seeking ?cancel_handler ?use_capture t =
    let open Lwt_js_events in
    let ev ?use_capture target =
      make_event (Dom_html.Event.make "seeking") ?use_capture target
    in
    seq_loop ev ?cancel_handler ?use_capture t


  let react_seeking_div : Html5_types.div elt React.signal =
    let react f =
      D.(div [
        pcdata (string_of_float f)
      ])
    in
    React.S.map react seeking_signal

  let append d elmt =
    Dom.appendChild (To_dom.of_element d)
      (To_dom.of_element elmt)



}}




let video_test_url =
  "http://media.w3.org/2010/05/sintel/trailer.mp4"


let vtest () =
  let vid =
    D.(video
         ~a:[a_controls `Controls;
             a_onseeking {{fun _ ->
               Firebug.console##log (Js.string "Seeeeek")}}]
         ~src:(Xml.uri_of_string video_test_url) [pcdata "alt"])
  in
  vid


(* ; css_link ~uri:css_uri () *)
let () =
  Web_orchestration_app.register
    ~service:media_service
    (fun name () ->
      let debug_div = Html5.D.div [] in
      let _ = {unit{append %debug_div (R.node react_seeking_div)}} in
      Lwt.return D.(
        Eliom_tools.D.html
          ~title:"Media"
          ~css:[]
          (body [
            debug_div;
            h2 [pcdata "Media"];
            vtest ()
          ])

      ))
