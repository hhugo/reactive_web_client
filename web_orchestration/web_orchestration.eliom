

{shared{
  open Eliom_lib
  open Eliom_content
  open Html5

}}


{client{



  let set_on_play v =
    let vdom = To_dom.of_node v in
    vdom##addEventListner "play" (fun _ ->
      Dom_html.window##alert(Js.string "hello")
    )





}}

let video_test_url =
  "http://media.w3.org/2010/05/sintel/trailer.mp4"



let main_service =
  Eliom_registration.Html5.register_service
    ~path:["media"]
    ~get_params:Eliom_parameter.unit
    D.(fun () () ->
      Lwt.return begin
     let v = video
       ~a:[a_autoplay `Autoplay;a_controls `Controls;a_loop `Loop]
       ~src:(Xml.uri_of_string video_test_url) [] in
     let _ = {unit {set_on_play v}} in
        (html
           (head (title (pcdata "Media")) [])
           (body [h1 [pcdata "Media"];
                  v

                 ])) end)
