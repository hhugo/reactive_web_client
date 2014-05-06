



let main_service =
  Eliom_registration.Streamlist.register_service
    ~path:["stream"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
      Lwt.return (
        [fun () ->
          Lwt.return (Ocsigen_stream.of_file "static/sin.mp4")
        ], "video/mp4"))





(* let _ = Eliom_predefmod.Streamlist.register view *)
(*   (fun sp id () -> *)
(*     return *)
(*       ([fun () -> *)
(*         return (Ocsigen_stream.of_string (get_data id))], *)
(*        "image/png")) *)
