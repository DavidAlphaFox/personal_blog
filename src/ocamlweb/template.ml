module Make
    (Config : Signatures.TEMPLATE_CONFIG) =
  struct
    module Context =
      struct
        type context = (string, CamlTemplate.Model.tvalue) Hashtbl.t

        let create () : context = Hashtbl.create 16

        let (<--) (context : context) (var_name, value) = Hashtbl.add context var_name value
      end

    let file_loader = lazy (
        CamlTemplate.Cache.make_file_loader ~template_dir:(Config.template_dir ())
      )

    let global_template_cache = lazy (
        CamlTemplate.Cache.create ~loader:(Lazy.force file_loader)
          ~check_interval:(Config.template_check_interval ()) ()
      )

    let render template_name context =
      let template =
        CamlTemplate.Cache.get_template (Lazy.force global_template_cache) template_name in
      let buf = Buffer.create 1024 in
        CamlTemplate.merge template context buf;
        Buffer.contents buf

  end

