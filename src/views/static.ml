open BatStd
open Data_mapping
open Config
open Common_fragments

class ['g, 'p] robots cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method get (page : 'g) =
      let ctx = Template.Context.create () in
      let content = Template.render "public/robots.tmpl" ctx in
        new Http.response ~content ~content_type:"text/plain" cgi
  end

