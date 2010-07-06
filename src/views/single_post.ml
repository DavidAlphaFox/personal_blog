open BatStd
open CamlTemplate.Model
open Utilities
open Data_mapping
open Config
open Common_fragments
module X = XHTML
module U = Unix
module Ak = Akismet

let (<--) = Template.Context.(<--)

let spf = Printf.sprintf


let comment_preview text =
  let processed_text = text |> Markup.parse_markup |> Markup.safe_xhtml_of_markup |>
                        X.string_of_element in
  spf "
<!-- div class=\"comment-meta col\">
  <p class=\"date small\">${format_date(\"%%e %%B %%Y, %%H:%%M\", comment.comment_time)}</p>
  <dl>
    <dt>
      #if (comment.author_url)
        <strong style=\"font-size:1.2em; line-height:1.6;\">
          <a class=\"url\" rel=\"external nofollow\"
             href=\"${comment.author_url}\">${comment.author_name}</a></strong>
      #else
        <strong style=\"font-size:1.2em; line-height:1.6;\">${comment.author_name}</strong>
      #end
    </dt>
    <dd>
      <img style=\"margin-top: 1em;\"
           alt=\"User Gravatar\"
           src=\"http://www.gravatar.com/avatar/${comment.gravatar}?rating=X&amp;size=64&amp;default=wavatar\" />
    </dd>
  </dl>
</div -->

<div class=\"comment-text\">
  %s
</div>
" processed_text

let body_good_comment post comment token_approve token_delete token_spam =
  let author_url = match comment.author_url with None -> "" | Some s -> s in
  let blog_hostname = Lazy.force Options.blog_hostname in
  let ip = U.string_of_inet_addr comment.author_ip in
  spf "
New comment on your post #%d \"%s\"
Author : %s (IP: %s)
E-mail : %s
URL    : %s
Whois  : http://ws.arin.net/cgi-bin/whois.pl?queryinput=%s
Comment:
--------------------------------------------------------------------------------
%s
--------------------------------------------------------------------------------

Approve it: http://%s/manage-comment/approve/%d/%s
Delete it: http://%s/manage-comment/delete/%d/%s
Spam it: http://%s/manage-comment/mark-spam/%d/%s"
    post.id !!(post.title) comment.author_name ip comment.author_email author_url ip
    comment.content
    blog_hostname comment.comment_id token_approve.tok_value
    blog_hostname comment.comment_id token_delete.tok_value
    blog_hostname comment.comment_id token_spam.tok_value

let body_spam_comment post comment token_approve token_spam =
  let blog_hostname = Lazy.force Options.blog_hostname in
  let author_url = match comment.author_url with None -> "" | Some s -> s in
  let ip = U.string_of_inet_addr comment.author_ip in
  spf "
New SPAM comment on your post #%d \"%s\"
Author : %s (IP: %s)
E-mail : %s
URL    : %s
Whois  : http://ws.arin.net/cgi-bin/whois.pl?queryinput=%s
Comment:
--------------------------------------------------------------------------------
%s
--------------------------------------------------------------------------------

Approve it: http://%s/manage-comment/approve/%d/%s
Delete it: http://%s/manage-comment/delete/%d/%s"
    post.id !!(post.title) comment.author_name ip comment.author_email author_url ip
    comment.content
    blog_hostname comment.comment_id token_approve.tok_value
    blog_hostname comment.comment_id token_spam.tok_value

class ['g, 'p] main cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method private create_common_context db_post =
      let complete_post = Db.complete_post db_post.id in
      let post, last_modified = tpl_model_of_complete_post ~process_markup:true
                                                           ~allow_html:true
                                                           complete_post in
        ctx<--("top_menu", Tstr (X.string_of_element (page_menu None)));
        ctx<--("post", post);
        ctx<--("last_modified", Tfloat last_modified);
        ctx<--("js_object_name", Tstr "single_post");
        ctx<--("preview_present", Tbool false);
        ctx<--("rss_link", Tstr ((Blog_template.post_permalink db_post) ^ "/feed/"));
        ctx<--("feed_title", Tstr ("Comments for “" ^ (!!(db_post.title)) ^ "”"));

    method private respond ?(cache = `No_cache) ?(headers = []) () =
      self#old_browser_warning ();
      let content = Template.render "public/single_post.tmpl" ctx in
        new Http.response ~cache ~headers ~content ~content_type cgi

    method private validate_comment url post author email comment =
      let now = U.gettimeofday () in
      let next_id = Db.next_val "comments" "comment_id" in
      let url = match url with
        | None -> None | Some u when u = "" -> None | Some u -> Some u in
      let comment =
        {
          comment_id = next_id;
          post_id = post.id;
          author_name = author;
          author_email = email;
          author_url = url;
          author_ip = cgi#environment#cgi_remote_addr |> U.inet_addr_of_string;
          author_ua = cgi#environment#input_header_field "User-Agent";
          comment_time = now;
          content = comment;
          approved = true;
          spam = false;
        } in
        let checker = Ak.comment_check
                          ~key:(Lazy.force Options.akismet_key)
                          ~blog:("http://" ^ (Lazy.force Options.blog_hostname) ^ "/")
                          ~user_ip:cgi#environment#cgi_remote_addr
                          ~user_agent:(cgi#environment#input_header_field "User-Agent")
                          ~permalink:(Blog_template.post_permalink post)
                          ~comment_type:"comment"
                          ~comment_author:author
                          ~comment_author_email:email
                          ~comment_content:comment.content in
        let res = match url with
          | None -> checker ()
          | Some u -> checker ~comment_author_url:u () in

        let mail_from = author, Lazy.force Options.my_email_address in
        let mail_to = Lazy.force Options.my_name, Lazy.force Options.my_email_address in
        let comment_str = string_of_comment comment in
        let token_approve = Blog_lib.new_token comment_str in
        let token_delete = Blog_lib.new_token comment_str in
        let token_spam = Blog_lib.new_token comment_str in

        let comment, subject, body, new_url =
          match res with
            | `Good_message -> (
                let comment = { comment with approved = true; spam = false } in
                let subject = spf "[%s] Comment: %s" (Lazy.force Options.blog_title) !!(post.title) in
                let body = body_good_comment post comment token_approve token_delete token_spam in
                let new_url = (Blog_template.post_permalink post) ^ "#comment-" ^ (string_of_int next_id) in
                  comment, subject, body, new_url
              )
            | `Spam_message -> (
                let comment = { comment with approved = false; spam = true } in
                let subject = spf "[%s] SPAM Comment: %s" (Lazy.force Options.blog_title) !!(post.title) in
                let body = body_spam_comment post comment token_delete token_spam in
                let new_url = (Blog_template.post_permalink post) ^ "?moderation=true#comment-moderated" in
                  comment, subject, body, new_url
              )
            | `Error http_call -> (
                let comment = { comment with approved = false; spam = false } in
                let subject = spf "[%s] NOT APPROVED comment: %s" (Lazy.force Options.blog_title) !!(post.title) in
                let body = body_good_comment post comment token_approve token_delete token_spam in
                let new_url = (Blog_template.post_permalink post) ^ "?moderation=true#comment-moderated" in
                  comment, subject, body, new_url
              ) in
          Db.update_comment comment;
          Sendmail.sendmail mail_from mail_to subject
            ~more_headers:[("Reply-To", spf "\"%s\" <%s>" author email)]
            body () |> ignore;
            new Http.response_redirect new_url cgi

    method get ((year, slug, moderation) : 'g) =
      let post = try Db.select_post_by_year_slug year slug
        with Not_found -> raise Http.Http404 in
        if post.status = false then raise Http.Http404;
        self#create_common_context post;
        let last_modified =
          match Hashtbl.find ctx "last_modified" with
            | Tfloat f -> f
            | _ -> failwith "impossible!" in
        let headers = [(`Last_Modified last_modified)] in
          ctx<--("moderation", Tbool (match moderation with Some b when b = true -> true | _ -> false));
          self#respond ~cache:`Unspecified ~headers ();

    method post ((post_id, (preview, (submit, (author, email, url, comment)))) : 'p) =
      let post = try Db.select_post post_id
        with Not_found -> raise Http.Http404 in
        self#create_common_context post;
        let author = strip author in
        let email = strip email in
          ctx<--("preview_present", Tbool true);
          ctx<--("comment_author", Tstr (X.pc author |> X.string_of_element));
          ctx<--("comment_email", Tstr (X.pc email |> X.string_of_element));
          ctx<--("comment_url", Tstr (X.pc (opt_def "" url) |> X.string_of_element));
          ctx<--("comment", Tstr (X.pc comment |> X.string_of_element));
          if author = "" then ctx<--("author_error", Tbool true);
          if email = "" then ctx<--("email_error", Tbool true);
          if comment = "" then ctx<--("comment_error", Tbool true);
            match preview, submit with
              | Some _, None -> begin
                  ctx<--("comment_preview", Tstr (comment_preview comment));
                  self#respond ();
                end
              | None, Some _ -> begin
                  if not (Pcre.pmatch ~rex:email_regexp email) then begin
                    ctx<--("comment_email", Tstr (X.pc "Please, use a correct email" |> X.string_of_element));
                    ctx<--("email_error", Tbool true);
                    self#respond ()
                  end
                  else self#validate_comment url post author email comment
                end
              | _, _ -> raise Http.Http404
  end


class ['g, 'p] preview cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method get (post_id : 'g) =
      match session with
        | Blog_session.Anonymous _ -> new Http.response_redirect "/login" cgi
        | Blog_session.Logged _ -> begin

          let db_post = try Db.select_post post_id
            with Not_found -> raise Http.Http404 in
          let complete_post = Db.complete_post db_post.id in
          let post, last_modified = tpl_model_of_complete_post ~process_markup:true
                                                               ~allow_html:true
                                                               complete_post in
            ctx<--("top_menu", Tstr (X.string_of_element (page_menu None)));
            ctx<--("post", post);
            ctx<--("last_modified", Tfloat last_modified);
            ctx<--("js_object_name", Tstr "single_post");
            ctx<--("preview_present", Tbool false);
            ctx<--("rss_link", Tstr ((Blog_template.post_permalink db_post) ^ "/feed/"));
            ctx<--("feed_title", Tstr ("Comments for “" ^ (!!(db_post.title)) ^ "”"));

            let content = Template.render "public/single_post.tmpl" ctx in
              new Http.response ~content ~content_type cgi
        end
  end


class ['g, 'p] comment_preview cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method post (comment_text : 'p) =
      let content = comment_preview comment_text in
        new Http.response ~content ~content_type cgi
  end


class ['g, 'p] manage_comment cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method get ((action, id, token) : 'g) =
      let valid = Blog_lib.validate_token ~delete:false token in
        if not valid then raise Http.Http404;
        try
          let comment = Db.select_comment id in
          let post = Db.select_post comment.post_id in
          let content =
            match action with
              | "delete" -> begin
                    Db.delete_comment id;
                    spf "<h1>Comment #%d DELETED!</h1>" id
                end
              | "mark-spam" -> begin
                  let comment = { comment with approved = false; spam = true } in
                    Db.update_comment comment;
                      let checker = Ak.submit_spam
                                        ~key:(Lazy.force Options.akismet_key)
                                        ~blog:("http://" ^ (Lazy.force Options.blog_hostname) ^ "/")
                                        ~user_ip:(U.string_of_inet_addr comment.author_ip)
                                        ~user_agent:(comment.author_ua)
                                        ~permalink:(Blog_template.post_permalink post)
                                        ~comment_type:"comment"
                                        ~comment_author:comment.author_name
                                        ~comment_author_email:comment.author_email
                                        ~comment_content:comment.content in
                      let () = match comment.author_url with
                        | None -> checker ()
                        | Some u -> checker ~comment_author_url:u () in
                        spf "<h1>Comment #%d marked as SPAM!</h1>" id
                end
              | "approve" -> begin
                  let comment = { comment with approved = true; spam = false } in
                    Db.update_comment comment;
                      let checker = Ak.submit_ham
                                        ~key:(Lazy.force Options.akismet_key)
                                        ~blog:("http://" ^ (Lazy.force Options.blog_hostname) ^ "/")
                                        ~user_ip:(U.string_of_inet_addr comment.author_ip)
                                        ~user_agent:(comment.author_ua)
                                        ~permalink:(Blog_template.post_permalink post)
                                        ~comment_type:"comment"
                                        ~comment_author:comment.author_name
                                        ~comment_author_email:comment.author_email
                                        ~comment_content:comment.content in
                      let () = match comment.author_url with
                        | None -> checker ()
                        | Some u -> checker ~comment_author_url:u () in
                        spf "<h1>Comment #%d APPROVED!</h1>" id
                end
              | _ -> raise Http.Http404 in
            new Http.response ~content ~content_type cgi
        with Not_found -> raise Http.Http404
  end

