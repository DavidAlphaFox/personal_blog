open Http_client

let (|>) x f = f x

let make_url ?key ?(version = "1.1") path =
  match key with
    | None -> ("http://rest.akismet.com/" ^ version ^ path)
    | Some k -> ("http://" ^ k ^ ".rest.akismet.com/" ^ version ^ path)

let verify_key_url         = make_url "/verify-key"
let comment_check_url ~key = make_url ~key "/comment-check"
let submit_spam_url   ~key = make_url ~key "/submit-spam"
let submit_ham_url    ~key = make_url ~key "/submit-ham"

let add name param l =
  match param with
    | None -> l
    | Some x -> (name, x)::l

let post_message  ~key ~blog ~user_ip ~user_agent
                  ~referrer ~permalink
                  ~comment_type ~comment_author ~comment_author_email
                  ~comment_author_url ~comment_content url_builder =
  let param_list = [
      ("blog", blog);
      ("user_ip", user_ip);
      ("user_agent", user_agent);
    ] in
  let param_list = add "referrer" referrer param_list |>
                   add "permalink" permalink |>
                   add "comment_type" comment_type |>
                   add "comment_author" comment_author |>
                   add "comment_author_email" comment_author_email |>
                   add "comment_author_url" comment_author_url |>
                   add "comment_content" comment_content in
    Convenience.http_post_message (url_builder ~key) param_list


(* http://akismet.com/development/api/ *)
let verify_key ~key ~blog =
  let response = Convenience.http_post_message verify_key_url [
      ("key", key);
      ("blog", blog)
    ] in
  if response#response_body#value = "valid"
  then `Valid_key
  else `Invalid_key (response#response_header#fields)


let comment_check ~key ~blog ~user_ip ~user_agent
                  ?referrer ?permalink
                  ?comment_type ?comment_author ?comment_author_email
                  ?comment_author_url ?comment_content () =
  let response = post_message ~key ~blog ~user_ip ~user_agent
                              ~referrer ~permalink
                              ~comment_type ~comment_author ~comment_author_email
                              ~comment_author_url ~comment_content comment_check_url in
  let resp_message = response#response_body#value in
    try
      match bool_of_string resp_message with
        | true -> `Spam_message
        | false -> `Good_message
    with
      | Invalid_argument "bool_of_string" -> `Error response


let submit_spam ~key ~blog ~user_ip ~user_agent
                ?referrer ?permalink
                ?comment_type ?comment_author ?comment_author_email
                ?comment_author_url ?comment_content () =
  post_message ~key ~blog ~user_ip ~user_agent
               ~referrer ~permalink
               ~comment_type ~comment_author ~comment_author_email
               ~comment_author_url ~comment_content submit_spam_url |> ignore


let submit_ham ~key ~blog ~user_ip ~user_agent
               ?referrer ?permalink
               ?comment_type ?comment_author ?comment_author_email
               ?comment_author_url ?comment_content () =
  post_message ~key ~blog ~user_ip ~user_agent
               ~referrer ~permalink
               ~comment_type ~comment_author ~comment_author_email
               ~comment_author_url ~comment_content submit_ham_url |> ignore

