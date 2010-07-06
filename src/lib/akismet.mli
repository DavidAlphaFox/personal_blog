val verify_key :
  key:string ->
  blog:string -> [> `Invalid_key of (string * string) list | `Valid_key ]

val comment_check :
  key:string ->
  blog:string ->
  user_ip:string ->
  user_agent:string ->
  ?referrer:string ->
  ?permalink:string ->
  ?comment_type:string ->
  ?comment_author:string ->
  ?comment_author_email:string ->
  ?comment_author_url:string ->
  ?comment_content:string -> unit -> [> `Error of Http_client.http_call | `Good_message | `Spam_message ]

val submit_spam :
  key:string ->
  blog:string ->
  user_ip:string ->
  user_agent:string ->
  ?referrer:string ->
  ?permalink:string ->
  ?comment_type:string ->
  ?comment_author:string ->
  ?comment_author_email:string ->
  ?comment_author_url:string -> ?comment_content:string -> unit -> unit

val submit_ham :
  key:string ->
  blog:string ->
  user_ip:string ->
  user_agent:string ->
  ?referrer:string ->
  ?permalink:string ->
  ?comment_type:string ->
  ?comment_author:string ->
  ?comment_author_email:string ->
  ?comment_author_url:string -> ?comment_content:string -> unit -> unit

