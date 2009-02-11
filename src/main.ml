open Common


let users = Db.users_list ();;
let posts = Db.posts_list ();;

Printf.printf "==============\n%!";;
Printf.printf "=== AUTORI ===\n%!";;
Printf.printf "==============\n%!";;
List.iter Data_mapping.print_user users;;


Printf.printf "============\n%!";;
Printf.printf "=== POST ===\n%!";;
Printf.printf "============\n%!";;
List.iter Data_mapping.print_post posts;;

Db.delete_user "pippo@pluto.com";;
let users = Db.users_list ();;
Printf.printf "==============\n%!";;
Printf.printf "=== AUTORI ===\n%!";;
Printf.printf "==============\n%!";;
List.iter Data_mapping.print_user users;;

Db.delete_post 101;;
let posts = Db.posts_list ();;
Printf.printf "============\n%!";;
Printf.printf "=== POST ===\n%!";;
Printf.printf "============\n%!";;
List.iter Data_mapping.print_post posts;;

let comments = Db.comments_list ();;
Printf.printf "================\n%!";;
Printf.printf "=== COMMENTS ===\n%!";;
Printf.printf "================\n%!";;
List.iter Data_mapping.print_comment comments;;

