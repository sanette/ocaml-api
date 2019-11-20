open Soup
open Printf

let debug = false
let pr = if debug then print_endline else fun _ -> ()
let home = "."
let src_dir = "libref"
let dst_dir = "docs"

let with_dir = Filename.concat
                 
let do_option f = function
  | None -> ()
  | Some x -> f x

let map_option f = function
  | None -> None
  | Some x -> Some (f x)

let flat_option f = function
  | None -> None
  | Some x -> f x
  
let copyright () =
  "<div class=\"copyright\">The present documentation is copyright Institut \
   National de Recherche en Informatique et en Automatique (INRIA). A complete \
   version can be obtained from <a \
   href=\"http://caml.inria.fr/pub/docs/manual-ocaml/\">this page</a>.</div>"
  |> parse

let process file out =

  sprintf "Processing %s ..." file |> pr;
  let html = read_file file in
  let soup = parse html in

  (* Add content wrapper *)
  let body = soup $ "body" in
  set_name "div" body;
  add_class "content" body;
  wrap body (create_element "body");

  (* Delete previous/up/next links *)
  body $? "div.navbar"
  |> do_option delete;

  (* Create TOC with H2 and H3 elements *)
  (* It would be much simpler to insert only H2 *)
  let header = create_element "header" in
  prepend_child body header;
  let nav = create_element "nav" ~class_:"toc" in
  append_child header nav;
  let ul = create_element "ul" in
  append_child nav ul;
  let h3_open = ref false in
  let h3_current = ref (create_element "ul") in
  let li_current = ref (create_element "li") in
  (* Create a "li" element inside "ul" from a header "h" (h2 or h3 typically) *)
  let li_of_h ul h =
    li_current := create_element "li";
    append_child ul !li_current;
    match attribute "id" h with
    | Some id -> 
      let href = "#" ^ id in
      let a = create_element "a" ~inner_text:(R.leaf_text h) ~attributes:["href", href] in
      append_child !li_current a
    | None -> () in

  descendants body
  |> elements
  |> iter (fun h -> match name h with
      | "h2" ->
        h3_open := false;
        li_of_h ul h
      | "h3" when !h3_open ->
        li_of_h !h3_current h
      | "h3" ->
        h3_open := true;
        h3_current := create_element "ul";
        append_child ul !li_current;
        append_child !li_current !h3_current;
        li_of_h !h3_current h
      | _ -> ());
  let title = soup $ "title" |> R.leaf_text in
  let a = create_element "a" ~inner_text:title ~attributes:["href", "#"] in
  let div = create_element ~class_:"toc_title" "div" in
  append_child div a;
  prepend_child nav div;

  (* In case of indexlist, add it to TOC *)
  (* This only happens for "index.html" *)
  let () = match body $? "ul.indexlist" with
    | Some uli ->
      delete uli;
      append_child ul uli;
      unwrap uli;
      create_element "h1" ~inner_text:"The OCaml API"
      |> prepend_child body 
    | None -> (* Add "general index" link to all other files *)
      create_element "a" ~inner_text:"< General Index"
        ~attributes:["href", "index.html"]
      |> prepend_child nav in

  (* Add logo *)
  let logo_html = "<nav class=\"toc brand\"><a class=\"brand\" href=\"https://ocaml.org/\" ><img src=\"colour-logo-gray.svg\" class=\"svg\" alt=\"OCaml\" /></a></nav>" in
  prepend_child header (parse logo_html);

  (* Add copyright *)
  append_child body (copyright ());

  (* Save new html file *)
  let new_html = to_string soup in
  sprintf "Saving %s..." out |> pr;
  write_file out new_html

let process ?(overwrite=false) file out =
  if overwrite || not (Sys.file_exists out)
  then Ok (process file out)
  else Error (sprintf "File %s already exists." out)

let all_html_files () =
  Sys.readdir (with_dir home src_dir) |> Array.to_list
  |> List.filter (fun s -> Filename.extension s = ".html")


(* Creation of a search bar *)
let parse_pair = function
  | [a; b] -> (a,b) 
  | _ -> raise (Invalid_argument "parse_pair")

let parse_tdlist = function
  | [alist, []] -> let mdule, value = parse_pair alist in
    pr (fst value);
    (mdule, value, [])
  | [[], infolist; alist, []] -> let mdule, value = parse_pair alist in
    pr (fst value);
    (mdule, value, infolist)
  | _ -> raise (Invalid_argument "parse_tdlist")
  
let make_index () =
  let html = read_file ("index_values.html"
                        |> with_dir src_dir
                        |> with_dir home) in
  let soup = parse html in
  soup $ "tbody"
  |> select "tr"
  |> fold (fun trlist tr ->
      let tdlist =
        tr $$ "td"
        |> fold (fun tdlist td ->
            let alist = td $$ ">a"
                        |> fold (fun alist a ->
                            (R.leaf_text a, R.attribute "href" a) :: alist
                          ) [] in
            let infolist = td $$ "div.info"
                           |> to_list in
            if alist = [] && infolist = [] then tdlist
            else (alist, infolist) :: tdlist
          ) [] in
      if tdlist = [] then trlist
      else (parse_tdlist tdlist) :: trlist
    ) []

let save_index file index =
  let outch = open_out file in
  output_string outch "var GENERAL_INDEX = [\n";
  index
  |> List.iter (fun (mdule, value, infolist) ->
      fprintf outch "[\"%s\", \"%s\", \"%s\", \"%s\", \"%s\"],\n"
        (fst mdule) (snd mdule) (fst value) (snd value)
        (infolist |> List.map to_string |> String.concat " " |> String.escaped));
  output_string outch "]\n";
  close_out outch

let () =
  let processed = ref 0 in
  all_html_files ()
  |> List.iter (fun file ->
      match process ~overwrite:true
              (file |> with_dir src_dir |> with_dir home)
              (file |> with_dir dst_dir |> with_dir home) with
      | Ok () -> incr processed
      | Error s -> pr s
    );
  sprintf "Done: %u files have been processed." !processed |> pr

let process_index () =
  let t = Unix.gettimeofday () in
  let index =  make_index () in
  save_index "index.js" index;
  sprintf "Time = %f\n" (Unix.gettimeofday () -. t) |> pr
