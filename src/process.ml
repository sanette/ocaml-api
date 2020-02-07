open Soup
open Printf

let ocamlorg = false
(* set this to true to generate the .md files for ocaml.org *)
  
let debug = match Array.to_list Sys.argv with
  | [] -> true
  | list -> not (List.mem "silent" list)
  
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


(* Header for ocaml.org md files. *)
let md_head = "<!-- ((! set title API !)) ((! set documentation !)) ((! set api !)) ((! set nobreadcrumb !)) -->\n"
  
(* it doesn't work without the "()" for some reason... (BUG??) *)
let copyright () =
  "<div class=\"copyright\">The present documentation is copyright Institut \
   National de Recherche en Informatique et en Automatique (INRIA). A complete \
   version can be obtained from <a \
   href=\"http://caml.inria.fr/pub/docs/manual-ocaml/\">this page</a>.</div>"
  |> parse

(* We don't add the "onchange" event because it forces to click twice to an
   external link after entering text. *)
let search_widget with_description =
  sprintf "<div class=\"api_search\"><input type=\"text\" name=\"apisearch\" id=\"api_search\"
	 oninput    = \"mySearch(%b);\"
         onkeypress = \"this.oninput();\"
         onclick    = \"this.oninput();\"
	 onpaste    = \"this.oninput();\">
<img src=\"search_icon.svg\" alt=\"Search\" class=\"svg\" onclick=\"mySearch(%b)\">%s</div>
<div id=\"search_results\"></div>" with_description with_description
    (if with_description then "<span class=\"search_comment\">(search values and descriptions)</span>" else "")
  |> parse

let logo_html () = "<nav class=\"toc brand\"><a class=\"brand\" href=\"https://ocaml.org/\" ><img src=\"colour-logo-gray.svg\" class=\"svg\" alt=\"OCaml\" /></a></nav>" |> parse
  
let process ?(search=true) file out =

  sprintf "Processing %s ..." file |> pr;
  let html = read_file file in
  let soup = parse html in

  (* Add javascript files *)
  if search then begin
    let head = soup $ "head" in
    create_element "script" ~attributes:["src","search.js"] 
    |> append_child head;
    create_element "script" ~attributes:["src","scroll.js"]
    |> append_child head;  
  end;

  (* Add content wrapper *)
  let body = soup $ "body" in
  set_name "div" body;
  add_class "api" body;
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
  let href = let base = Filename.basename file in
    if String.sub base 0 5 = "type_"
    then String.sub base 5 (String.length base - 5) else "#top" in
  let a = create_element "a" ~inner_text:title ~attributes:["href", href] in
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
      if search then search_widget true |> prepend_child body;
      create_element "h1" ~inner_text:"The OCaml API"
      |> prepend_child body;
    | None ->
      if search then search_widget false |> prepend_child nav;
      (* Add "general index" link to all other files *)
      create_element "a" ~inner_text:"< General Index"
        ~attributes:["href", "index.html"]
      |> prepend_child nav in

  (* Add logo *)
  prepend_child header (logo_html ());

  (* Add copyright *)
  append_child body (copyright ());

  (* Save new html file *)
  let new_html = to_string soup in
  sprintf "Saving %s..." out |> pr;
  write_file out new_html;

  (* Save ocamlorg md file *)
  if ocamlorg then
    let md = Filename.remove_extension out ^ ".md" in
    soup $ "div.api" |> to_string
    |> (^) md_head 
    |> write_file md
      
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
    (mdule, value, ("",""))
  | [[], infolist; alist, []] ->
    let mdule, value = parse_pair alist in
    let infohtml, infotext = List.split infolist in
    let infohtml = infohtml |> List.map to_string |> String.concat " "
                   |> String.escaped in
    let infotext = infotext |> String.concat " " |> String.escaped in
    pr (fst value);
    (mdule, value, (infohtml, infotext))
  | _ -> raise (Invalid_argument "parse_tdlist")

let make_index () =
  let html = read_file ("index_values.html"
                        |> with_dir src_dir
                        |> with_dir home) in
  let soup = parse html in
  soup $ "table"
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
                           |> to_list
                           |> List.map (fun info ->
                               let infotext = texts info
                                              |> String.concat "" in
                               (info, infotext)) in
            if alist = [] && infolist = [] then tdlist
            else (alist, infolist) :: tdlist
          ) [] in
      if tdlist = [] then trlist
      else (parse_tdlist tdlist) :: trlist
    ) []


(* Fast version of make_index using Scanf *)
(******************************************)
module Index = struct

  open Scanf
  let index_len = 2545

  let sid x : string = x

  let rec find ch word =
    if Scanning.end_of_input ch then raise Not_found
    else if bscanf ch "%s " sid <> word then find ch word

  (* Return all words encountered before reaching [word]. All 'spaces' between
     words are replaced by single ' 's. *)
  let concat_before ch word =
    let b = Buffer.create 256 in
    let rec loop () = 
      if Scanning.end_of_input ch then raise Not_found;
      let next = bscanf ch "%s " sid in
      if next <> word then begin
        Buffer.add_char b ' ';
        Buffer.add_string b next;
        loop ()
      end else Buffer.contents b in
    loop ()

  let extract_infotext list =
    list |> List.map (fun (a, (val_name, val_ref), info) ->
        let infotext = Soup.texts (Soup.parse info) |> String.concat "" in
        let val_name = Soup.(parse val_name |> R.leaf_text) in
        (* We parse [val_name] simply to replace "&amp;" by "&", etc... it could
           be done much faster manually of course (saving 20% time...): there
           are only a very limited number of such cases. *)
        (a, (val_name, val_ref),
         (String.escaped info, String.escaped infotext)))

  let make () =
    let ch = Scanning.open_in ("index_values.html" |> with_dir src_dir) in
    find ch "<table>";
    let rec loop list =
      if try find ch "<tr><td><a"; false with Not_found -> true
      then list
      else let val_ref = bscanf ch " href=%S" sid in
        let val_name = bscanf ch ">%s@<" sid in
        find ch  "[<a";
        let mod_ref = bscanf ch " href=%S" sid in
        let mod_name = bscanf ch ">%s@<" sid in
        bscanf ch "/a>]</td> <td>" ();
        let info = match bscanf ch "<%s@>" sid with
          | "/td" -> find ch "</tr>"; ""
          | "div class=\"info\"" -> 
            let s = concat_before ch "</td></tr>" in
            "<div class=\"info\">" ^ s
          | s -> raise (Scan_failure s) in
        pr val_ref;
        let new_entry = ((mod_name, mod_ref), (val_name, val_ref), info) in
        loop (new_entry :: list) in
    loop []
    |> extract_infotext

end
(******************************************)

let save_index file index =
  let outch = open_out file in
  output_string outch "var GENERAL_INDEX = [\n";
  index
  |> List.iter (fun (mdule, value, infolist) ->
      fprintf outch "[\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", \"%s\"],\n"
        (fst mdule) (snd mdule) (fst value) (snd value) (fst infolist) (snd infolist));
  output_string outch "]\n";
  close_out outch

let process_index ?(fast=true) () =
  pr "Recreatig index file, please wait...";
  let t = Unix.gettimeofday () in
  let index = if fast then Index.make () else make_index () in
  sprintf "Index created. Time = %f\n" (Unix.gettimeofday () -. t) |> pr;
  save_index (with_dir home "src/index.js") index;
  sprintf "Index saved. Time = %f\n" (Unix.gettimeofday () -. t) |> pr

let process_html overwrite =
  let processed = ref 0 in
  all_html_files ()
  |> List.iter (fun file ->
      match process ~overwrite
              (file |> with_dir src_dir |> with_dir home)
              (file |> with_dir dst_dir |> with_dir home) with
      | Ok () -> incr processed
      | Error s -> pr s
    );
  sprintf "HTML processing done: %u files have been processed." !processed |> pr

let () =
  let args = Sys.argv |> Array.to_list |> List.tl in
  let overwrite = List.mem "overwrite" args in
  let makeindex = List.mem "makeindex" args in
  let makehtml = List.mem "html" args || not makeindex in
  if makehtml then process_html overwrite;
  if makeindex then process_index ()
      
