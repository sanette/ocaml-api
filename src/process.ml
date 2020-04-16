open Soup
open Printf

let ocamlorg = false
(* set this to true to generate the .md files for ocaml.org *)

let compiler_libref = false
(* set this to true to process compilerlibref instead of libref *)

let libref = if compiler_libref then "compilerlibref" else "libref"
    
let debug = match Array.to_list Sys.argv with
  | [] -> true
  | list -> not (List.mem "silent" list)
  
let pr = if debug then print_endline else fun _ -> ()
let home = "."
let with_dir = Filename.concat
                 
(* Set this to the directory where to find the html sources of all versions: *)
let html_maindir = "html"
(* This is the destination directory: *)
let docs_maindir =
  if compiler_libref then libref |> with_dir "docs"
  else "docs"
let src_dir version = libref |> with_dir version |> with_dir html_maindir
let dst_dir version = version |> with_dir docs_maindir

let do_option f = function
  | None -> ()
  | Some x -> f x

let map_option f = function
  | None -> None
  | Some x -> Some (f x)

let flat_option f = function
  | None -> None
  | Some x -> f x

let string_of_opt = function
  | None -> ""
  | Some s -> s

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
    (if with_description
     then {|<span class="search_comment">(search values, type signatures, and descriptions - case sensitive)<div class="search_help"><ul><li>You may search bare values, like <code>map</code>, or indicate the module, like <code>List.map</code>, or type signatures, like <code>int -> float</code>.</li><li>To combine several keywords, just separate them by a space - except if you want to search type signatures: then you must use <strong>two</strong> spaces as separators.</li><li>You may use the special chars <code>^</code> and <code>$</code> to indicate where the matched string should start or end, respectively.</li></ul></div></span>|}
     else "")
  |> parse

let logo_html () = "<nav class=\"toc brand\"><a class=\"brand\" href=\"https://ocaml.org/\" ><img src=\"colour-logo-gray.svg\" class=\"svg\" alt=\"OCaml\" /></a></nav>" |> parse
  
let process ?(search=true) ~version file out =

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
      let a = create_element "a" ~inner_text:(texts h |> String.concat "") ~attributes:["href", href] in
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
  pr "title";
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
      create_element "h1" ~inner_text:(sprintf "The OCaml %sAPI" (if compiler_libref then "Compiler " else ""))
      |> prepend_child body;
    | None ->
      if search then search_widget false |> prepend_child nav;
      (* Add "general index" link to all other files *)
      create_element "a" ~inner_text:"< General Index"
        ~attributes:["href", "index.html"]
      |> prepend_child nav in

  (* Add version number *)
  let vnum = create_element "div" ~class_:"toc_version" in
  let a = create_element "a" ~inner_text:("API Version " ^ version)
      ~attributes:["href", "../index.html";
                   "id", "version-select"] in
  append_child vnum a;
  prepend_child nav vnum;

  (* Add logo *)
  prepend_child header (logo_html ());

  (* Add copyright *)
  append_child body (copyright ());


  sprintf "Saving %s..." out |> pr;

  (* Save ocamlorg md file *)
  if ocamlorg then begin
    soup $$ "a#version-select"
    |> iter (set_attribute "href" "/docs");
    soup $ "div.api" |> to_string
    |> (^) md_head 
    |> write_file out
  end
  else
    (* Save new html file *)
    let new_html = to_string soup in
    write_file out new_html

      
let process ?(overwrite=false) ~version file out =
  let out = if ocamlorg
    then Filename.remove_extension out ^ ".md"
    else out in
  if overwrite || not (Sys.file_exists out)
  then Ok (process ~version file out)
  else Error (sprintf "File %s already exists." out)

let all_html_files version =
  Sys.readdir (with_dir home (src_dir version)) |> Array.to_list
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

let make_index version =
  let html = read_file ("index_values.html"
                        |> with_dir (src_dir version)
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
    list |> List.map (fun (a, (val_name, val_ref), info, signature) ->
        let infotext = Soup.texts (Soup.parse info) |> String.concat "" in
        let val_name = Soup.(parse val_name |> R.leaf_text) in
        (* We parse [val_name] simply to replace "&amp;" by "&", etc... it could
           be done much faster manually of course (saving 20% time...): there
           are only a very limited number of such cases. *)
        (a, (val_name, val_ref),
         (String.escaped info, String.escaped infotext), signature))


  (* Remove all <a> tags. Example:
     {|'a <a href="Event.html#TYPEevent">event</a> -> ('a -> 'b) -> 'b <a href="Event.html#TYPEevent">event</a>|} ==> "'a event -> ('a -> 'b) -> 'b event"
  *)
  let strip_tag =
    let reg = Str.regexp {|<[^>]+ [^>]+>\([^<]+\)</[^>]+>|} in
    let reg_br = Str.regexp "<br>" in
    let reg_space = Str.regexp "  +" in
    fun s -> Str.global_replace reg "\\1" s
             |> Str.global_replace reg_br " "
             |> Str.global_replace reg_space " "
             
  (* Look for signature (with and without html formatting);
     [id] is the HTML id of the value. Example:
     # get_sig ~version:"4.10" ~id:"VALfloat_of_int" "Pervasives.html";;
     - : string option = Some "int -> float"
  *)
  let get_sig ?mod_name ~version ~id file  =
    sprintf "Looking for signature for %s in %s" id file |> pr;
    let anon_t_regexp = Str.regexp "\\bt\\b" in
    let inch = open_in (file |> with_dir (src_dir version)) in
    let reg = Str.regexp_string (sprintf {|id="%s"|} id) in
    (* let reg_type = Str.regexp {|<code class="type">\(.+\)</code>|} in *)
    let rec loop () = try
        let line = input_line inch in
        try let _ = Str.search_forward reg line 0 in
          let code = parse line $ "code.type" in
          let sig_txt = texts code
                        |> String.concat ""
                        |> String.escaped in

          (* We now replace anonymous "t"'s by the qualified "Module.t" *)
          let sig_txt = match mod_name with
            | None -> sig_txt
            | Some mod_name ->
              Str.global_replace anon_t_regexp (mod_name ^ ".t") sig_txt in
          (* let _ = Str.search_forward reg_type line 0 in
           * let the_sig = Str.matched_group 1 line |> strip_a_tag in *)
          sprintf "Signature=[%s]" sig_txt |> pr;
          close_in inch; Some (to_string code |> String.escaped, sig_txt)
        with
        | Not_found -> loop () 
      with
      | End_of_file -> close_in inch; None in
    loop ()

  (* Example: "Buffer.html#VALadd_subbytes" ==> Some "VALadd_subbytes" *)
  let get_id ref =
    try let i = String.index ref '#' in
      Some (String.sub ref (i+1) (String.length ref - i - 1))
    with Not_found -> sprintf "Could not find id for %s" ref |> pr; None

  let make ?(with_sig = true) version =
    let ch = Scanning.open_in ("index_values.html"
                               |> with_dir (src_dir version)) in
    find ch "<table>";
    let rec loop list =
      if try find ch "<tr><td><a"; false with Not_found -> true
      then list
      else
        (* Scan value reference *)
        let val_ref = bscanf ch " href=%S" sid in
        let val_name = bscanf ch ">%s@<" sid in
        find ch  "[<a";

        (* Scan module reference (ie. filename) *)
        let mod_ref = bscanf ch " href=%S" sid in
        let mod_name = bscanf ch ">%s@<" sid in
        bscanf ch "/a>]</td> <td>" ();

        let signature =
          if with_sig then
            get_id val_ref
            |> flat_option (fun id -> get_sig ~mod_name ~version ~id mod_ref)
          else None in

        (* Scan info *)
        let info = match bscanf ch "<%s@>" sid with
          | "/td" -> find ch "</tr>"; ""
          | "div class=\"info\"" -> 
            let s = concat_before ch "</td></tr>" in
            "<div class=\"info\">" ^ s
          | s -> raise (Scan_failure s) in
        pr val_ref;

        let new_entry = ((mod_name, mod_ref), (val_name, val_ref), info, signature) in
        loop (new_entry :: list) in
    loop []
    |> extract_infotext;;
    
end
(******************************************)

let save_index file index =
  let outch = open_out file in
  output_string outch "var GENERAL_INDEX = [\n";
  index
  |> List.iter (fun (mdule, value, infolist, signature) ->
      fprintf outch {|["%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s"],|}
        (fst mdule) (snd mdule) (fst value) (snd value) (fst infolist) (snd infolist)
        (map_option fst signature |> string_of_opt)
        (map_option snd signature |> string_of_opt);
      output_string outch "\n");
  output_string outch "]\n";
  close_out outch

let process_index ?(fast=true) version =
  pr "Recreatig index file, please wait...";
  let t = Unix.gettimeofday () in
  let index = if fast then Index.make version
    else make_index version |> List.map (fun (a,b,c) -> (a,b,c,None)) in
  sprintf "Index created. Time = %f\n" (Unix.gettimeofday () -. t) |> pr;
  save_index (with_dir home (sprintf "src/%s/index-%s.js" libref version)) index;
  sprintf "Index saved. Time = %f\n" (Unix.gettimeofday () -. t) |> pr

let sys_mkdir dir =
  if Sys.command (sprintf "mkdir -p %s" dir) <> 0
  then failwith ("Could not create directory" ^ dir)

let process_html overwrite version =
  sys_mkdir (dst_dir version);
  let processed = ref 0 in
  all_html_files version
  |> List.iter (fun file ->
      match process ~overwrite ~version
              (file |> with_dir (src_dir version) |> with_dir home)
              (file |> with_dir (dst_dir version) |> with_dir home) with
      | Ok () -> incr processed
      | Error s -> pr s
    );
  sprintf "Version %s, HTML processing done: %u files have been processed."
    version !processed |> pr

(*-------------------------*)
  
let sys_cp file dst =
  if Sys.command (sprintf "cp %s %s" file dst) <> 0
  then failwith ("Could not copy " ^ file)

let sys_mv file dst =
  if Sys.command (sprintf "mv %s %s" file dst) <> 0
  then failwith ("Could not move " ^ file)

(* Download version of the "libref" if the dir does not exist yet. *)
(* Remark: remove the html_maindir to force downloading everything. *)
let download_version version =
  let pwd = Sys.getcwd () in
  begin try
      let dir = Filename.concat html_maindir version in
      if ((not compiler_libref) || float_of_string version >= 4.08) &&
         not (Sys.file_exists (with_dir dir libref))
      then begin
        sys_mkdir dir;
        let url = sprintf
            "http://caml.inria.fr/distrib/ocaml-%s/ocaml-%s-refman-html.tar.gz"
            version version in
        let tmp = Filename.temp_file version ".tar.gz" in
        if Sys.command (sprintf "wget %s -O %s" url tmp) <> 0
        then failwith ("Could not download manual at " ^ url)
        else begin
          Sys.chdir (Filename.dirname tmp);
          if Sys.command (sprintf "tar xvf %s" tmp) <> 0
          then failwith (sprintf "Could not extract %s." tmp)
          else begin
            Sys.remove tmp;
            let src = with_dir "htmlman" libref in
            sys_mv src (with_dir pwd dir);
          end
        end
      end
    with
    | e -> Sys.chdir pwd; raise e
  end;
  Sys.chdir pwd

let copy_css version =
  sys_cp (sprintf "src/%s/index-%s.js" libref version)
    (with_dir (dst_dir version) "index.js");
  ["style.css"; "search.js"; "scroll.js";
   "colour-logo-gray.svg"; "search_icon.svg"] 
  |> List.iter (fun src ->
      let dst = src |> with_dir (dst_dir version) in
      if not (Sys.file_exists dst)
      then if Sys.command (sprintf "ln -s -f ../%s %s" src dst) <> 0
        then failwith (sprintf "Could not link %s" dst))

let () =
  let _all_versions = ["4.08"] in

  let all_versions = Array.init 11 (fun i -> sprintf "4.%02u" i)
                     |> Array.to_list in

  let all_versions = if compiler_libref
    then List.filter (fun v -> float_of_string v >= 4.08) all_versions
    else all_versions in
  
  List.iter download_version all_versions;

  let args = Sys.argv |> Array.to_list |> List.tl in
  let overwrite = List.mem "overwrite" args in
  let makeindex = List.mem "makeindex" args in
  let makehtml = List.mem "html" args || not makeindex in
  if makehtml then List.iter (process_html overwrite) all_versions;
  if makeindex then List.iter process_index all_versions;
  List.iter copy_css all_versions

