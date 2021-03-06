(* Pour la compilation : ocamlopt -pp camlp4o *)


(* Structure de donn�es d'une ligne *)

type rate_latency = {
  rate : float;
  latency : float;
}

type server = {
  beta : rate_latency list;
}

type sigma_rho = {
  sigma : float;
  rho : float;
}

type flow = {
  debut : int;
  fin : int;
  alpha : sigma_rho list;
}

type line = {
    nservers : int;
    nflows : int;
    servers : server array;
    flows : flow array;
    objectif : char * int;
}  

(* Fonctions de base sur les paires et les listes *)

let fst (a,b) = a;;

let snd (a,b) = b;;


let sort_flows liste = Array.of_list (List.rev liste);;  

let sort_servers liste = Array.of_list (List.rev liste);;

let comment_killer_line_counter lc s =
  let rec state0 = parser
      [< 'c; s >] -> (match c with
	'#' -> state1 s
      | '\n' -> incr lc; [< 'c; state0 s >]
      | _ -> [< 'c; state0 s >])
    | [< >] -> [< >]
  and state1 = parser
      [< 'c; s >] ->
        if c = '\n' then
          begin
            incr lc;
            [< '' '; state0 s >]
          end
        else
          state1 s
    | [< >] -> [< >]
  in
  state0 s

let rec parse_line s = 
  let rec parse1 = parser
      [< '(Genlex.Kwd "nservers"); '(Genlex.Int n);
	 '(Genlex.Kwd "nflows"); '(Genlex.Int m);
	 '(Genlex.Kwd "objective"); '(Genlex.Char obj); '(Genlex.Int k);
	 '(Genlex.Kwd "beginservers"); servers = parse_servers [];
	 '(Genlex.Kwd "beginflows"); flows = parse_flows [] >] -> 
	  ({nservers = n; nflows = m; objectif = (obj,k); 
	     servers = (sort_servers servers);flows = (sort_flows flows)})
and parse_servers res = parser
    [< '(Genlex.Kwd "endservers") >] -> res
  | [< '(Genlex.Kwd "server"); '(Genlex.Int i);
       beta = parse_beta [];s>] -> 
parse_servers ({beta=beta}::res) s 

and parse_beta res = parser
    [< '(Genlex.Kwd ";")>] -> res
  | [< '(Genlex.Kwd "("); 
       '(Genlex.Float rate); '(Genlex.Kwd ",");'(Genlex.Float latency); 
       '(Genlex.Kwd ")");s >] -> parse_beta ({rate=rate;latency=latency}::res) s

and parse_flows res = parser
    [< '(Genlex.Kwd "endflows")>] -> res
  | [< '(Genlex.Kwd "flow"); '(Genlex.Int j); '(Genlex.Kwd "[");
       '(Genlex.Int d); '(Genlex.Kwd ","); '(Genlex.Int f); '(Genlex.Kwd "]");
       alpha = parse_alpha []; s >] -> parse_flows 
      ({debut = d;fin = f;alpha = alpha}::res) s

and parse_alpha res = parser 
    [< '(Genlex.Kwd ";")>] -> res
  | [< '(Genlex.Kwd "("); 
       '(Genlex.Float sigma); '(Genlex.Kwd ",");'(Genlex.Float rho); 
       '(Genlex.Kwd ")"); s >] -> parse_alpha ({sigma=sigma;rho=rho}::res) s

in parse1 s
    
let load_line fn = 
  let ic = open_in fn in 
  let lc = ref 1 in 
  let ts = (Genlex.make_lexer
	      ["nservers";"nflows";"objective";"beginflows";
	       "endflows";"beginservers";"endservers";",";";";
	       "(";")";"[";"]";"server";"flow"])
      (comment_killer_line_counter lc (Stream.of_channel ic)) in
  let pe x = failwith (Printf.sprintf "parse error at line %d in file %s : %s.\n
			 " !lc fn x)
  in
  try
    let t = parse_line ts in
    close_in ic;
        (*Printf.printf "parsing OK\n";*)
    t
  with
    Parsing.Parse_error -> pe "Lexical error"
  |     (Stream.Error(_)|Stream.Failure) -> pe "Syntax error"

let ligne = load_line Sys.argv.(1);;	

(* Fonctions d'�criture des contraintes sur les arriv�es et les services *)
(* du syst�me, sans prendre en compte l'objectif *)

(* Ordre des dates *)

let genere_ordre_dates n oc = 
  Printf.fprintf oc "/* Contraintes sur les dates*/ \n \n"  ;
  for i = 1 to n do
    Printf.fprintf oc "t%d - t%d >=0; \n" i (i-1);
  done;;

(* Causalit� et croissance et d�but de p�riode d'activit�*)

let genere_variables_flot i flot oc = 
  Printf.fprintf oc "\n /* Contraintes de causalit� et croissance */ \n \n"  ;
  Printf.fprintf oc "f%ds%dt%d >= 0; /* positivit� */\n" 
    i (flot.debut-1) (flot.debut-1);  
  for j = flot.debut to flot.fin do
    (*croissance du processus des arriv�es *)
    Printf.fprintf oc "f%ds%dt%d >= f%ds%dt%d; /* croissance */\n" 
      i (flot.debut-1) j i (flot.debut-1) (j-1); 
    (*causalit� des processi(?) en tj*)
    Printf.fprintf oc "f%ds%dt%d >=f%ds%dt%d; /* causalit� */ \n" i (flot.debut-1) j i (j-1) j;
    Printf.fprintf oc "f%ds%dt%d >=f%ds%dt%d; /* causalit� */ \n" i (j-1) j i j j;
    (*Printf.fprintf oc "f%ds%dt%d >=f%ds%dt%d;\n" i (flot.debut-1) j i j j;*)
    (*croissance des processi fij en tj-1 et tj *)
    Printf.fprintf oc "f%ds%dt%d >=f%ds%dt%d; /* croissance */\n" i j j i j (j-1);
    (* D�but de p�riode charg�e en tj-1 dans le serveur j*)
    Printf.fprintf oc "f%ds%dt%d =f%ds%dt%d; /*d�but charge*/ \n" i (j-1) (j-1) i j (j-1);
  done;;


let genere_variables ligne oc = 
  for i= 1 to ligne.nflows do
    let flot=ligne.flows.(i-1) in
      genere_variables_flot i flot oc;
  done;;

(* Sur les arriv�es (sigma rho *)

let genere_sigma_rho flot deb fin sr oc =
  let sigma = sr.sigma in
  let rho = sr.rho in
    begin
      for i = deb to fin do
	for j = i to fin do
	  Printf.fprintf oc "f%ds%dt%d - f%ds%dt%d <= %f + %f t%d - %f t%d;\n"
	    flot (deb-1) j flot (deb-1) (i-1) sigma rho j rho (i-1);
	done;
      done;
    end;;

let rec genere_flot_alpha i alpha deb fin oc =
  match alpha with
      [] -> ()
    | h::t -> begin
	genere_sigma_rho i deb fin h oc;
	genere_flot_alpha i t deb fin oc;
      end;;

let genere_alpha ligne oc=
  for i= 1 to ligne.nflows do
    let flot = ligne.flows.(i-1) in
    let deb = flot.debut in
    let fin = flot.fin in
    let alpha = flot.alpha in
      genere_flot_alpha i alpha deb fin  oc;
  done;;

(* Sur les services *)

let rec genere_beta_serveur flots j beta oc = 
  match beta with
      [] -> ()
    | h::t -> 
	begin
	for i = 1 to (Array.length flots) do
	  let flot = flots.(i-1) in
	    if (flot.debut <= j) && (flot.fin >= j) then
	      Printf.fprintf oc " + f%ds%dt%d - f%ds%dt%d" i j j i j (j-1);
	done;
	  let l = h.latency in
	  let r = h.rate in
	    Printf.fprintf oc " >= %f t%d - %f t%d -%f;\n" r j r (j-1) (l*.r);
	    genere_beta_serveur flots j t oc;
	end;;


let genere_beta ligne oc =
  for j= 1 to ligne.nservers do
    genere_beta_serveur ligne.flows j (ligne.servers.(j-1)).beta oc;
  done;;

(* Objectif *)

let rec genere_contraintes_obj j j1 j2 alpha oc= 
  match alpha with
      [] -> ()
    | h::t -> begin
	Printf.fprintf oc "f%ds%du - f%ds%dt%d <= %f + %f u - %f t%d ;\n" 
	j (j1-1) j (j1-1)(j1-1) h.sigma h.rho h.rho (j1-1);
	Printf.fprintf oc "f%ds%dt%d - f%ds%du <= %f + %f t%d - %f u ;\n" 
	j (j1-1) j2 j (j1-1) h.sigma h.rho j2 h.rho;
	genere_contraintes_obj j j1 j2 t oc;
      end;;

let fonction_objectif ligne oc = 
  let j = snd ligne.objectif in 
    if ((fst ligne.objectif) == 'b') 
    then  (* burst maxi sur le serveur *)
      begin
	Printf.fprintf oc "max: ";
	let i = ref 0 in
	  while (!i < ligne.nflows) do
	    let k= ligne.flows.(!i) in 
	      (*Printf.printf "max:  %d %d %d \n"  j k.debut  k.fin;*)
	      if ((k.debut<= j) && (k.fin >= j))
	      then Printf.fprintf oc "+f%ds%dt%d - f%ds%dt%d" (!i+1) (j-1) (j) (!i+1) j j;
	      i := !i+1;
	  done;
	Printf.fprintf oc ";\n";
      end
    else   (* delai max sur le flot*)
      begin
	let j1 = ligne.flows.(j-1).debut in
	let j2 = ligne.flows.(j-1).fin in
	Printf.fprintf  oc "max: t%d - u;\n" j2;
	Printf.fprintf oc "f%ds%du-f%ds%dt%d >=0; \n" j (j1 -1) j j2 j2;
	Printf.fprintf oc "t%d <= u;\n" (j1-1);
	Printf.fprintf oc "u <= t%d;\n" j2;
	Printf.fprintf oc "f%ds%du-f%ds%dt%d >=0; \n " j (j1-1) j (j1-1) (j1-1);
	Printf.fprintf oc "f%ds%dt%d-f%ds%du >=0; \n " j (j1-1) j2 j (j1-1);
	genere_contraintes_obj j j1 j2 ligne.flows.(j-1).alpha oc;
      end;;


(* G�n�rer le programme lp_solve *)

let genere_fichier ligne lp = 
  (*let fic = Unix.openfile lp [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 
    0o640 in*)
  let oc = open_out lp in
    fonction_objectif ligne oc;
    genere_ordre_dates ligne.nservers oc;
    genere_variables ligne oc;
    genere_alpha ligne oc;
    genere_beta ligne oc;
    close_out oc;;

let () = 
  genere_fichier ligne Sys.argv.(2);;
