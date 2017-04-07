(* Pour la compilation : ocamlopt -pp camlp4o *)


(* Structure de données d'une ligne *)

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

(* Fonctions d'écriture des contraintes sur les arrivées et les services *)
(* du système, sans prendre en compte l'objectif *)

(* Générer un tableau de dates / matrice avec l'ordre *)

(*let m1 = Array.make_matrix (1) (1) 0;;*)

let genere_matrice_ordre m =
  let k = Array.length m in
  let matrice = Array.make_matrix (2*k) (2*k) 0 in
    for i=0 to k-1 do
	matrice.(2*i).(2*i+1) <- 1;
	for j=i+1 to k-1 do
	  if (m.(i).(j) == 1) then
	    begin
	      matrice.(2*i).(2*j) <-1;
	      matrice.(2*i+1).(2*j+1) <- 1;
	      matrice.(2*i).(2*j+1) <- 1;
	    end;
	done;
    done;
    matrice;;

(*let m2 = genere_matrice_ordre m1 1;;
let m4 = genere_matrice_ordre m2 2;;
let m8 = genere_matrice_ordre m4 4;;*)

(* genere les contraintes d'arrivee entre deux dates pour 1 flux *)
let rec genere_alpha i j k l alpha oc = 
  match alpha with
      [] -> ()
    | h::t -> begin
	Printf.fprintf oc "f%ds%dt%d - f%ds%dt%d <= %f + %f t%d - %f t%d;\n"
	    i (j-1) k i (j-1) l h.sigma h.rho k h.rho l;
	genere_alpha i j k l t oc;
      end;;

(*génération des contrainets beta*)
let rec genere_beta m flots j k l beta oc = 
  match beta with
      []->()
    | h::t -> begin
	for i=1 to m do
	  if (flots.(i-1).debut <= j) && (flots.(i-1).fin >= j) then
	    Printf.fprintf oc " + f%ds%dt%d - f%ds%dt%d" i j k i (j-1) l;
	done;
	let late = h.latency in
	let rate = h.rate in
	  Printf.fprintf oc " >= %f t%d - %f t%d -%f;\n" rate k rate l 
	    (late*.rate);
	  genere_beta m flots j k l t oc;
	end;;  
(*exponentiation entière*)
let rec expo n = match n with
    0-> 1
  | n -> 2*expo (n-1);;

(* Serveur par serveur, genération des contraintes *)

let contraintes_serveur ligne j ordre oc  =
  let n = ligne.nservers and flots = ligne.flows in
    begin
      Printf.fprintf oc "/* Contraintes serveur %d*/ \n\n" j;
      for k = 0 to  (expo (n-j+1))-1 do
	for l = k+1 to (expo (n-j+1))-1 do
	  if (ordre.(k).(l)==1) then
	    begin
	      Printf.fprintf oc "t%d - t%d >=0; \n" ((expo (n-j+1))+k) 
		(( expo (n-j+1)) +l);
	      (*Printf.fprintf oc "t%d - t%d >=0; \n" (2^(n-j)+k) (2^(n-j)+l);*)
	      for i = 1 to ligne.nflows do
		if (flots.(i-1).debut <= j) && (flots.(i-1).fin >= j) then
		  Printf.fprintf oc "f%ds%dt%d - f%ds%dt%d >=0;\n" i (j-1) 
		    ((expo (n-j+1))+k) i (j-1) ((expo (n-j+1))+l);
		if (flots.(i-1).debut == j) then
		  genere_alpha i j ((expo (n-j+1))+k) ((expo (n-j+1))+l) flots.(i-1).alpha oc;
	      done;
	    end;
	done;
      done;
      for k=0 to (expo (n-j))-1 do
	Printf.fprintf oc "t%d >= t%d;\n" ((expo (n-j))+k) (2*((expo (n-j))+k));
	for i = 1 to ligne.nflows do
	  if (flots.(i-1).debut <= j) && (flots.(i-1).fin >= j) then
	    Printf.fprintf oc "f%ds%dt%d = f%ds%dt%d;\n" i j ((expo (n-j))+k) i 
	      (j-1) (2*((expo (n-j))+k));
	done;
      genere_beta ligne.nflows flots j ((expo (n-j))+k) (2*((expo (n-j))+k)+1) ligne.servers.(j-1).beta oc;
      done;
    end;;
 
  
   
(* contraintes tous les serveurs *)
let genere_contraintes ligne oc = 
  let n = ligne.nservers in
  let ordre = ref (Array.make_matrix 1 1 0) in
    Printf.fprintf  oc "max: t1 - t%d;\n\n"  (expo n);
    for j=n downto 1 do
      ordre := genere_matrice_ordre !ordre;
      contraintes_serveur ligne j !ordre oc;
	
    done;;




 
(* Ordre des dates *)

let genere_ordre_dates n oc = 
  Printf.fprintf oc "/* Contraintes sur les dates*/ \n \n"  ;
  for i = 1 to n do
    Printf.fprintf oc "t%d - t%d >=0; \n" i (i-1);
  done;;

(* Causalité et croissance et début de période d'activité*)

let genere_variables_flot i flot oc = 
  Printf.fprintf oc "\n /* Contraintes de causalité et croissance */ \n \n"  ;
  Printf.fprintf oc "f%ds%dt%d >= 0; /* positivité */\n" 
    i (flot.debut-1) (flot.debut-1);  
  for j = flot.debut to flot.fin do
    (*croissance du processus des arrivées *)
    Printf.fprintf oc "f%ds%dt%d >= f%ds%dt%d; /* croissance */\n" 
      i (flot.debut-1) j i (flot.debut-1) (j-1); 
    (*causalité des processi(?) en tj*)
    Printf.fprintf oc "f%ds%dt%d >=f%ds%dt%d; /* causalité */ \n" i (flot.debut-1) j i (j-1) j;
    Printf.fprintf oc "f%ds%dt%d >=f%ds%dt%d; /* causalité */ \n" i (j-1) j i j j;
    (*Printf.fprintf oc "f%ds%dt%d >=f%ds%dt%d;\n" i (flot.debut-1) j i j j;*)
    (*croissance des processi fij en tj-1 et tj *)
    Printf.fprintf oc "f%ds%dt%d >=f%ds%dt%d; /* croissance */\n" i j j i j (j-1);
    (* Début de période chargée en tj-1 dans le serveur j*)
    Printf.fprintf oc "f%ds%dt%d =f%ds%dt%d; /*début charge*/ \n" i (j-1) (j-1) i j (j-1);
  done;;


let genere_variables ligne oc = 
  for i= 1 to ligne.nflows do
    let flot=ligne.flows.(i-1) in
      genere_variables_flot i flot oc;
  done;;

(* Sur les arrivées (sigma rho *)

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


(* Générer le programme lp_solve *)

let genere_fichier ligne lp = 
  (*let fic = Unix.openfile lp [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 
    0o640 in*)
  let oc = open_out lp in
    (*fonction_objectif ligne oc;
    genere_ordre_dates ligne.nservers oc;
    genere_variables ligne oc;
    genere_alpha ligne oc;
    genere_beta ligne oc;*)
    genere_contraintes ligne oc;
    close_out oc;;

let () = 
  genere_fichier ligne Sys.argv.(2);;
