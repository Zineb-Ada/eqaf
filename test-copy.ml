let myprintint i = Printf.printf "%d " i;;
let myprintbool i = Printf.printf "%b " i;;
let myprintchar i = Printf.printf "%c " i;;
let myprintstring i = Printf.printf "%s " i;;

module SS = Set.Make;;
(* let rec eucled a b =
  if b=0 then a else eucled b (a mod b);;

(* Chapter 3 *)
(* question 1 *)
(* correction *)
let not x = match x with
    true -> false
  | false -> true

(* question 2 *)

let rec add n = match n with
  1->1
  |_ -> n + add (n-1);;

(* question 3 *)
let power x n = match n with 
  1.->1.
  |_ -> (x ** n);;
(*  correction à retenir *)
let rec powercorrection x n = match n with
0->1
|1->x
|_ -> x * powercorrection x(n-1);;

(* question 5 *)
(* match 1 + 1 with 
2 -> match 2 + 2 with 
3 -> 4 
| 4 -> 5 *)
(* output -> 5 *)

(* question 6 *)
let islower x = match x with
'a'..'z'->true
|_ -> false;;

let isupper x = match x with 
'A'..'Z' -> true
|_ -> false;;


let rec getlength x = match x with
[]->0
|h::t-> 1+ getlength t;;

let rec addlist l = match l with
[]->0
|[e]->e
|h::t-> h + addlist t;;

let rec addlist2 l = match l with
[]->0
|h::t-> h+addlist2 t;;

(* get length of list *)
let rec length2 l n = match l with
[]->n
|h::t-> length2 t n+1;;

let rec addntol l n = match l with
[]->n
|h::t-> (h+n) + addntol t n;;


let rec addninl l n = match l with 
[]->[]
|[e]->[e+n]
|e::t->e+n:: addninl t n;; 

let rec foo l = match l with
h::_::t->h::foo t
|_->l;; *)


let rec addltol l1 l2 = match l1 with 
[]->l2
|h::t-> h:: addltol t l2;;

let rec take n l = 
  match n with 
  0->[]
  |_-> match l with 
    []->[]
    |h::t-> h:: take (n-1) t;;

let rec drop n l = match n with 
  0->l
  |_-> match l with 
    []->[]
    |h::t-> drop (n-1) t;;

(* answers chapter 4 *)
(* question 1 *)

let rec odd_element l = match l with
  []->[]
  |[e]->[]
  |e::_::t->e::odd_element t;;
  let rec even l = match l with 
  []->[]
  |[h]->[]
  |_::h::t->h:: even t;;
  (* question 2 *)
let rec counttrue n l = match l with 
  []->n
  |true::t-> counttrue (n+1) t
  |false::t-> counttrue n t;;

let rec counttrue l = match l with
[]->0
|true::t-> 1+ counttrue t
|false::t-> counttrue t;;


let rec countfalse l = match l with 
[]->0
|true::t->countfalse t
|false::t-> 1+ countfalse t;;

(* question 3 *)

let palindrome l = l @ List.rev l;;
let isapalindrome l = 
  let rec rev l = 
    match l with
    []->[]
    |h::t-> rev t @ [h] in
  if l = rev l then true else false;;

let isapalindrome2 l = match l with
    []->false
    |h::t-> let x = List.rev l in if x = l then true else false;;

  (* if List.length l 
  if n = (List.length l)/2 && m = (List.length l)/2 then true else *)
  (* if m <> List.length l then false else 
  if n <> 0 then false else  *)
 
let rec rev l = match l with 
  []->[]
  |h::t-> rev t @ [h];;



(* question 4 *)
  let drop_last l = 
    List.rev l |> List.tl |> List.rev;;
  let rec drop_last2 n l = 
    if n >= List.length l then [] else 
    if n != List.length l -1 then drop_last2 (n+1) l else
    if n = List.length l -1 then  List.rev l |> List.tl |> List.rev else [];;
  
  let rec drop_last3 l = match l with 
      []->[]
      |[_]->[]
      |h::t-> h:: drop_last3 t;;

(* question 5 *)
  let rec isinlist n l= 
    match l with 
    []->false
    |h::t-> if h = n then true else isinlist n t;;

  let rec isinlist2 n l = match l with 
      []->false
      |h::t-> h=n || isinlist2 n t;;

  (* question 6 *)

  let rec make_set l = 
    match l with
      []->[]
      |h::t-> if isinlist h t then make_set t else h::make_set t;;

    
(* Chapter 6 *)

let rec sort l = 
  let rec insert x l = match l with
    []->[x]
    |h::t-> if x<=h then x::h::t else h::insert x t in
  match l with
    []->[]
    |h::t-> insert h (sort t);;

(* question 3 chapter 5 *)

  let rec sortrev l = 
    let rec insert x l = match l with
      []->[x]
      |h::t-> if h<=x then x::insert h t else h::insert x t in
    match l with 
      []->[]
      |h::t-> insert h (sortrev t);;

  (* question 4  *)

  let issortedlist l = match l with 
  []->true
  |h::t-> if l = sort l then true else false;;

  let rec double l = match l with
    []->[]
    |h::t-> h*2:: double t;;
  
  let rec isevenorodd l = match l with 
    []->[]
    |h::t->  (h mod 2 = 0) :: isevenorodd t;;

  let rec map f l = match l with 
    []->[]
    |h::t-> f h:: map f t;;
  
  let isevenorodd2 x = x mod 2 = 0;;
  map isevenorodd2 [1; 2; 3; 4];;

let isevenorodd3 l = map (fun x-> x mod 2 = 0) l;;

  (* match l with 
    []->[x]
    |h::t-> *)

(* let () = List.iter myprint (isapalindrome 0 (List.length l) l);; *)

(* Chapter 6 *)
(* question 1 *)

let rec replaceexclamations l = match l with 
  []->[]
  |h::t-> if h = '!' then '.'::replaceexclamations t else h::replaceexclamations t;;

let rec replaceexclamations2 l = match l with 
    []->[]
    |'!'::t->'.'::replaceexclamations2 t
    |h::t->h::replaceexclamations2 t;;

let replaceexclamations2 x = if x = '!' then '.' else x;;
map replaceexclamations2 ['H'; 'e'; 'l'; 'p'; '!'; ' '; 'F'; 'i'; 'r'; 'e'; '!'];;

let replaceexclamations3 l = map (fun x -> if x = '!' then '.' else x) l;;

(* question 2 *)

let rec clip l = match l with
    []->[]
    |h::t-> if h >10 then 10::clip t else if h<1 then 1:: clip t else h::clip t;;

let rec clip2 x = if x>10 then 10 else if x<1 then 1 else x;;
let cliplist l = map clip2 l;;

(* question 3 *)

let rec cliplist2 l = map (fun x -> if x>10 then 10 else if x<1 then 1 else x) l;;

(* question 4 *)

let imnotinspired x = x;;
let rec apply f n m = match n with
0-> m
|_-> f(apply f (n-1) m);;
let power a b =
  apply (fun x -> x * a) b 1;;
(* 1-> f(m)
|_-> f(apply f (n-1) m);; *)


(* question 5 *)

(* >= *)
let greater x y = x >=y;;
let smaller x y = x<=y;;
let rec modifysort f l = 
  let rec insert f x l = match l with
    []->[x]
    |h::t-> if f h x then x::h::t else h::insert f x t in 
  match l with 
    []->[]
    |h::t-> insert f h (modifysort f t);;

(* question 6 *)

let firstfun x = x mod 3 = 0;;
let rec filter f l = match l with
  []->[]
  |h::t-> if firstfun h = true then  h:: filter f t else filter f t;;

let rec filter1 l = 
  let firstfun x = x mod 3 = 0 in
  match l with
  []->[]
  |h::t-> if firstfun h = true then  h:: filter1 t else filter1 t;;

let rec filter2 f l = match l with 
  []->[]
  |h::t-> if f h then h::filter2 f t else filter2 f t;;

filter2 (fun x -> x mod 3 = 0) [2; 5; 3; 9; 6; 12];;

  (* quetion 7 *)
(* cmp = <= *)
let rec for_all cmp l = 
  let rec firstone cmp x = cmp x 10 in
    match l with 
      []->true
      |h::t-> if (firstone cmp h) then for_all cmp t else false;;
let rec for_all2 f l = match l with 
  []-> true
  |h::t-> if f h then for_all2 f t else false;;

for_all2 (fun x -> x<=10) [1; 3; 5; 10; 20];;


let l = [[1; 2; 3]; [2; 3; 4]];;


let rec mapl f l = 
    match l with 
      []->[]
      |h::t-> map f h:: mapl f t;;

mapl (fun x -> x>=5) [[1; 2; 3]; [2; 3; 4]];;


(* chapter 7 *)

let rec takewithexeption n l = match l with
  []-> if n=0 then [] else raise (Invalid_argument "takewithexeption")
  |h::t-> if n<0 then raise (Invalid_argument "takewithexeption") else if n=0 then [] else h::takewithexeption (n-1) t;;

let rec dropwithexeption n l = match l with 
  []-> if n=0 then [] else raise (Invalid_argument "drop")
  |h::t-> if n<0 then raise (Invalid_argument "drop") else if n = 0 then l else dropwithexeption (n-1) t;;


let f x y = try x/y with
  Division_by_zero ->0;;

  (* Question 1 *)

let rec smallest l = match l with
  []->0
  |[x]-> if x < 0 then raise (Not_found) else x
  |h::t-> if h<0 then raise (Not_found) else if h < smallest t && h>=0 then h else smallest t;;
  (* |h::t-> if h<0 then raise (Not_found) else if h < smallest t then h else smallest t;; *)

let rec smallest_or_zero l = try smallest l with 
  Not_found -> 0;;
    (* match l with
  []->0
  |h::t-> smallest (smallest_or_zero t);; *)
  (* |_-> smallest l;; *)
    (* match l with 
    []->0
    |h::t-> smallest_or_zero (smallest t) t;; *)
(* if smallest l = raise(Not_found) then 0 else smallest_or_zero (smallest h) t;; *)

(* Chapter 8 *)

(* Question 1 *)
let ld = [(1, 4); (2, 2); (3, 2); (4, 3); (5, 1); (6, 2)];;
let rec numberofkeys l = List.length l;;
let rec numberofkeys1 l = match l with 
  []->0
  |h::t-> 1+ numberofkeys1 t;;

(* Question 2 *)

let rec replace a b l = match l with
  []-> raise (Not_found)
  |(x, y)::t-> if x = a then (a, b)::t else (x, y)::replace a b t;;

(* Question 3 *)

let rec builddistionnary l1 l2 = if List.length l1 != List.length l2 then raise (Invalid_argument "builddictionnary") else
    match l1, l2 with
  [], l2-> []
  |l1, []-> []
  |hx::tx, hy::ty-> (hx, hy):: builddistionnary tx ty;;

(* Question 4 *)

let rec splitdictionnary l e1 e2 = match l with 
  []->(List.rev e1, List.rev e2)
  |(a, b)::t-> splitdictionnary t (a::e1) (b::e2) ;;

(* let rec split_inner1 l = match l with 
  []->[]
  |h::t-> List.rev h;;

let split_inner2 l = match l with 
  []->[]
  |h::t-> t;;

let split_inner3 l = match split_inner2 l with
  []->[]
  |h::t-> List.rev h;;

(* let splitdictionnary l1 l2 = match l1, l2 with 
  [], l2-> l2
  |l1, []-> l1
  |_-> ;; *)

let rec splitinner l e1 e2 = match l with
  []->(e1, e2)
  |(a, b)::t-> splitinner t (a::e1) (b::e2);;

  let rec splitinner1 l e1 e2 = match l with
    []-> e1, e2
    |h::t-> if h mod 2 = 0 then (splitinner1 t (h::e1) e2) else splitinner1 t e1 (h::e2);;
   *)

(* Question 5 *)

let rec member n l = match l with
  []-> false
  |h::t-> if n = h then true else member n t;;
let rec listtodic_inner keyslist l = match l with
  []->[]
  |(a, b)::t-> if (member a keyslist)=true then listtodic_inner keyslist t else (a,b)::listtodic_inner (a::keyslist) t;;

let listtodic l = listtodic_inner [] l;;

(* Question 6 *)
let rec replace a b l = match l with
  []-> [(a, b)]
  |(x, y)::t-> if x = a then (a, b)::t else (x, y)::replace a b t;;
let rec member2 n l = match l with
  []-> false
  |(a, b)::t-> if n = a then true else member2 n t;;
let rec union_inner a b l = match l with
  []->[(a, b)]
  |(e, c)::t-> if member2 a l then union_inner e c (replace a b t) else (a,b)::union_inner e c t;;
    
let rec union l1 l2 = match l1 with
  []->l2 
  |(a,b)::t-> union t (union_inner a b l2);;

let rec verifykeys l1 l2 = match l2 with
  []-> l1
  |(e,b)::t-> if member2 e l1 then verifykeys l1 t else verifykeys ((e,b)::l1) (t);;
 

(* Chapter 9 *)
let rec map f l = match l with
  []->[]
  |h::t-> f h::map f t ;;
let add x y = x+y ;;
let f = add 6;;

map (fun x -> x*2) [1; 2; 3; 4];;
map (( * )2) [1; 2; 3; 4];;

let rec mapl f l = match l with
 []->[]
 |h::t-> map f h::mapl f t;;
let rec mapl2 f l = map (map f) l;;
let rec mapl3 f = map (map f);; 
  (* we call the function mapl3 with the f and a list of lists  mapl3 (( * )2) listoflists;;*)
 
let add2 = (fun x -> fun y-> x+y);;

(* Question 1 *)
let add3arguments x y z = x+y+z;;
let add3arguments1 = (fun x -> fun y -> fun z -> x+y+z);;
let add4 = add3arguments 4;; 
    (* (on appelle => add4 2 3) *)

(* Question 2 *)

let member_all_inner x = map (member x) 

let member_all x ls = 
    not (member false (member_all_inner x ls));;

(* Question 3 *)
let div x y = y / x
let rec halve l = map (div 2) l;;
(* Questio 4 *)
let mapll f = map (map (map f));;
  mapll (( * )2) [[[1; 2; 3; 4]; [5; 6; 7; 8]]; [[1; 2; 3; 4]; [5; 6; 7; 8]]];;

(* Question 5 *)

let rec truncate x l = 
   match l with 
    []->[]
    |h::t-> if List.length l <=x then l else truncate x t;;
let trunc_all x ls = map (truncate x) ls;;

(* Question 6 *)

let returnfirstintfromlist_inner x l = match l with
  []->x
  |h::_-> h;;

let returnfirstintfromlist x l = map (returnfirstintfromlist_inner x) l;;

(* Chapter 10 *)
(* create new type *)
type colour = Red| Green|Blue|Yellow;;
(* we can build a list from theses colors *)
let col = Blue;;
let cols = [Blue; Yellow];;
let colpai = ('R', Red);;
(* type all the RGB colors *)
type colour = Red| Green|Blue|Yellow|RGB of int*int*int;;
let cols = [Red; Green; Blue; Yellow; RGB(150, 0, 255)];;

let components c = match c with
  Red -> (255, 0, 0)
  | Green -> (0, 255, 0)
  | Blue -> (0, 0, 255)
  | Yellow -> (255, 255, 0)
  | RGB (r, g, b) -> (r, g, b);;

let rec lookup_opt n l = match l with 
  []-> None
  |(a, b)::t-> if n = a then Some b else lookup_opt n t;;

type 'a sequence = Nil | Cons of 'a * 'a sequence;;
let rec length l = match l with 
  Nil->0
  |Cons(_, t) -> 1 + length t;;
let rec append a b = match a with
  Nil->b
  |Cons(h, t)-> Cons(h, append t b);;

(* answers to questions chapter 10 *)
(* Question 1 *)

type rect = Rectangles of int*int| Squares of int;;

(* Question 2 *)

let areaofrect a = match a with 
  Rectangles (w, h)-> w*h
  |Squares w-> w*w;;

(* Question 3 *)

let rotatesrect a = match a with 
  Rectangles (w,h)-> if w>h then Rectangles (h,w) else Rectangles (w,h)
  |Squares _ -> a;;

(* Question 4 *)

(* first method return a list of width's rects
let rec smallestwidthoflist_inner rl a el = match rl with 
  []->el
  |Rectangles (w, h)::t-> if w>a then smallestwidthoflist_inner t a (w::el) else smallestwidthoflist_inner t w (a::el)
  |Squares w::t-> if w>a then smallestwidthoflist_inner t a (w::el) else smallestwidthoflist_inner t w (a::el);;

let smallestwidthoflist_inner2 f = List.sort compare f;; *)

(* seconde method return a list of width's rects
let rec smallestwidthoflist rl = 
  let rec smallestwidthoflist_inner n l = match l with 
      []->[n]
      |h::t-> if h<n then h::n::t else n::h::t in 
    match rl with
      []->[]
      |Rectangles (w, h)::t-> smallestwidthoflist_inner w (smallestwidthoflist t)
      |Squares w::t-> smallestwidthoflist_inner w (smallestwidthoflist t);; *)

(* third method *)

let width_of_rect r = match r with
  Rectangles (w, _)-> w
  |Squares w -> w;;

let rect_compare a b = width_of_rect a < width_of_rect b;;
let rec sort f l = 
  let rec insert x l = match l with
    []->[x]
    |h::t-> if f x h then x::h::t else h:: insert x t in 
  match l with 
    []->[]
    |h::t-> insert h (sort f t) ;; 

let smallest_width_of_rectlist rects = sort rect_compare (map rotatesrect rects);;


(* Question 5 *)

let rec takeseq n l = match n with 
    0->Nil
  |_->  
  match l with
    Nil-> raise (Invalid_argument "takeseq")
    |Cons(h, t)-> Cons (h, takeseq (n-1) t);;

let rec dropseq n l = match n with
    0->l 
    |_->
  match l with
    Nil-> raise (Invalid_argument "dropseq")
    |Cons(h, t)-> dropseq(n-1) t;;

let rec mapseq f l = match l with 
    Nil->Nil
    |Cons(h, t)-> f (Cons(h, mapseq f t));;


(* Question 6 *)

type expr = Num of int
| Add of expr * expr
| Subtract of expr * expr | Multiply of expr * expr | Divide of expr * expr
| Power of expr * expr;;

let rec evaluate e = match e with 
    Num x -> x
    |Add (a, b)-> evaluate a + evaluate b 
    |Subtract (a, b)-> evaluate a - evaluate b 
    | Multiply (a, b)-> evaluate a * evaluate b
    |Divide (a, b) -> evaluate a / evaluate b
    |Power (a,b)-> power (evaluate a) (evaluate b);;
(* Question 7 *)
let evaluate_opt e = try Some (evaluate e)  with 
  Division_by_zero-> None


(* Chapter 11 *)
type 'a tree = Br of 'a * 'a tree * 'a tree| Lf;;
  
let rec size tr = match tr with 
    Br (_, l, r)-> 1 + size l + size r
    |Lf-> 0;;
let rec total tr = match tr with
    Br(x, l, r)-> x + total l + total r
    |Lf->0;;

let max x y = if x>y then x else y;;
let rec max_depth tr = match tr with 
  Br(_, l, r)-> 1+ max (max_depth l) (max_depth r)
  |Lf-> 0;;

let rec list_of_tree tr = match tr with 
    Br (x, l, r)-> list_of_tree l @ [x] @ list_of_tree r
    |Lf->[];;

let rec tree_map f tr =match tr with 
    Br(x, l, r)-> Br( f x, tree_map f l, tree_map f r)
    |Lf->Lf;;     

let rec lookuptree tr k = match tr with 
    Lf-> raise (Not_found)
    |Br((e, v), l, r)-> if e=k then v 
    else if k<e then lookuptree l k 
    else lookuptree r k;;

let rec lookuptree_opt tr k = match tr with 
    Lf-> None 
  |Br((e, v), l, r)-> if e=k then Some v
  else if k<e then lookuptree_opt l k
  else lookuptree_opt r k;;


(* Revoir inserttree *)
let rec inserttree tr k v = match tr with 
  Lf-> Br((k, v), Lf, Lf)
  |Br((k', v'), l , r)-> if k = k' then Br((k, v), l, r) 
    else if k<k' then Br((k', v'), inserttree l k v, r) 
    else Br((k', v'), l, inserttree r k v);;

(* Question 1 *)

let rec ifinthetree e tr = match tr with 
    Br (x, l, r)-> e = x || ifinthetree e l || ifinthetree e r
    |Lf-> false;;

(* Question 2 *)

let rec fliptree tr = match tr with 
  Lf->Lf
  |Br(x, l, r) -> Br (x, fliptree r, fliptree l);;

(* Question 3 *)
let rec ifhavethesameshape tr1 tr2 = match tr1, tr2 with 
    Lf, Lf-> true
    |Br(_, l1, r1), Br(_, l2, r2)-> ifhavethesameshape l1 l2 && ifhavethesameshape r1 r2
    |_, _-> false

(* Question 4 *)

let rec inserttree tr k v = match tr with 
  Lf-> Br((k, v), Lf, Lf)
  |Br((k', v'), l, r)-> if k = k' then Br((k, v), l, r) 
  else if k<k' then Br((k', v'), inserttree l k v, r) 
  else Br((k', v'), l, inserttree r k v);; 
let rec tree_of_list l = match l with 
  []-> Lf
  |(k, v)::t-> inserttree (tree_of_list t) k v;;


(* Question 5 *)

let rec combine_two_dictionnaries d1 d2 = match d1, d2 with 
    Lf, Lf-> Lf
    |Br((k1, v1), l1, r1), Br ((k2, v2), l2, r2)-> if lookuptree d2 k1 
      then inserttree (combine_two_dictionnaries l1 d2) k1 v1 
      else inserttree (combine_two_dictionnaries r1 d2) k1 v1
    |_, _-> combine_two_dictionnaries d1 d2;;

let combine_two_dictionnaries d1 d2 = tree_of_list (list_of_tree d1 @ list_of_tree d2);;
 
(* Question 6 *)

type 'a mtree = Branch of 'a * 'a mtree list;;

let rec sum l = match l with 
      []->0
      |h::t-> h+sum t;;
let rec sizemtree tr = match tr with 
    |Branch(e, l) -> 1 + sum (map sizemtree l);;

let mtreeslist = (Branch (2, [Branch (3, [ Branch (3, [])])]));;

let rec totalmtree (Branch (e, l)) = e + sum ( map totalmtree l);;

let rec mapmtree f (Branch (e, l)) = Branch (f e, map (mapmtree f) l);;

(* Chapter 12 *)
let print_dict_entry (k, v) = print_int k; print_newline (); print_string v; print_newline ();;
let rec print_dict d = match d with
[]-> ()
|h::t-> print_dict_entry h; print_dict t;; 

let rec iter f l = match l with 
      []->()
      |h::t->f h; iter f t;;

let print_dict1 l = iter print_dict_entry l;;
let print_dict2 = iter print_dict_entry;;


let rec read_dict () = let i = read_int () in
if i = 0 then [] else
let name = read_line () in
        (i, name) :: read_dict ();;


 let rec read_dict () = try
  let i = read_int () in if i = 0 then [] else        
    let name = read_line () in (i, name) :: read_dict ()      
   with
    Failure _ ->
      print_string "This is not a valid integer. Please try again.";
      print_newline ();
      read_dict ();;        

             
       
let entry_to_channel ch (k, v) = 
  output_string ch (string_of_int k); 
  output_char ch '\n';
  output_string ch v;
  output_char ch '\n';;

let dictionary_to_channel ch d = iter (entry_to_channel ch) d;;
let dictionary_to_file filename dict = let ch = open_out filename in
    dictionary_to_channel ch dict;
    close_out ch;;
(* print_dict2 [(1, "non"); (2, "oui"); (3, "ok")];; *)
(* dictionary_to_file "file.txt" (read_dict ());; *)


let entry_of_channel ch =
  let number = input_line ch in let name = input_line ch in
        (int_of_string number, name);;
  let rec dictionary_of_channel ch = try
  let e = entry_of_channel ch in e :: dictionary_of_channel ch
  with
      End_of_file -> [];;
let dictionary_of_file filename = 
  let ch = open_in filename in
  let dict = dictionary_of_channel ch in close_in ch;
dict;;
(* dictionary_of_file "table.txt";; *)
(* Question 1 *)
let rec printintlist l = 
    match l with
  []-> print_char ']'
  |[e]-> print_int e; printintlist []
  |h::t-> print_int h; print_char ';'; print_char ' '; printintlist t;;

(* let printchar0 = print_char '['; printintlist [1; 2; 3; 4; 5];; *)
(* seconde method *)
let rec printintlist_inner l = match l with 
  []->()
  |[h]->print_int h
  |h::t-> print_int h; print_string "; "; printintlist_inner t;;
let printintlist1 l = print_string "["; printintlist_inner l; print_string "]"; print_newline ();;
(* printintlist1 [1; 2; 3; 4; 8];; *)

(* Question 2 *)

let rec read_int_fromuser () = try 
  print_string ("type three integers from 0 to 9, press enter after each"); 
  print_newline ();
  let a = read_int () in 
    let b = read_int () in 
      let c = read_int () in 
        (a, b, c) 
with 
  Failure _ -> print_string "failed to read integer please try again"; print_newline (); read_int_fromuser();;

(* read_int_fromuser ();; *)

(* Question 3 *)
let rec read_dict_inner n = 
  if n = 0 then [] else
  try 
let i = read_int () in if i = 0 then [] else 
  let name = read_line () in (i, name) :: read_dict_inner (n-1) with
  Failure _-> 
    print_string ("This is not an integer please type a number from 0 to 9"); 
    print_newline ();
    read_dict_inner n;;

exception BadNumber;;
let rec read_dict2 () = 
  print_string "How many entries?"; 
  print_newline ();
  try let n = read_int () in
    if n < 0 then raise BadNumber else 
      read_dict_inner n
    with 
      Failure _-> print_string "not an integer type another one"; 
      print_newline (); read_dict2 ()
      |BadNumber-> print_string "not a valid number"; 
      print_newline (); read_dict2 ();;
(* read_dict2 ();; *)

(* Question 4 *)

let rec maprint f l = match l with
    []->[]
    |h::t-> print_int h; print_newline (); maprint f t;;
let rec printinteger_inner n = match n with 
    0->[]
    |_-> printinteger_inner (n-1) @ [n];;

let rec print_matrice_inner ch n = 
    iter (fun x ->
      iter (fun i ->
        output_string ch (string_of_int i);
        output_string ch "\t") 
      (map (( * ) x) (printinteger_inner n));
      output_string ch "\n"
    )
    (printinteger_inner n);;
(* print_matrice_inner stdout 5;; *)

(* print_matrice (rev_list 5) 1;; *)
(* printinteger (List.rev (printinteger_inner 5));; *)

exception FileProblem;;
let matrice_to_file filename n = 
  if n < 0 then raise (Invalid_argument "matrice_to_file") else 
    try
      let ch = open_out filename in 
        print_matrice_inner ch n; 
        close_out ch
    with 
      FileProblem -> raise FileProblem;;

(* matrice_to_file "table.txt" 8;; *)

(* Question 5 *)
let rec number_of_lines_inner ch =
  try 
    let _ = input_line ch in
    1 + number_of_lines_inner ch 
  with
    End_of_file -> 0;;
let number_of_lines filename =
  try 
    let ch = open_in filename in 
      let table = number_of_lines_inner ch in
        close_in ch;
        table
  with
    _-> raise (Failure "number_of_lines")
;;

(* print_int (number_of_lines "table.txt");; *)

(* Question 6 *)
exception CopyFailed
let rec copy_file_inner fromch toch = 
  try 
    output_string toch (input_line fromch);
    output_string toch "\n";
    copy_file_inner fromch toch
with 
  End_of_file -> ();;

let rec copy_file fromfile tofile = try 
  let fromch = open_in fromfile in 
    let toch = open_out tofile in
      copy_file_inner fromch toch;
  close_in fromch;
  close_out toch
  with
    _-> raise CopyFailed;;

(* copy_file "table.txt" "copytable.txt";; *)

(* Chapter 13 *)

let x = ref 1;; 
 (* (for change the value of x) *)
x := 50;;
(* change x from ref to int *)
let p = !x;;

let swap a b = let t = !a in 
  a:= !b; b:= t;;
let swap2 a b = let t = !a in 
  if t = 5 then 
    (* using begin and end for readability *)
    begin b := 0 end 
else print_int !a; print_int !b; print_newline(); print_int t;; 
(* swap2 (ref 5) (ref 6);; *)
(* let loop = for x = 1 to 10 do print_int x; print_newline () done;; *)
(* loop;; *)
let smallest_pow2 x = 
  let t = ref 1 in
    while !t < x do 
      t := !t *2;
    done;
  print_int !t ; print_newline ();;

(* smallest_pow2 37;; *)


let paragraph = "One morning, when Gregor Samsa woke from troubled dreams, he found
himself transformed in his bed into a horrible vermin.  He lay on
his armour-like back, and if he lifted his head a little he could
see his brown belly, slightly domed and divided by arches into stiff
sections.  The bedding was hardly able to cover it and seemed ready
to slide off any moment.  His many legs, pitifully thin compared
with the size of the rest of him, waved about helplessly as he
looked."

(* on crée un channel *)
let print_paragraph_inner ch p = 
    output_string ch p;;

(* On crée un fichier *)
exception FileProblem;;
let paragraph_to_file filename p = 
    try
      let ch = open_out filename in 
        print_paragraph_inner ch p; 
        close_out ch
    with 
      FileProblem -> raise FileProblem;;

(* paragraph_to_file "gregor.txt" paragraph;; *)

(* Question 5 *)
let number_of_lines ch = 
  let lines = ref 0 in 
    try 
      while true do
      let _ = input_line ch in 
        lines := !lines + 1
      done
    with 
      End_of_file-> 
        print_string "There were "; 
        print_int !lines;
        print_string "lines.";
        print_newline ();;

let number_of_lwsc ch = 
  let lines = ref 0 in
  let characters = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  try
    while true do 
      let line = input_line ch in 
        lines := !lines +1;
        characters := !characters + String.length line; 
        String.iter 
          (fun c ->
            match c with 
            '!' | '?' | '.' -> sentences := !sentences +1
            |' ' -> words := !words + 1
            |_ -> ())
          line
    done 
  with 
    End_of_file->
      print_string "There were ";
        print_int !lines;
        print_string " lines, making up ";
        print_int !characters;
        print_string " characters with ";
        print_int !words;
        print_string " words in ";
        print_int !sentences;
        print_string " sentences.";
        print_newline ();;

    
let file filename = 
  let ch = open_in filename in 
    try
      number_of_lwsc ch; 
      close_in ch;
    with
      _-> close_in ch;;

(* file "gregor.txt";; *)

(* array in ocaml *)
(* accéder à un élément du tableau *)
let array = [|1; 2; 3; 4; 5|];;
array.(3);;
(* pour changer un élément par son index dans le tableau *)
array.(1) <- 22;;

(* initialiser un tableau *)
let create_bool_array = Array.make 6 true;; 
let create_char_array = Array.make 10 'A';;
let create_arrays_in_array = Array.make 4 (Array.make 4 6);;

let i = int_of_char 'a';;

let print_histogram arr = 
  print_string "Character frequencies:"; 
  print_newline ();
  for x = 0 to 255 do 
    if arr.(x) > 0 then 
      begin
        print_string "For character '";
        print_char (char_of_int x);
        print_string "' (character number ";
        print_int x;
        print_string ") the count is ";
        print_int arr.(x);
        print_string ".";
        print_newline ()
      end
  done;;
(* print_histogram [|97; 98; 99|];; *)
  let number_of_lwsc ch = 
    let lines = ref 0 in
    let characters = ref 0 in
    let words = ref 0 in
    let sentences = ref 0 in
    let histogram = Array.make 256 0 in
    try
      while true do 
        let line = input_line ch in 
          lines := !lines +1;
          characters := !characters + String.length line; 
          String.iter 
            (fun c ->
              match c with 
              '!' | '?' | '.' -> sentences := !sentences +1
              |' ' -> words := !words + 1
              |_ -> ())
            line;
          String.iter 
              (fun c -> 
                let i = int_of_char c in
                histogram.(i) <- histogram.(i) + 1)
                line;
      done; 
      
    with 
      End_of_file->
        print_string "There were ";
          print_int !lines;
          print_string " lines, making up ";
          print_int !characters;
          print_string " characters with ";
          print_int !words;
          print_string " words in ";
          print_int !sentences;
          print_string " sentences.";
          print_newline ();
          print_histogram histogram;;


let file filename = 
  let ch = open_in filename in 
    try
      number_of_lwsc ch; 
      close_in ch;
    with
      _-> close_in ch;;
(* file "gregor.txt";; *)
(* myprintchar (char_of_int 32);; *)
  
(* print_int array.(4);; *)
(* Array.iter print_int create_array;; *)

(* Question 1 *)

let x = ref 1 in let y = ref 2 in x := !x + !x; y := !x + !y; !x + !y;;
(* the output is 6 *)

(* Question 2 *)
let y = [ref 5; ref 5];;

let x = ref 5 in [x; x];;
(* the both have type int ref list but x 
for y if we change one reference the other it doesn't change for 
  but for x if we change a reference the other going to be change to  *)

(* Question 3 *)
let loop x = while x > ref 1 && x < ref 34 do 
  print_int (!x); print_newline ();
  x := (!x+1)
done;;

(* loop (ref 33);; *)

(* Question 4 *)
[|1; 2; 3|];;
(* int array *)
[|true; false; true|];;
(* bool array *)
[|[|1|]|];;
(* int array array  *)
[|[1; 2; 3]; [4; 5; 6]|];;
(* int list array  *)
[|1; 2; 3|].(2);;
(* int *)
[|1; 2; 3|].(2) <- 4;;
(* unit *)

(* Question 5 *)
(* first method *)
let sum_array arr = 
  Array.fold_left (+) 0 arr;;
(* for construct method with ref *)
let sum_array1 arr = 
  let sum = ref 0 in 
    for i = 0 to Array.length arr -1 do
      sum:= !sum + arr.(i)
    done;
    !sum;;

(* print_int (sum_array [|1; 2; 3; 4; 5|]);; *)

(* Question 6 *)
let array_rev arr = 
 let j = ref (Array.length arr -1) in
    for i = 0 to (((Array.length arr) /2)-1) do 
      let h = arr.(i) in 
      let t = arr.(!j) in 
      arr.(i) <- t;
      arr.(!j) <- h;
      j:= !j - 1
    done; 
    Array.iter print_int arr;;
(* array_rev [|1; 2; 3; 4; 5|];; *)

(* Question 7 *)

let multiplication_table  n = 
  let arr = Array.make_matrix !n !n 5 in 
      for i = 0 to Array.length arr -1 do 
        let m = ref 1 in
        for j = 0 to Array.length arr.(i) -1 do 
          arr.(i).(j) <- (i+1) * !m;
          m := !m + 1
        done;
      done;
    arr;;
  (* Array.iter (Array.iter print_int) (multiplication_table (ref 5));; *)

  (* Question 8 *)

let uppercase c = 
  if int_of_char c >= 97 && int_of_char c <= 122 
    then char_of_int (int_of_char c -32) else c;;

let lowercase c = 
  if int_of_char c >= 65 && int_of_char c <= 90 
    then char_of_int (int_of_char c +32) else c;;  

(* print_char (uppercase 'h' );; *)

(* Question 9 *)
(* 
Compter les mots en comptant les espaces est inexact - une ligne de dix mots n'en comptera que neuf *)

let number_of_lwsc2 ch = 
  let lines = ref 0 in
  let characters = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  let histogram = Array.make 256 0 in
  try
    while true do 
      let line = input_line ch in 
        lines := !lines +1;
        characters := !characters + String.length line; 
        String.iter 
          (fun c ->
            match c with 
            '!' -> sentences := !sentences +1
            |'?'  -> sentences := !sentences +1
            |'.' -> sentences := !sentences +1
            |' ' -> words := !words + 1
            |'\n'-> characters := !characters + 1 
            |_ -> ())
          line;
          String.iter 
          (fun c ->
            match c with 
            '!'|'?'|'.'|' '|'\n' -> words := !words + 1
            |_ -> ())
          line;
        String.iter 
            (fun c -> 
              let i = int_of_char c in
              histogram.(i) <- histogram.(i) + 1)
              line;
    done 
  with 
    End_of_file->
      print_string "There were ";
        print_int !lines;
        print_string " lines, making up ";
        print_int !characters;
        print_string " characters with ";
        print_int !words;
        print_string " words in ";
        print_int !sentences;
        print_string " sentences.";
        print_newline ();
        print_histogram histogram;;


let file2 filename = 
let ch = open_in filename in 
  try
    number_of_lwsc ch; 
    close_in ch;
  with
    _-> close_in ch;;
(* file "gregor.txt";; *)

(* Chapter 14 *)
(* Question 1 *)

let nearest_number_inner n = 
  (* match string_of_float n with *)
  let m = string_of_float n in
  let emptystring = ref 0 in
    for i = 0 to String.length (string_of_float n ) -1 do 
      if m.[i] = '.' then emptystring := int_of_char m.[i+1] 
      else ()
    done;
    char_of_int !emptystring;;

let nearest_number n = if nearest_number_inner n >= '5' then ceil n else floor n;;

(* the book method *)
let nearest_number1 n = 
  let c = ceil n in 
    let f = floor n in 
      if c -. n < n -. f then c else f;; 
(* print_float (nearest_number 3.7);; *)

(* Question 2 *)
let equidistance_point (a, b) (a', b')= 
    ((a +. a') /. 2., (b +. b') /. 2.);;
(* equidistance_point (2., 4.) (4., 3.);; *)

(* Question 3 *)
let rec separate_float n = 
  if n < 0. then 
    let (a, b) = separate_float (-. n) in 
      (-. a, b) 
    else 
    let firstparty = floor n in
    let secondeparty = (n -. firstparty) in
      (firstparty, secondeparty);;
(* separate_float 33.55;; *)

(* question 4 *)

let star x =
let i = int_of_float (floor (x *. 50.)) in
let i' = if i = 50 then 49 else i in
for x = 1 to i' - 1 do print_char ' ' done; 
print_char '*';
print_newline ();;

(* star 0.2;; *)

(* Question 5 *)
let pi = 4.0 *. atan 1.0;;
let plot f a b ss = 
  let starposition = ref a in 
    while !starposition <= b do
      star (f !starposition);
      starposition := !starposition +. ss
    done;;
(* plot sin 0. pi (pi /. 20.);;;; *)

(* Chapter 15 *)

(* Question 1 *)
let rec concatlist ll l= 
  match ll with 
    [] -> l
    |hl::tl -> 
       match hl with  
          [] -> concatlist tl l
          |h::t -> concatlist (t::tl) (l @ [h]);;
(* List.iter (print_string) (concatlist [["a"; "z"; "b"; "y"]; ["c"; "x"; "d"; "w"]] []) ;; *)

(* Question 2 *)
let rec iftrue ll = match ll with 
  [] -> true
  |hl::tl -> List.mem true hl && iftrue tl;;

(* myprintbool (iftrue [[false; false; false; true]; [false; false; false; true]; [true; false; false; false]]);; *)

(* Question 3 *)
(* (first method) *)
let countexclamations s = 
  let count = ref 0 in
  if s = String.empty then count := !count else
    String.iter (fun e -> 
    if e = '!' then count := !count + 1
      )s;
      !count;;

(* (seconde method) *)
let countexclamations1 s = 
  let listofsub =  String.split_on_char '!' s in 
        List.length listofsub -1;;


(* print_int (countexclamations1 "son,kjcnkn!kjkebv!vfejh!!!!!!vvfvf! !!fvrvrv!!!rgvb!rg! ");; *)

(* Question 4  *)

let replaceexclamations s = 
  (* let point = '.' in *)
  String.map (fun e -> if e = '!' then '.' else e) s;;

(* String.iter (print_char) (replaceexclamations "son,kjcnkn'!'kjkebv!vfejh!!!!!!vvfvf! !!fvrvrv!!!rgvb!rg! ");; *)

(* Question 5 *)
(* book method *)
let concat sl = String.concat "" sl;; 
(* my method *)
let rec concat_list_of_strings ls =
  match ls with 
    [] -> ""
    |hl::tl -> hl ^ " " ^ concat_list_of_strings tl;;

(* let concat_list_of_chars_to_string ls = 
  List.to_seq ls |> String.of_seq ;; *)

(* print_string (concat_list_of_strings ["this"; "is"; "the"; "man"; "world"]);; *)

(* Question 6 *)
let concate_string ss =
  let b = Buffer.create 16 in 
  List.iter (Buffer.add_string b) ss;
  Buffer.contents b;;
  
(* print_string (concate_string ["this"; "is"; "the"; "man"; "world"; "that"; "what"; "i am saying." ]);; *)

(* Question 7 *)

let number_of_occurences ss s =
  if ss = "" then 0 else 
  let count = ref 0 in 
    for i = 0 to String.length ss - (String.length s) do 
    let substringss = String.sub ss i (String.length s) in
    if substringss = s then (print_int i ; print_newline(); count := !count + 1) else ()
    done;
    !count;;

(* print_int (number_of_occurences "this OCaml is such as good, never OCaml has an ocaml from caml to OCamls" "OCaml") *)

(* Chapter 16 *)


(* dans fichier textstat.ml *)
(* type stats = int * int * int * int;;
let stats_from_channel _ = (0, 0, 0, 0);;
let stats_from_file filename = 
  let channel = open_in filename in
  let result = stats_from_channel channel in
  close_in channel;
  result;;

stats_from_channel "textstat.ml";; *)
let sortedlist l =  List.sort compare l;; 
let smallint l = List.hd (sortedlist l);;
let newlist l = List.tl (sortedlist l);;
let length l  = List.length l;;
let rec leftlist l ll = 
    match l with 
      [] -> ll
      |[e] -> if length l mod 2 = 0 then ll else ll @ [e]
      |h::_::t ->leftlist t (ll @ [h] );;

let rec rightlist l lr =
    match l with 
      [] -> lr
      |[e] -> if length l mod 2 = 0 then [e] @ lr else lr
      |_::m::t -> rightlist t ([m] @ lr ) ;;
let make_valley (a: int list)=
    match newlist a with 
      [] -> []
      |[e] -> [e]
      |h::e::t -> 
        if length a mod 2 != 0 
          then (rightlist (newlist a) [] @ [smallint a]) @  leftlist (newlist a) [] 
        else List.rev  ((rightlist (newlist a) [] @ [smallint a]) @  leftlist (newlist a) [])
    ;;

let rec make_valley (a: int list) acc =
  let sorteda = List.sort (fun x y -> compare y x) a in
    match sorteda with 
      [] -> acc
      |[e] -> e::acc
      |h::e::t -> h :: make_valley t (e::acc)
    ;;
    
(* List.iter(Printf.printf ("%d ")) (make_valley [94; 88; 74; 63; 50; 46; 32; 27; 25; 22; 18] []); print_newline ();; *)
(* List.iter(print_int) (make_valley [99; 73; 52; 50; 44; -51; -84; -98]); print_newline ();; *)
(* 94; 74; 50; 32; 25; 18; 22; 27; 46; 63; 88] *)
  let rec y l1 l2= 
      match l1 with 
        [] -> l2
        |h::t -> y t (h::l2);;
let z l1 l2 = y (List.rev l1) l2;;

(* List.iter (print_int) (z [1; 2; 3; 4] [5; 6; 7; 8]);; *)

(* Random.self_init ();;
let x = Printf.printf ("%d ") (Random.int 50);; *)


(* let rowSumOddNumbers (n: int) = 
  
  for i = 1 to n do 
    if i = n then 
      for j = 1 to i do 
        if j mod 2 != 0 then 
          print_int j; print_char ' '    *)


(* let rec nb_year (p0: int) (percent: float) (aug: int) (p: int): int =
  let count_years = ref 0 in
  if p  <= 0 
    then count_years := !count_years
  else count_years := !count_years + 1; nb_year (p0 + int_of_float ((float_of_int p0) *. (percent /. 100.)) + aug) percent aug  (p - (p0 + int_of_float ((float_of_int p0) *. (percent/. 100.)) + aug)) ;; *)

let nb_year (p0: int) (percent: float) (aug: int) (p: int): int =
  let count_years = ref 0 in
  
    while p0 < p do 
      
      count_years := !count_years + 1
    done;
    count_years;;
      (* count_years = !count_years + 1 ; !count_years;; *)
  (* let count_years = ref 0 in
  if p0  >= p 
    then count_years := !count_years
  else count_years := !count_years + 1; nb_year (p0 + int_of_float ((float_of_int p0) *. (percent /. 100.)) + aug) percent aug p ;; *)


nb_year 1000 2.0 50 1200;;
