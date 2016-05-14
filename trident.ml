(* Trident Hotel Point of Sale and Interactive Consequence Delivery System  *)
(*  version 1.0 alpha, with special features:                               *)
(*                                                                          *)
(*   - infinite (countable) consequence delivery,                           *)
(*   - infinite (countable) room availability,                              *)
(*   - infinite (countable) detainment life-cycle management.               *)
(*                                                                          *)
(* (c)Copyright, 2016 to Omega (a.k.a. Aleph_0, the first limit ordinal),   *)
(* Platform for Transparent Political Mathematics                           *)

let version = "1.0 alpha"

type detainee_name  = Trident

type detainee_kind  = Mandatory | Non_mandatory

type detainee_count = Finite of int
                    | Omega              (* countable infinity of detainees *)

type streamline_state = Held_in_Del_Rio of int                 (* days held *)
                      | Mass_trial_in_Del_Rio of int    (* ID in mass trial *)
                      | Mass_federal_prison_sentence of int (* 2 - 20 years *)

type consequence = Detainment
                 | Streamline_prosecution of streamline_state
                 | Deportation

type detainee = { name               : detainee_name;
                  id                 : int;
                  kind               : detainee_kind;
                  current_room       : int option;
                  movements          : string list;
                  consequences       : consequence list;
                  days_in_detention  : int;
                  salary_lost        : float;
                  psychological_toll : float }

type hotel_state =
    { num_detainees                  : detainee_count;
      visible_detainees              : detainee list;
      price_per_night                : float;
      costs_per_night                : float;
      profit                         : float }

type arrival = { new_visible_detainees : detainee list;
                 new_detainee_count    : detainee_count }

type event  = New_arrival of arrival
            | Next_day
            | Req_Interrogation of detainee
            | Req_Op_Streamline of detainee
            | Req_release of detainee
            | Profit_and_loss

(* Room morphisms, the key to detaining infinitely many guests at once *)

type room_morph = Plus of int       (* e.g., (room i) -> (room i+1) *)
                | Times of int      (* e.g., (room i) -> (room 2*i) *)
                | Pow of int        (* e.g., (room i) -> (room 3^i) *)

type action = Detain of int         (* initial single detainment and room assignment *)
            | Detains of room_morph (* initial infinite detainment and room assignment *)
            | Move of int * int     (* move detainee from room A to room B *)
            | Moves of room_morph   (* move infinitely many detainees via morphism *)
            | Release of int        (* release detainee and free room *)
            | Streamline of int     (* send to Operation Streamline and free room *)

(* Detainee arithmetic *)

exception Infinite_detainee_subtraction

let add (x : detainee_count) (y : detainee_count) =
  match x,y with
    Finite n, Finite m -> Finite (n+m)
  | _, Omega -> Omega
  | Omega, _ -> Omega

let sub (x : detainee_count) (y : detainee_count) =
  match x,y with
    Finite n, Finite m -> Finite (max (n-m) 0)
  | Omega, Finite m -> Omega
  | _, Omega -> raise Infinite_detainee_subtraction

let profit_mult (x : detainee_count) (y : float) =
  match x with
    Finite n -> (float_of_int n) *. y
  | Omega -> infinity

let rec mk_lst base k acc =
  if k <= 0 then
    acc
  else let i = k + base in
       mk_lst base (k-1) (i::acc)

(* Printers *)

let string_of_detainee_count (x : detainee_count) =
  match x with
    Finite n -> string_of_int n
  | Omega -> "Omega"

let string_of_room_morph (m : room_morph) =
  match m with
    Plus n -> Printf.sprintf "(i -> i + %d)" n
  | Times n -> Printf.sprintf "(i -> i * %d)" n
  | Pow n -> Printf.sprintf "(i -> 3^(%d)" n

let string_of_room (i : int option) =
  match i with
    Some n -> string_of_int n
  | None -> "None"

let string_of_name (n : detainee_name) =
  match n with
    Trident -> "TRIDENT"

let string_of_kind (k : detainee_kind) =
  match k with
    Mandatory -> "Mandatory"
  | Non_mandatory -> "Non_mandatory"

let string_of_movements ms =
  let s = String.concat ",\n   " (List.rev ms) in
  "[ " ^ s ^ " ]"

let string_of_detainee (d : detainee) =
  Printf.sprintf
    "Name: %s\nID: %d\nKind: %s\nRoom: %s\nMovements:\n %s
Days in detention: %d\nSalary lost: %f\nPsychological toll: %f\n"
    (string_of_name d.name)
    d.id
    (string_of_kind d.kind)
    (string_of_room d.current_room)
    (string_of_movements d.movements)
    d.days_in_detention
    d.salary_lost
    d.psychological_toll

(* Room assignments, including infinitely many *)

let mk_detain_actions rooms =
  List.map (fun i -> Detain i) rooms

let assign_rooms (s : hotel_state) (num_new_detainees : detainee_count) =
  match s.num_detainees, num_new_detainees with
  | Finite n, Finite m ->
     let avail_rooms = mk_lst n m [] in
     let detain_acts = mk_detain_actions avail_rooms in
     detain_acts
  | Finite n, Omega ->
     let morph = Plus n in
     [Detains morph]
  | Omega, Finite m ->
     let morph = Plus m in
     let move_acts = [Moves morph] in
     let avail_rooms = mk_lst 0 m [] in
     let detain_acts = mk_detain_actions avail_rooms in
     move_acts @ detain_acts
  | Omega, Omega ->
     let morph = Times 2 in
     let move_acts = [Moves morph] in
     move_acts

let trident (d : detainee) (e : event) =
  let onboard_trident_factor n =
    sqrt(float_of_int(n + 1))
  in
  let cur_psych_toll = d.psychological_toll in
  let num_days = d.days_in_detention in
  match e with
    New_arrival _ ->
    cur_psych_toll +. (onboard_trident_factor num_days)
  | _ -> cur_psych_toll +. 1.0

(* Detainee onboarding *)

let onboard_detainee (d : detainee) (room : int) =
  { d with
    current_room = Some room;
    movements = (Printf.sprintf "Held in room %d" room) :: d.movements;
    days_in_detention = d.days_in_detention + 1;
    psychological_toll = trident d (New_arrival { new_visible_detainees = [d];
                                                  new_detainee_count = Finite 1 });
    consequences = Detainment :: d.consequences }

let rec zip l1 l2 =
  match l1,l2 with
    [], [] -> []
  | h1::t1, h2::t2 -> (h1, h2) :: (zip t1 t2)
  | _ -> failwith "zip"

let rec pow n m =
  if m < 0 then failwith "pow"
  else if m = 0 then 1
  else n * (pow n (m-1))

let rec apply_room_morph m d =
  let movement_desc a b =
      Printf.sprintf "Moved from room %d to room %d" a b
  in
  match m, d.current_room with
    Plus i, Some r ->
    let new_room = r + i in
    { d with current_room = Some new_room;
             movements = (movement_desc r new_room) :: d.movements }
  | Times i, Some r ->
    let new_room = r * i in
    { d with current_room = Some new_room;
             movements = (movement_desc r new_room) :: d.movements }
  | Pow i, Some r ->
    let new_room = pow i r in
    { d with current_room = Some new_room;
             movements = (movement_desc r new_room) :: d.movements }
  | _ -> failwith "apply_room_morph"

let proc_event (s : hotel_state) (e : event) =
  let proc_act s d act =
    match act, d with
      Detain room_num, Some d ->
      let d' = onboard_detainee d room_num in
      let _ = Printf.printf "Immigrant %d detained in room %s.\n"
                            d'.id (string_of_room d'.current_room) in
      {s with num_detainees = add s.num_detainees (Finite 1);
              visible_detainees = d' :: s.visible_detainees;
      }
    | Detains m, None ->
       let _ = Printf.printf "\nNew detainees interned via room morphism: %s\n"
                             (string_of_room_morph m)
       in
       { s with num_detainees = add s.num_detainees Omega }
    | Moves m, _ ->
       let _ = Printf.printf "\nApplying room morphism %s to %s existing detainees.\n"
                             (string_of_room_morph m) (string_of_detainee_count s.num_detainees)
       in
       { s with num_detainees = add s.num_detainees (Finite 1);
                visible_detainees = List.map (apply_room_morph m) s.visible_detainees }
    | _ -> s
  in
  match e with
  | New_arrival { new_visible_detainees = ds; new_detainee_count = Finite n } ->
     let acts = assign_rooms s (Finite (List.length ds)) in
     (match acts with
        (Moves m) :: rst ->
        let s' = proc_act s None (Moves m) in
        List.fold_left (fun s (d,r) -> proc_act s (Some d) r) s' (zip ds rst)
      | _ -> List.fold_left (fun s (d,r) -> proc_act s (Some d) r) s (zip ds acts))
  | New_arrival { new_visible_detainees = []; new_detainee_count = Omega } ->
     let acts = assign_rooms s Omega in
     proc_act s None (List.hd acts)
  | _ -> Printf.printf "Unprocessed event!";
         s

let next_day (s : hotel_state) =
  let price  = s.price_per_night in
  let cost   = s.costs_per_night in
  let profit_per_night = price -. cost in
  let overnight_visible_detainees =
    List.map
      (fun d ->
       { d with psychological_toll = trident d Next_day;
                days_in_detention = d.days_in_detention + 1;
                salary_lost = sqrt(float_of_int(d.days_in_detention + 1)) *. 1000.0 })
      s.visible_detainees
  in
  { s with
    visible_detainees = overnight_visible_detainees;
    profit = s.profit +. (profit_mult s.num_detainees profit_per_night);
  }

let create_detainee () =
  { name = Trident;
    id = Random.int(10000000);
    kind = if Random.bool() then Non_mandatory else Mandatory;
    current_room = None;
    movements = ["Initially caught"];
    consequences = [];
    days_in_detention = 0;
    salary_lost = Random.float(1000.0);
    psychological_toll = Random.float(25.0) }

let rec create_detainees k acc =
  if k <= 0 then acc
  else let d = create_detainee () in
       create_detainees (k-1) (d :: acc)

let initial_state =
  { num_detainees = Finite 0;
    visible_detainees = [];
    price_per_night = 1000.0;
    costs_per_night = 100.0;
    profit = 0.0 }

let banner () = ();;
#use"banner.ml";;

open Printf

let point_of_sale () =
  let _ = banner () in
  let _ = printf " Welcome to TRIDENT Hotel Point of Sale and Interactive Consequence Delivery System\n" in
  let _ = printf " Press 'h' for help.\n" in
  let rec pos (s : hotel_state) =
    let _ = Printf.printf "\nTRIDENT:POS>" in
    let l = read_line () in
    let s', q =
      match l with
        "h" -> printf "\nTRIDENT HELP SYSTEM\n\n";
               printf " a: detain new (potentially infinitely many) arrivals\n";
               printf " d: move clock forward to next day\n";
               printf " h: help system\n";
               printf " i: interogate detainee\n";
               printf " o: occupancy report (with vacancies)\n";
               printf " p: current hotel profit\n";
               printf " s: send detainee to Operation Streamline\n";
               printf " q: quit TRIDENT:POS\n";
               (s, false)
      | "a" -> let _ = printf "\nHow many new arrivals (use 'omega' for infinitely many): " in
               let l = read_line () in
               (match l with
                  "omega" ->
                  let e = New_arrival { new_visible_detainees = [];
                                        new_detainee_count = Omega } in
                  let s = proc_event s e in
                  (s, false)
                | _ -> (try
                           let k = int_of_string l in
                           let ds = create_detainees k [] in
                           let e = New_arrival { new_visible_detainees = ds;
                                                 new_detainee_count = Finite k } in
                           let s = proc_event s e
                           in (s, false)
                         with e -> (printf "Invalid detainee count.\n";
                                    raise e)))
      | "d" -> let _ = printf "\nMoving clock to next day.\n" in
               let s' = next_day s in
               let _ = printf "All %s detainees secured.\n"
                              (string_of_detainee_count s'.num_detainees) in
               (s', false)
      | "i" -> let _ = printf "\nDetainee visibility index: " in
               let l = read_line () in
               (try
                   let k = int_of_string l - 1 in
                   print_string (string_of_detainee (List.nth s.visible_detainees k));
                   (s, false)
                 with _ -> printf "Detainee %s not found" l;
                           (s, false))
      | "p" -> let _ = printf "\nHotel profit to date: %f\n"
                              s.profit
               in
               (s, false)
      | "o" -> let _ = printf "\nOccupancy report:\n" in
               let _ = printf "  %s rooms occupied.\n  %d detainees visible.\n  Infinitely many rooms available.\n"
                              (string_of_detainee_count s.num_detainees)
                              (List.length s.visible_detainees)
               in (s, false)

      | "q" -> (s, true)
      | _ -> (s, false)
    in
    if s.profit <> infinity && s'.profit = infinity
    then printf "\n*** Infinite profitability reached! ***\n";
    if not q then pos s' else s'
  in
  pos (initial_state)
;;

(* Setup our TRIDENT read-eval-print loop *)

#use"topfind";;
#require"lambda-term";;
#require"Str";;

let prompt = LTerm_text.(eval [B_fg (LTerm_style.green); S "TRIDENT> "]);;
UTop.prompt := fst (React.S.create prompt);;
