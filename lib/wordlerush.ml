open Lwt.Infix

(* Starts the Wordle game loop *)
module WordleRush = struct
  type game_category =
    | Classic
    | Four
    | Six
    | Blank

  type difficulty_level =
    | Easy
    | Medium
    | Hard

  type char_color =
    | Green
    | Yellow
    | Grey
    | Red

  type pairings = (int * (char * char_color)) list
  type keyb_hash = (char, char_color) Hashtbl.t

  let cat_hash = Hashtbl.create 1
  let () = Hashtbl.add cat_hash 1 Blank
  let lwt_read_line () : string Lwt.t = Lwt_io.read_line Lwt_io.stdin

  let rec select_category () : unit =
    print_newline ();
    print_string
      "Choose Wordle:      4) Four-letter      5) Five-letter      6) \
       Six-letter       ";
    match String.trim (String.lowercase_ascii (read_line ())) with
    | "4" -> Hashtbl.replace cat_hash 1 Four
    | "5" -> Hashtbl.replace cat_hash 1 Classic
    | "6" -> Hashtbl.replace cat_hash 1 Six
    | _ ->
        print_newline ();
        print_string "Not an Option. Select Again.";
        select_category ()

  let rec select_difficulty () : difficulty_level =
    print_newline ();
    print_string
      "Choose difficulty level:      e) Easy      m) Medium      h) Hard      ";
    match String.trim (String.lowercase_ascii (read_line ())) with
    | "e" -> Easy
    | "m" -> Medium
    | "h" -> Hard
    | _ ->
        print_newline ();
        print_string "Not a Difficulty. Select Again.";
        select_difficulty ()

  let rec select_time () : int =
    print_newline ();
    print_string "Choose timer:      1) 120s      2) 60s      3) 30s      ";
    match String.trim (String.lowercase_ascii (read_line ())) with
    | "1" -> 120
    | "2" -> 60
    | "3" -> 30
    | _ ->
        print_newline ();
        print_string "Not a Timer. Select Again.";
        select_time ()

  (**[get_max_attempts level] returns the level with the amount of attempts
     connected to it. Used in main game loop, and in calculating score*)
  let get_max_attempts (level : difficulty_level) : int =
    match level with
    | Easy -> 8
    | Medium -> 6
    | Hard -> 4

  (**letters on keyboard, this is used as reference for [paint_keyboard] as
     reference for qwerty keyboard layout *)
  let qwerty_hash : keyb_hash = Hashtbl.create 26

  let keys =
    [
      'q';
      'w';
      'e';
      'r';
      't';
      'y';
      'u';
      'i';
      'o';
      'p';
      'a';
      's';
      'd';
      'f';
      'g';
      'h';
      'j';
      'k';
      'l';
      'z';
      'x';
      'c';
      'v';
      'b';
      'n';
      'm';
    ]

  let () = List.iter (fun c -> Hashtbl.add qwerty_hash c Grey) keys

  let reset_Hashtbl (hash : keyb_hash) : unit =
    Hashtbl.reset hash;
    List.iter (fun c -> Hashtbl.add hash c Grey) keys

  let read_file (filename : string) : string list =
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true do
        lines := input_line chan :: !lines
      done;
      !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines

  let word_list (game_c : game_category) : string list =
    match game_c with
    | Classic -> read_file "data/sgb-5-letter.txt"
    | Four -> read_file "data/sgb-4-letter.txt"
    | Six -> read_file "data/sgb-6-letter.txt"
    | Blank -> failwith "not a category"

  let double_newline () : unit =
    print_newline ();
    print_newline ()

  let score_multiplier = 100

  type player = {
    name : string;
    mutable score : int;
    mutable cumlative_score : int;
  }

  let current_player = ref { name = "Player"; score = 0; cumlative_score = 0 }
  let timed = ref false
  let current_timer_task = ref (Lwt.return ())

  let rec introduction () : unit =
    Random.self_init ();
    reset_Hashtbl qwerty_hash;
    print_newline ();
    !current_player.score <- 0;
    print_newline ();
    Printf.printf
      "               .---.                              ,--,             \
       ,-.----.                              ,---,     \n\
      \    ";
    Printf.printf
      "          /. ./|                       ,---,,--.'|             \\    /  \
       \\                           ,--.' |     \n\
      \    ";
    Printf.printf
      "      .--'.  ' ;   ,---.    __  ,-.  ,---.'||  | :             ;   :    \
       \\          ,--,            |  |  :     \n\
      \    ";
    Printf.printf
      "     /__./ \\ : |  '   ,'\\ ,' ,'/ /|  |   | ::  : '             |   | \
       .\\ :        ,'_ /|   .--.--.  :  :  :     \n\
      \    ";
    Printf.printf
      " .--'.  '   \\' . /   /   |'  | |' |  |   | ||  ' |      ,---.  .   : \
       |: |   .--. |  | :  /  /    ' :  |  |,--. \n\
      \    ";
    Printf.printf
      "/___/ \\ |    ' '.   ; ,. :|  |   ,',--.__| |'  | |     /     \\ |   |  \
       \\ : ,'_ /| :  . | |  :  /`./ |  :  '   | \n\
      \    ";
    Printf.printf
      ";   \\  \\;      :'   | |: :'  :  / /   ,'   ||  | :    /    /  ||   : \
       .  / |  ' | |  . . |  :  ;_   |  |   /' : \n\
      \    ";
    Printf.printf
      " \\   ;  `      |'   | .; :|  | ' .   '  /  |'  : |__ .    ' / |;   | \
       |  \\ |  | ' |  | |  \\  \\    `.'  :  | | | \n\
      \    ";
    Printf.printf
      "  .   \\    .\\  ;|   :    |;  : | '   ; |:  ||  | '.'|'   ;   /||   | \
       ;\\  \\:  | : ;  ; |   `----.   \\  |  ' | : \n\
      \    ";
    Printf.printf
      "   \\   \\   ' \\ | \\   \\  / |  , ; |   | '/  ';  :    ;'   |  / |:   \
       ' | \\.''  :  `--'   \\ /  /`--'  /  :  :_:,' \n\
      \    ";
    Printf.printf
      "    :   '  |--\"   `----'   ---'  |   :    :||  ,   / |   :    |:   : \
       :-'  :  ,      .-./'--'.     /|  | ,'     \n\
      \    ";
    Printf.printf
      "     \\   \\ ;                      \\   \\  /   ---`-'   \\   \\  / \
       |   |.'     `--`----'      `--'---' `--''       \n\
      \    ";
    Printf.printf
      "      '---\"                        `----'              `----'  \
       `---'                                           \n\
      \    ";
    print_newline ();
    settings_menu ();
    print_newline ()

  and settings_menu () : unit =
    print_newline ();
    print_string
      "---------------------------------------------------------------------------------";
    print_newline ();
    Printf.printf "Hello there %s" !current_player.name;
    print_newline ();
    print_string
      "---------------------------------------------------------------------------------";
    print_newline ();
    print_string "SETTINGS MENU";
    print_newline ();
    print_string "1. Change Player Name";
    print_newline ();
    print_string "2. Play Game Timed";
    print_newline ();
    print_string "3. Play Game Untimed";
    print_newline ();
    print_string "Choose an option: ";
    match String.trim (String.lowercase_ascii (read_line ())) with
    | "1" ->
        print_newline ();
        print_string "Enter your new name: ";
        let new_name = read_line () in
        current_player := { !current_player with name = new_name };
        print_newline ();
        Printf.printf "Player name updated to %s\n" new_name;
        settings_menu ()
    | "2" ->
        timed := true;
        ()
    | "3" ->
        timed := false;
        ()
    | _ ->
        print_newline ();
        print_string "Invalid option. Try again.";
        settings_menu ()

  let select_random_word game_category : string =
    List.nth (word_list game_category)
      (Random.int (List.length (word_list game_category)))

  let is_valid_word (word : string) (category : game_category) : bool =
    let lower_trim_word = word |> String.lowercase_ascii |> String.trim in
    match category with
    | Classic ->
        String.length lower_trim_word = 5
        && List.mem lower_trim_word (word_list Classic)
    | Four ->
        String.length lower_trim_word = 4
        && List.mem lower_trim_word (word_list Four)
    | Six ->
        String.length lower_trim_word = 6
        && List.mem lower_trim_word (word_list Six)
    | Blank -> failwith "Not an option"

  let is_correct_guess (target_word : string) (guessed_word : string) : bool =
    let lower_trim_guess =
      guessed_word |> String.lowercase_ascii |> String.trim
    in
    String.equal target_word lower_trim_guess

  let rec remove_duplicates (lst : 'a list) : 'a list =
    let rec mem lst entry =
      match lst with
      | [] -> false
      | h :: t -> h = entry || mem t entry
    in
    match lst with
    | [] -> []
    | h :: t ->
        let acc = remove_duplicates t in
        if mem acc h then acc else h :: acc

  let keep_count = ref 0

  let rec map_green_chars (target_chars : char list) (guess_chars : char list)
      (color_letter_pairs : pairings) (index : int) : pairings =
    match (target_chars, guess_chars) with
    | [], _ | _, [] -> []
    | t :: ts, g :: gs ->
        let rest_list = map_green_chars ts gs color_letter_pairs (index + 1) in
        if t = g then (index, (g, Green)) :: rest_list else rest_list

  let rec map_yellow_red_chars (target_chars : char list)
      (guess_chars : char list) (color_letter_pairs : pairings) (index : int) :
      pairings =
    match guess_chars with
    | [] -> color_letter_pairs
    | g :: gs ->
        let rest_list =
          map_yellow_red_chars target_chars gs color_letter_pairs (index + 1)
        in
        if
          List.mem g target_chars
          && not (List.mem_assoc index color_letter_pairs)
        then (index, (g, Yellow)) :: rest_list
        else if not (List.mem g target_chars) then
          (index, (g, Red)) :: rest_list
        else rest_list

  let map_chars_to_color (target_chars : char list) (guess_chars : char list) :
      pairings =
    let greens = map_green_chars target_chars guess_chars [] 0 in
    map_yellow_red_chars target_chars guess_chars greens 0

  let sort_bindings (char_color_list : pairings) : pairings =
    let sort_indices =
      List.sort_uniq compare (fst (List.split char_color_list))
    in
    List.map (fun i -> (i, List.assoc i char_color_list)) sort_indices

  let match_color (color : char_color) : ANSITerminal.style list =
    match color with
    | Green -> [ ANSITerminal.green ]
    | Yellow -> [ ANSITerminal.yellow ]
    | Grey -> [ ANSITerminal.default ]
    | Red -> [ ANSITerminal.red ]

  let rec print_chars (char_color_list : pairings) : unit =
    let sorted = sort_bindings char_color_list in
    match sorted with
    | [] -> print_newline ()
    | (_, (c, color)) :: t ->
        ANSITerminal.print_string (match_color color) (String.make 1 c ^ " ");
        print_chars t

  let rec correct_letters_score (guess_lst : string list) (max_score : int)
      (target_word : string) =
    match guess_lst with
    | [] -> max_score
    | _ :: t ->
        if !keep_count > max_score then
          correct_letters_score t !keep_count target_word
        else correct_letters_score t max_score target_word

  let char_list_to_string (char_list : char list) : string =
    String.concat "" (List.map (String.make 1) char_list)

  let rec iter (head : string) (string1 : string) (string2 : string)
      (acc : (char * int) list) : (char * int) list =
    match
      ( List.init (String.length string1) (String.get string1),
        List.init (String.length string2) (String.get string2) )
    with
    | [], [] -> acc
    | h :: t, a :: b ->
        if h = a then
          if List.mem (h, String.index head h) acc = false then
            iter head (char_list_to_string t) (char_list_to_string b)
              ((h, String.index head h) :: acc)
          else iter head (char_list_to_string t) (char_list_to_string b) acc
        else iter head (char_list_to_string t) (char_list_to_string b) acc
    | _ -> failwith "impossible"

  let rec overall_correct_letters (guess_lst : string list)
      (lst : (char * int) list) (target_word : string) : (char * int) list =
    match guess_lst with
    | [] -> lst
    | head :: tail ->
        iter head head target_word lst
        @ overall_correct_letters tail lst target_word

  let rec update_colors (char_color_list : pairings) (qwerty_hash : keyb_hash) :
      (char * char_color) list =
    match char_color_list with
    | [] -> []
    | (_, (c, color)) :: t ->
        let current_color =
          if Hashtbl.mem qwerty_hash c then Hashtbl.find qwerty_hash c else Grey
        in
        let new_color = if current_color = Green then Green else color in
        (c, new_color) :: update_colors t qwerty_hash

  (* Makes the keys hava a keyboard shape in the terminal *)
  let square_keyboard (some_key : char list) : unit =
    List.iter
      (fun key ->
        if String.make 1 key = "q" then print_newline ()
        else if String.make 1 key = "a" then (
          print_newline ();
          print_string " ")
        else if String.make 1 key = "z" then (
          print_newline ();
          print_string "   ")
        else ();
        let color = Hashtbl.find qwerty_hash key in
        ANSITerminal.print_string (match_color color) (String.make 1 key ^ " "))
      some_key

  let rec print_keyboard_chars (guess_lst : string list) (target_word : string)
      (qwerty_hash : keyb_hash) : unit =
    let target_approx =
      List.init (String.length target_word) (String.get target_word)
    in
    match guess_lst with
    | [] -> square_keyboard keys
    | h :: t ->
        let colorified_list =
          map_chars_to_color target_approx
            (List.init (String.length h) (String.get h))
        in
        let colored_list = update_colors colorified_list qwerty_hash in
        List.iter
          (fun (key, value) ->
            if Hashtbl.mem qwerty_hash key then
              Hashtbl.replace qwerty_hash key value)
          colored_list;
        print_keyboard_chars t target_word qwerty_hash

  let combine_list target_word list_of_n =
    List.combine
      (List.init (String.length target_word) (String.get target_word))
      list_of_n

  (* Get indices based on the category *)
  let get_indices_by_category () : int list =
    match Hashtbl.find cat_hash 1 with
    | Four -> [ 0; 1; 2; 3 ]
    | Classic -> [ 0; 1; 2; 3; 4 ]
    | Six -> [ 0; 1; 2; 3; 4; 5 ]
    | Blank -> failwith "not a category"

  (* Prints each character in for the hint function *)
  let rec output (curr_ind : int) (target_approx : 'a list)
      (random_number : int) (rand_letter : char * 'b) : unit =
    if curr_ind = List.length target_approx then ()
    else if curr_ind = random_number then (
      ANSITerminal.print_string [ ANSITerminal.green ]
        (String.make 1 (fst rand_letter) ^ " ");
      output (curr_ind + 1) target_approx random_number rand_letter)
    else
      (ANSITerminal.print_string [ ANSITerminal.default ] "_ ";
       output (curr_ind + 1))
        target_approx random_number rand_letter

  let list_combine target_word list_of_n =
    List.combine
      (List.init (String.length target_word) (String.get target_word))
      list_of_n

  let reveal_letter (guess_lst : string list) (target_word : string) : unit =
    let list_of_n = get_indices_by_category () in
    let target_approx = list_combine target_word list_of_n in
    match
      remove_duplicates (overall_correct_letters guess_lst [] target_word)
    with
    | [] ->
        let random_number = Random.int (List.length target_approx) in
        let rand_letter = List.nth target_approx random_number in
        output 0 target_approx random_number rand_letter
    | h :: t ->
        let subtracted_lst =
          List.filter (fun x -> List.mem x (h :: t) = false) target_approx
        in
        let random_number = Random.int (List.length subtracted_lst) in
        let rand_letter = List.nth subtracted_lst random_number in
        output 0 target_approx random_number rand_letter

  let get_hint (target : string) (guess_lst : string list) : unit =
    reveal_letter guess_lst target

  let give_up (target_chars : char list) : unit =
    List.iter
      (fun h ->
        ANSITerminal.print_string [ ANSITerminal.green ] (String.make 1 h ^ " "))
      target_chars

  let print_message game_category =
    match game_category with
    | Classic ->
        Printf.printf "\n%s, try to guess the hidden five-letter word.\n"
          !current_player.name
    | Four ->
        Printf.printf "\n%s, try to guess the hidden four-letter word.\n"
          !current_player.name
    | Six ->
        Printf.printf "\n%s, try to guess the hidden six-letter word.\n"
          !current_player.name
    | Blank -> failwith "You're not supposed to be here"

  let print_invalid game_category =
    match game_category with
    | Classic ->
        Printf.printf
          "Invalid word. Please enter a five-letter word from the word list.\n"
    | Four ->
        Printf.printf
          "Invalid word. Please enter a four-letter word from the word list.\n"
    | Six ->
        Printf.printf
          "Invalid word. Please enter a six-letter word from the word list.\n"
    | Blank -> failwith "You're not supposed to be here"

  let rec process_guess (target_word : string) (attempts_left : int)
      (guess_lst : string list) : unit Lwt.t =
    Lwt.return (print_newline ()) >>= fun () ->
    Lwt.return (print_string "Enter 'H' for a hint or 'G' to give up \n")
    >>= fun () ->
    Lwt.return (Printf.printf "Attempts left: %d\n" attempts_left) >>= fun () ->
    Lwt.return (print_newline ()) >>= fun () ->
    Lwt.return (Printf.printf "Letters: ") >>= fun () ->
    Lwt.return (print_keyboard_chars guess_lst target_word qwerty_hash)
    >>= fun () ->
    Lwt.return (print_newline ()) >>= fun () ->
    Lwt.return (print_string "Enter your guess: ") >>= fun () ->
    Lwt.return (flush stdout) >>= fun () ->
    lwt_read_line () >>= fun guess ->
    let input = String.trim (String.lowercase_ascii guess) in
    Lwt.return (print_newline ()) >>= fun () ->
    Lwt.return (ANSITerminal.erase Screen) >>= fun () ->
    if String.equal input "h" then
      Lwt.return (get_hint target_word guess_lst) >>= fun () ->
      Lwt.return (print_newline ()) >>= fun () ->
      Lwt.return (double_newline ()) >>= fun () ->
      wordle_game target_word (attempts_left - 1) guess_lst
    else if String.equal guess "g" then
      let length = String.length target_word in
      Lwt.return (give_up (List.init length (String.get target_word)))
      >>= fun () ->
      Lwt.return (double_newline ()) >>= fun () -> restart_game ()
    else process_valid_guess target_word attempts_left guess_lst guess

  and process_valid_guess (target_word : string) (attempts_left : int)
      (guess_lst : string list) (guess : string) : unit Lwt.t =
    if is_valid_word guess (Hashtbl.find cat_hash 1) then
      let updated_lst = guess_lst @ [ guess ] in
      if is_correct_guess target_word guess then
        Lwt.return (double_newline ()) >>= fun () ->
        Lwt.return
          (Printf.printf "Congratulations! You've guessed the word: %s"
             target_word)
        >>= fun () ->
        Lwt.return (double_newline ()) >>= fun () ->
        Lwt.return
          (!current_player.score <-
            (5 * score_multiplier) + (attempts_left * score_multiplier))
        >>= fun () ->
        Lwt.return
          (!current_player.cumlative_score <-
            !current_player.cumlative_score + !current_player.score)
        >>= fun () ->
        Lwt.return
          (Printf.printf "Your round score is : %i" !current_player.score)
        >>= fun () ->
        Lwt.return (print_newline ()) >>= fun () ->
        Lwt.return
          (Printf.printf "Your overall score is : %i"
             !current_player.cumlative_score)
        >>= fun () ->
        Lwt.return (double_newline ()) >>= fun () -> restart_game ()
      else wordle_game target_word (attempts_left - 1) updated_lst
    else
      let category = Hashtbl.find cat_hash 1 in
      Lwt.return (print_invalid category) >>= fun () ->
      wordle_game target_word attempts_left guess_lst

  and wordle_game (target_word : string) (attempts_left : int)
      (guess_lst : string list) : unit Lwt.t =
    Lwt.return
      (List.iter
         (fun guess ->
           print_chars
             (map_chars_to_color
                (List.init (String.length target_word) (String.get target_word))
                (List.init (String.length guess) (String.get guess))))
         guess_lst)
    >>= fun () ->
    let correct_score = correct_letters_score guess_lst 0 target_word in
    Lwt.return
      (!current_player.score <-
        (correct_score * score_multiplier) + (attempts_left * score_multiplier))
    >>= fun () ->
    Lwt.return (keep_count := 0) >>= fun () ->
    if attempts_left = 0 then
      Lwt.return
        (!current_player.cumlative_score <-
          !current_player.cumlative_score + !current_player.score)
      >>= fun () ->
      Lwt.return (print_newline ()) >>= fun () ->
      Lwt.return
        (Printf.printf "You've run out of attempts. The word was: %s\n"
           target_word)
      >>= fun () ->
      Lwt.return (Printf.printf "Your score is : %i" !current_player.score)
      >>= fun () ->
      Lwt.return (print_newline ()) >>= fun () ->
      Lwt.return
        (Printf.printf "Your overall score is : %i"
           !current_player.cumlative_score)
      >>= fun () ->
      Lwt.return (double_newline ()) >>= fun () -> restart_game ()
    else process_guess target_word attempts_left guess_lst

  and restart_game () : unit Lwt.t =
    Printf.printf "Enter 'R' to play again! or 'Q' to quit.\n";
    let user_input = String.lowercase_ascii (read_line ()) in
    match String.trim user_input with
    | "q" ->
        print_endline "Thanks for Playing. Goodbye!";
        exit 0
    | "r" ->
        reset_Hashtbl qwerty_hash;
        !current_player.score <- 0;
        if !timed then Lwt.return () >>= fun () -> start_timed_game ()
        else start_untimed_game ()
    | _ ->
        print_string "Not an Option";
        double_newline ();
        restart_game ()

  and start_timed_game () : unit Lwt.t =
    select_category ();
    let category = Hashtbl.find cat_hash 1 in
    let target_word = select_random_word category in
    let difficulty_level = select_difficulty () in
    let time = select_time () in
    double_newline ();
    let max_attempts = get_max_attempts difficulty_level in
    let rec timer (seconds : int) : unit Lwt.t =
      let rec next_second (sec : int) : unit Lwt.t =
        if sec < 0 then
          Lwt.return (print_newline ()) >>= fun () ->
          Lwt.return
            (Printf.printf "You've run out of time! The word was: %s\n"
               target_word)
          >>= fun () ->
          Lwt.return (print_newline ()) >>= fun () -> restart_game ()
        else
          Lwt_unix.sleep 1. >>= fun () ->
          ANSITerminal.save_cursor ();
          ANSITerminal.set_cursor 35 0;
          Printf.printf "Time left: %d seconds " sec;
          ANSITerminal.restore_cursor ();
          flush stdout;
          next_second (sec - 1)
      in
      Lwt.catch
        (fun () -> next_second seconds)
        (function
          | Lwt.Canceled -> timer seconds
          | exn -> Lwt.fail exn)
    in
    Lwt.cancel !current_timer_task;
    current_timer_task := timer time;
    Lwt.return (print_message category) >>= fun () ->
    Lwt.return
      (Printf.printf "Your overall score is %i" !current_player.cumlative_score)
    >>= fun () ->
    Lwt.return (print_newline ()) >>= fun () ->
    Lwt.pick [ !current_timer_task; wordle_game target_word max_attempts [] ]

  and start_untimed_game () : unit Lwt.t =
    select_category ();
    let category = Hashtbl.find cat_hash 1 in
    let target_word = select_random_word category in
    let difficulty_level = select_difficulty () in
    let max_attempts = get_max_attempts difficulty_level in
    Lwt.return (print_message category) >>= fun () ->
    Lwt.return
      (Printf.printf "Your overall score is %i" !current_player.cumlative_score)
    >>= fun () ->
    Lwt.return (print_newline ()) >>= fun () ->
    wordle_game target_word max_attempts []

  and start_game () : unit =
    introduction ();
    let game_mode = if !timed then start_timed_game else start_untimed_game in
    Lwt_main.run (game_mode ())
end

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "start" then
    WordleRush.start_game ()
