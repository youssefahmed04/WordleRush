(** module WordleGame is a Wordle game loop with multiple game categories,
    difficulty levels.*)
module WordleRush : sig
  (** Game category is the type of wordle game that the user can play. It could
      be Classic like the origin one played with 5 letter words, or Four letter
      Wordle played with four lettered words, or 6 letters played with 6 letter
      words*)
  type game_category =
    | Classic
    | Four
    | Six
    | Blank

  (** [difficulty_level] is the difficulty of the Wordle game that is used to
      map the number of attempts the user gets based on the difficulty.*)
  type difficulty_level =
    | Easy
    | Medium
    | Hard

  (** [char_color] is a type for the colors that the characters could take. They
      could be Green for correctly guessed characters, Yellow for misplaced but
      correctly included letters in the target word and grey letter which are
      all other types of colors. Red colors are letters that have already been
      guessed*)
  type char_color =
    | Green
    | Yellow
    | Grey
    | Red

  type pairings = (int * (char * char_color)) list
  (** [pairings] define an association list between each index of char in a
      word, the char and its color*)

  type keyb_hash = (char, char_color) Hashtbl.t
  (** [Keyb_hash] is a hashtable that maps chars to thier colors*)

  type player = {
    name : string;
    mutable score : int;
    mutable cumlative_score : int;
  }
  (** [player] is a type that includes the player name, his score for each round
      is going to be the number of correct letters that he has + the number of
      attempts left * 100 and his total score. Score is a mutable int as it
      changes Cumlative_score resets to 0 after each game.*)

  val cat_hash : (int, game_category) Hashtbl.t
  (** [cat_hash] is a hash table of 1 element containing the game category
      chosen.*)

  val lwt_read_line : unit -> string Lwt.t
  (** [lwt_read_line] reads one complete line from ic and returns it without the
      end of line. End of line is either "\n" or "\r\n". If the end of input is
      reached before reading any character, End_of_file is raised. If it is
      reached before reading an end of line but characters have already been
      read, they are returned. This method also uses promises to not block the
      wordle timer from running while the user is inputting*)

  val select_category : unit -> unit
  (** [select_category] returns the selected category of the game by prompting
      the user. Returns [Four] if the four-letter wordle is chosen, [Five] if
      the five-letter wordle is chosen and [Six] if the six-letter Wordle is
      chosen. Forces the user to pick again if his input is not 4, 5 or 6 by
      running the function again.*)

  val select_difficulty : unit -> difficulty_level
  (** [select_difficulty] prompts the user to choose the difficulty level of the
      wordle. Returns the difficulty level they choose *)

  val select_time : unit -> int
  (** [select_time ()] gives the user the option of which time setting they'd
      like to play the game at. Returns either 30 seconds, 60s or 120s based on
      which time the user chooses. Forces the user to pick again if his input is
      not 1, 2 or 3.*)

  val get_max_attempts : difficulty_level -> int
  (**[get_max_attempts] matches the difficulty level argument to a number that
     represent the number of attempts that the wordle game round is going to
     have based on the difficulty level*)

  val qwerty_hash : keyb_hash
  (** [qwerty_hash] This is used as reference for [paint_keyboard] as reference
      for qwerty keyboard layout *)

  val keys : char list
  (** [keys] assigns qwerty letters to qwerty_has*)

  val reset_Hashtbl : keyb_hash -> unit
  (** [reset_Hashtbl] returns all qwert_hash values to Grey*)

  val read_file : string -> string list
  (** [read_file filename] reads the string contents of a file and outputs a
      string list code obtained from user aneccodeal on StackOverflow *)

  val word_list : game_category -> string list
  (** [word_list] takes a game_category as input and returns the associated
      word_list. Ex. if game_category is Four, it returns the associated txt
      file with all the four-letter words*)

  val double_newline : unit -> unit
  (** [double_newline] is a function that prints 2 lines in the terminal using
      print_newline ()*)

  val score_multiplier : int
  (** [score_multiplier] is a multiple to multiple score by if needed.*)

  val current_player : player ref
  (** current_player is the current player with type [player] who is playing in
      the game. *)

  val timed : bool ref
  (** [timed] is a boolean ref to control whether the wordle game is played in
      timed mode or untimed mode. It is mutable so it can be changed each round.*)

  val current_timer_task : unit Lwt.t ref
  (**[current_timer_task] is a mutable promise to start the time of the game *)

  val introduction : unit -> unit
  (**[introduction ()] prints the introduction page *)

  val settings_menu : unit -> unit
  (**[settings_menu] prints the introduction page *)

  val select_random_word : game_category -> string
  (**[select_random_word game_category] selects a string word from the list of
     words in the txt files depending on the category of the word*)

  val is_valid_word : string -> game_category -> bool
  (**[is_valid_word word category] matches category with word length, and then
     checks that its a valid word by checking its respective length and that its
     a member of word_list*)

  val is_correct_guess : string -> string -> bool
  (**[is_correct_guess target_word guessed_word] checks that target_word is
     equal to guessed_word*)

  val remove_duplicates : 'a list -> 'a list
  (**[remove_duplicates lst] removes any duplicate elements in a [lst] list*)

  val keep_count : int ref

  val map_green_chars : char list -> char list -> pairings -> int -> pairings
  (** [map_green_chars target_chars guess_chars color_letter_pairs index] maps
      the letters in the correct position to the green color and corresponding
      index*)

  val map_yellow_red_chars :
    char list -> char list -> pairings -> int -> pairings
  (** [map_yellow_red_chars target chars guess_chars color_letter_pairs index]
      maps the letters in the wrong position but in the target word to the
      yellow color and corresponding index. maps the letters not in the target
      word to the red color and corresponding index*)

  val map_chars_to_color : char list -> char list -> pairings
  (** [map_chars_to_color target_chars guess_chars] returns a pairings of maps
      from the green letters first, then the yellow, and red letters *)

  val sort_bindings : pairings -> pairings
  (** [sort_bindings char_color_list] Sorts the mappings based on the index in
      increasing order *)

  val match_color : char_color -> ANSITerminal.style list
  (** [match_color color] Matches color of type char_color with its
      corresponding ANSITerminal color*)

  val print_chars : pairings -> unit
  (** [print_chars char_color_list] Prints the letters in the map with their
      corresponding color and in the correct index*)

  val correct_letters_score : string list -> int -> string -> int
  (**[correct_letters guess_lst max_score target_word] is the highest number of
     correct letters achieved by the guesser. *)

  val char_list_to_string : char list -> string
  (** Converts a list of chars into a string*)

  val iter :
    string -> string -> string -> (char * int) list -> (char * int) list
  (* [iter head string1 string2 acc] adds a list of all the correct letters and
     their index of a string *)

  val update_colors : pairings -> keyb_hash -> (char * char_color) list
  (* [update_colors char_color_list qwerty_hash] maps the keys on a keybaord to
     a color *)

  val overall_correct_letters :
    string list -> (char * int) list -> string -> (char * int) list
  (**[overall_correct_letters guess_lst lst target_word] returns a list of all
     the correct letters guessed (green) as well as their index *)

  val print_keyboard_chars :
    string list -> string -> (char, char_color) Hashtbl.t -> unit
  (**[print_keyboard_chars guess_lst target_word qwertyhash] mutates the hashtbl
     of letters so it may print the updated color version of them at the bottom
     of the wordle screen*)

  val combine_list : string -> 'a list -> (char * 'a) list
  (* [combine_list target_word list_of_n] converts [target_word] into a char
     list and combines it with [list_of_n] to form a tuple *)

  val reveal_letter : string list -> string -> unit
  (**[reveal_letter target guess_lst] 2.0 finds how many correct_letters have
     been found and assures that hint stops when all the letters have been
     discovereed independlty*)

  val get_hint : string -> string list -> unit
  (** [get_hint] reveals one of the unguessed letters to the user. Returns unit*)

  val give_up : char list -> unit
  (**[give_up target_chars] prints the correct word prematurly as the user
     chooses to quit *)

  val process_guess : string -> int -> string list -> unit Lwt.t
  (** [process_guess target_word attempts_left guess_lst] asks the player to
      input a word and check if it is a member of the [word_list], "h" for a
      hint, or "g" to give up*)

  val process_valid_guess : string -> int -> string list -> string -> unit Lwt.t
  (** [process_valid_guess target_word attempts_left guess_lst guess] adds the
      player's most recent [guess] to [guess_list] and checks if the most recent
      [guess] is equal to the [target_word]. The player's current and overall
      score is also updated*)

  val wordle_game : string -> int -> string list -> unit Lwt.t
  (** [wordle_game target_word attempts_left guess_lst] prints out all the
      characters for each guess in [guess_list]. Calls [restart_game] once the
      player is out of attempts. The player's current and overall score is also
      updated*)

  val restart_game : unit -> unit Lwt.t
  (** [restart_game()] asks the player to input an "r" to restart the game
      (restarting the game means to run either [start_timed_game] or
      [start_untimed_game] depending on the game mode) or to input a "q" to quit
      the game. If anything else is inputed, [restart_game()] is called. *)

  val start_timed_game : unit -> unit Lwt.t
  (** [start_timed_game()] gets the game category, the difficulty level, and the
      time that the player requested through their inputs. Concurrently runs the
      [timer] with the [wordle_game]. Calls [restart_game()] when timer runs
      out. Prints the player's overall score *)

  val start_untimed_game : unit -> unit Lwt.t
  (** [start_untimed_game()] gets the game category and the difficulty level
      that the player requested through their inputs. Runs the [wordle_game]
      without a timer*)

  val start_game : unit -> unit
  (** [start_game()] chooses to run either [start_timed_game] or
      [start_untimed_game] in a promise depending on the player's timer input*)
end
