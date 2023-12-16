(* Approach: Our testing strategy aims to validate the correctness and
   efficiency of our system, focusing on a comprehensive OUnit test suite of
   unit tests using a similar foundational framework relative to our former
   assignments for the course. The tests were designed to cover a variety of
   scenarios, ranging from typical cases to edge cases, ensuring robustness
   against unexpected inputs and conditions. *)

(* What was tested: In order to ensure comprehensive coverage we tested for
   function behavior/functionality, edge cases, data consistency, and error
   handling. *)

(* What was omitted: We decided to omit integration testing because our primary
   goal was to ensure the correctness of individual units instead. We decided to
   additionally omit aspects of performance testing because our functions don't
   involve complex computations or large datasets, making performance less of a
   concern. *)

(* Correctness: The main argument defending the correctness of our program is
   backed by claims of comprehensiveness, consistency, transparency, and
   modularity. The implemented OUnit test suites embody comprehensiveness
   throughout its coverage ofa wide range of scenarios, including both standard
   and edge cases. Consistency is demonstrated through the utilization of the
   OUnit framework's repeatable test execution. The clear syntactic structure
   and semantics of tests make it easy to navigate what is being tested and why
   highlighting the transparency brought by the OUnit test suites. Modularity of
   the program is exemplified through each function's independent test features,
   allowing for an isolated verification of each component. *)

(* Automated Testing vs. Manual: All created tests were automated using the
   foundational OUnit framework. This includes assertions of expected outcomes
   and exception handling. Since all tests were designed for automated execution
   and validation, there was a lack of necessity for utilizing a manual testing
   framework. *)

(* Test Development Strategy: Our group utilized randomized, black box, and
   glass box testing approaches. Although we did not extensively utilize
   randomized testing, our select_random_word_test function incorporates
   randomness to verify its behavior. To incorporate a black box testing
   approach, we designed tests based on function specifications, instead of
   internal implementations, to validate external behavior. As for a glass box
   testing approach we designed the test cases of our OUnit test suite with
   internal logic in mind, ensuring coverage of all code paths, with respect to
   the implementation of the desired program to be verified. *)

open OUnit2
open Wordlerush.WordleRush

(* Function to test is_valid_word function *)
let is_valid_word_test out word cat =
  let is_valid = is_valid_word word cat in
  fun _ -> assert_equal out is_valid

(* Function to test is_correct_guess function *)
let is_correct_guess_test out target guess =
  let is_correct = is_correct_guess target guess in
  fun _ -> assert_equal out is_correct

(* Function to test get_max_attempts function *)
let get_max_attempts_test out level =
  let attempt = get_max_attempts level in
  fun _ -> assert_equal out attempt

(* Function to test read_file function *)
let read_file_test out file =
  let file_name = read_file file in
  fun _ -> assert_equal out file_name

(* Function to test correct_letters_score function *)
let correct_letters_score_test out g max t =
  let score = correct_letters_score g max t in
  fun _ -> assert_equal out score

(* Function to test char_list_to_string function *)
let char_list_to_string_test out list =
  let char = char_list_to_string list in
  fun _ -> assert_equal out char

(* Function to test remove_duplicates function *)
let remove_duplicates_test out list =
  let dup = remove_duplicates list in
  fun _ -> assert_equal out dup

(* Function to test overall_correct_letters function *)
let overall_correct_letters_test out guess list target =
  let overall = overall_correct_letters guess list target in
  fun _ -> assert_equal out overall

(* Function to test map_chars_to_color function *)
let map_chars_to_color_test out target guess =
  let map = map_chars_to_color target guess in
  fun _ -> assert_equal out map

(* Function to test map_yellow_red_chars function *)
let map_yellow_red_chars_test out target guess pair index =
  let map = map_yellow_red_chars target guess pair index in
  fun _ -> assert_equal out map

(* Function to test map_green_chars function *)
let map_green_chars_test out target guess pair index =
  let map = map_green_chars target guess pair index in
  fun _ -> assert_equal out map

(* Function to test sort_bindings function *)
let sort_bindings_test out bind =
  let bindings = sort_bindings bind in
  fun _ -> assert_equal out bindings

(* Function to test word_list function *)
let word_list_test out game =
  let list = word_list game in
  fun _ -> assert_equal out list

(* Function to test match_color function *)
let match_color_test out color =
  let map = match_color color in
  fun _ -> assert_equal out map

(* Function to test select_random_word function *)
let select_random_word_test out_len out_in_list cat _ =
  let word = select_random_word cat in
  let length = String.length word in
  let is_in_list = List.mem word (word_list cat) in
  assert_equal out_len length;
  assert_equal out_in_list is_in_list

(* Function to test invalid_category function *)
let test_invalid_category err cat _ =
  assert_raises err (fun () -> select_random_word cat)

(* Function to test iter function *)
let iter_test out head str1 str2 acc =
  let iterate = iter head str1 str2 acc in
  fun _ -> assert_equal out iterate

(* Function to test update_colors function *)
let update_colors_test out color hash =
  let update = update_colors color hash in
  fun _ -> assert_equal out update

(* Function to test combine_list function *)
let combine_list_test out target list =
  let combine = combine_list target list in
  fun _ -> assert_equal out combine

(* is_valid_word tests *)
let is_valid_word_tests =
  [
    (* Test five letter words *)
    "Test is_valid_word | valid word"
    >:: is_valid_word_test true "money" Classic;
    "Test is_valid_word | word not of length 5"
    >:: is_valid_word_test false "cool" Classic;
    "Test is_valid_word | made up word of length 5"
    >:: is_valid_word_test false "hriko" Classic;
    "Test is_valid_word | valid word with uppercase letters"
    >:: is_valid_word_test true "Hello" Classic;
    "Test is_valid_word | not valid word with numbers"
    >:: is_valid_word_test false "12345" Classic;
    "Test is_valid_word | empty word" >:: is_valid_word_test false "" Classic;
    (* Test four letter words *)
    "Test is_valid_word | valid word" >:: is_valid_word_test true "four" Four;
    "Test is_valid_word | word not of length 4"
    >:: is_valid_word_test false "state" Four;
    "Test is_valid_word | made up word of length 4"
    >:: is_valid_word_test false "gooo" Four;
    "Test is_valid_word | valid word with uppercase letters"
    >:: is_valid_word_test true "Tape" Four;
    "Test is_valid_word | not valid word with numbers"
    >:: is_valid_word_test false "1234" Four;
    "Test is_valid_word | empty word" >:: is_valid_word_test false "" Four;
    (* Test six letter words *)
    "Test is_valid_word | valid word" >:: is_valid_word_test true "police" Six;
    "Test is_valid_word | word not of length 6"
    >:: is_valid_word_test false "cool" Six;
    "Test is_valid_word | made up word of length 6"
    >:: is_valid_word_test false "grtops" Six;
    "Test is_valid_word | valid word with uppercase letters"
    >:: is_valid_word_test true "Please" Six;
    "Test is_valid_word | not valid word with numbers"
    >:: is_valid_word_test false "123456" Six;
    "Test is_valid_word | empty word" >:: is_valid_word_test false "" Six;
  ]

(* is_correct_guess tests *)
let is_correct_guess_tests =
  [
    "Test is_correct_guess | words match"
    >:: is_correct_guess_test true "train" "train";
    "Test is_correct_guess | words do not match"
    >:: is_correct_guess_test false "train" "cool";
    "Test is_correct_guess | matching words with different cases"
    >:: is_correct_guess_test true "train" "Train";
    "Test is_correct_guess | matching words with leading and trailing spaces"
    >:: is_correct_guess_test true "train" "  train  ";
    "Test is_correct_guess | matching words with extra spaces in between"
    >:: is_correct_guess_test false "big  cat" "big cat";
    "Test is_correct_guess | matching empty words"
    >:: is_correct_guess_test true "" "";
    "Test is_correct_guess | words do not match with numbers and special \
     characters"
    >:: is_correct_guess_test false "word@123" "word123";
    "Test is_correct_guess | words do not match with different lengths"
    >:: is_correct_guess_test false "apple" "app";
  ]

(* get_max_attempts tests *)
let get_max_attempts_tests =
  [
    "Test get_max_attempts | easy attempt" >:: get_max_attempts_test 8 Easy;
    "Test get_max_attempts | medium attempt" >:: get_max_attempts_test 6 Medium;
    "Test get_max_attempts | hard attempt" >:: get_max_attempts_test 4 Hard;
  ]

(* read_file tests *)
let read_file_tests =
  [
    "Test read_file | empty file with no text"
    >:: read_file_test [] "data/example1.txt";
    "test_read_file | non-empty file/one line"
    >:: read_file_test [ "Line 1" ] "data/example2.txt";
    "test_read_file | non-empty file/multiple lines"
    >:: read_file_test [ "Line 1"; "Line 2"; "Line 3" ] "data/example3.txt";
    "test_read_file | non-empty file with whitespace"
    >:: read_file_test
          [ "Line 1"; ""; "Line 2"; ""; "Line 3" ]
          "data/example4.txt";
    "test_read_file | non-empty file with special characters"
    >:: read_file_test [ "@"; ""; "."; ""; "#" ] "data/example5.txt";
    "test_read_file | non-empty file with leading trailing spaces"
    >:: read_file_test
          [ "trail one "; "trail two  "; "trail three   " ]
          "data/example6.txt";
  ]

(* correct_letters_score tests *)
let correct_letters_score_tests =
  [
    "Test correct_letters_score | 6 correct letters"
    >:: correct_letters_score_test 6 [ "g"; "u"; "e"; "s"; "s"; "s" ] 6 "guesss";
    "Test correct_letters_score | 5 correct letters"
    >:: correct_letters_score_test 5 [ "g"; "u"; "e"; "s"; "s" ] 5 "guess";
    "Test correct_letters_score | 4 correct letter"
    >:: correct_letters_score_test 4 [ "g"; "u"; "e"; "s"; "s" ] 4 "gueso";
    "Test correct_letters_score | 3 correct letters"
    >:: correct_letters_score_test 3 [ "g"; "u"; "e"; "s"; "s" ] 3 "gueoo";
    "Test correct_letters_score | 2 correct letters"
    >:: correct_letters_score_test 2 [ "g"; "u"; "e"; "s"; "s" ] 2 "guooo";
    "Test correct_letters_score | 1 correct letters"
    >:: correct_letters_score_test 1 [ "g"; "u"; "e"; "s"; "s" ] 1 "goooo";
    "Test correct_letters_score | 0 correct letters"
    >:: correct_letters_score_test 0 [ "g"; "u"; "e"; "s"; "s" ] 0 "money";
  ]

(* char_list_to_string tests *)
let char_list_to_string_tests =
  [
    "Test char_to_string | empty character_to_string"
    >:: char_list_to_string_test "" [];
    "Test char_list_to_string | single character list"
    >:: char_list_to_string_test "a" [ 'a' ];
    "Test char_list_to_string | multiple characters"
    >:: char_list_to_string_test "hello" [ 'h'; 'e'; 'l'; 'l'; 'o' ];
    "Test char_list_to_string | mix of upper and lower case"
    >:: char_list_to_string_test "OCaml" [ 'O'; 'C'; 'a'; 'm'; 'l' ];
    "Test char_list_to_string | special characters and digits"
    >:: char_list_to_string_test "!@#$123" [ '!'; '@'; '#'; '$'; '1'; '2'; '3' ];
  ]

(* remove_duplicates tests *)
let remove_duplicates_tests =
  [
    "Test remove_duplicates | empty list with dups removed"
    >:: remove_duplicates_test [] [];
    "Test remove_duplicates | list with no duplicates"
    >:: remove_duplicates_test [ 1; 2; 3; 4 ] [ 1; 2; 3; 4 ];
    "Test remove_duplicates | list with duplicates at the beginning"
    >:: remove_duplicates_test [ 1; 2; 3; 4 ] [ 1; 1; 2; 3; 4 ];
    "Test remove_duplicates | list with duplicates at the end"
    >:: remove_duplicates_test [ 1; 2; 3; 4 ] [ 1; 2; 3; 4; 4 ];
    "Test remove_duplicates | list with consecutive duplicates"
    >:: remove_duplicates_test [ 1; 2; 3; 4 ] [ 1; 2; 2; 3; 3; 4 ];
    "Test remove_duplicates | list with non-consecutive duplicates"
    >:: remove_duplicates_test [ 1; 2; 4; 3 ] [ 1; 2; 3; 2; 4; 3 ];
    "Test remove_duplicates | list with duplicates in the middle"
    >:: remove_duplicates_test [ 1; 3; 2; 4 ] [ 1; 2; 3; 2; 4 ];
    "Test remove_duplicates | list with all duplicates"
    >:: remove_duplicates_test [ 1 ] [ 1; 1; 1; 1; 1 ];
  ]

(* overall_correct_letters tests *)
let overall_correct_letters_tests =
  [
    "Test overall_correct_letters | empty (char * int) list with no correct \
     letters"
    >:: overall_correct_letters_test [] [] [] "";
    "Test overall_correct_letters | no correct letters in the guessed word"
    >:: overall_correct_letters_test
          [ ('a', 2); ('b', 4) ]
          []
          [ ('a', 2); ('b', 4) ]
          "hello";
    "Test overall_correct_letters | correct letters at different positions"
    >:: overall_correct_letters_test
          [ ('l', 2); ('o', 4) ]
          []
          [ ('l', 2); ('o', 4) ]
          "hello";
    "Test overall_correct_letters | correct letters and correct positions"
    >:: overall_correct_letters_test
          [ ('h', 1); ('e', 2); ('l', 3); ('o', 4) ]
          []
          [ ('h', 1); ('e', 2); ('l', 3); ('o', 4) ]
          "hello";
    "Test overall_correct_letters | correct letters, correct positions, and \
     extra correct letters"
    >:: overall_correct_letters_test
          [ ('x', 5); ('y', 6) ]
          []
          [ ('x', 5); ('y', 6) ]
          "helloxyz";
    "Test overall_correct_letters | correct letters and extra correct letters"
    >:: overall_correct_letters_test
          [ ('x', 3); ('y', 4) ]
          []
          [ ('x', 3); ('y', 4) ]
          "helloxyz";
    "Test overall_correct_letters | no guessed word"
    >:: overall_correct_letters_test [ ('x', 3) ] [] [ ('x', 3) ] "";
    "Test overall_correct_letters | empty correct letters and correct \
     positions list"
    >:: overall_correct_letters_test [] [] [] "abc";
    "Test overall_correct_letters | correct positions with no correct letters"
    >:: overall_correct_letters_test
          [ ('a', 2); ('b', 4) ]
          []
          [ ('a', 2); ('b', 4) ]
          "xyz";
    "Test overall_correct_letters | empty guessed word"
    >:: overall_correct_letters_test
          [ ('a', 1); ('b', 2) ]
          []
          [ ('a', 1); ('b', 2) ]
          "";
    "Test overall_correct_letters | empty guessed word with correct positions"
    >:: overall_correct_letters_test
          [ ('a', 2); ('b', 4) ]
          []
          [ ('a', 2); ('b', 4) ]
          "";
    "Test overall_correct_letters | correct letters and correct positions with \
     duplicates"
    >:: overall_correct_letters_test
          [ ('l', 2); ('o', 2) ]
          []
          [ ('l', 2); ('o', 2) ]
          "hello";
    "Test overall_correct_letters | correct letters and correct positions with \
     duplicates and extra correct letters"
    >:: overall_correct_letters_test
          [
            ('z', 7);
            ('o', 4);
            ('l', 2);
            ('e', 1);
            ('h', 0);
            ('x', 5);
            ('y', 6);
            ('x', 5);
            ('y', 6);
          ]
          [ "helloxyz" ]
          [ ('x', 5); ('y', 6) ]
          "helloxyz";
    "Test overall_correct_letters | correct letters with duplicates and \
     correct positions with duplicates"
    >:: overall_correct_letters_test
          [
            ('e', 1);
            ('h', 0);
            ('l', 2);
            ('o', 2);
            ('l', 4);
            ('o', 4);
            ('l', 2);
            ('o', 2);
            ('l', 4);
            ('o', 4);
          ]
          [ "hello" ]
          [ ('l', 2); ('o', 2); ('l', 4); ('o', 4) ]
          "hello";
    "Test overall_correct_letters | correct letters with duplicates and \
     correct positions with duplicates and extra correct letters"
    >:: overall_correct_letters_test
          [
            ('z', 7);
            ('y', 6);
            ('x', 5);
            ('e', 1);
            ('h', 0);
            ('l', 2);
            ('o', 2);
            ('l', 4);
            ('o', 4);
            ('l', 2);
            ('o', 2);
            ('l', 4);
            ('o', 4);
          ]
          [ "helloxyz" ]
          [ ('l', 2); ('o', 2); ('l', 4); ('o', 4) ]
          "helloxyz";
    "Test overall_correct_letters | guessed word longer than list of pairs"
    >:: overall_correct_letters_test
          [ ('h', 0); ('e', 1) ]
          []
          [ ('h', 0); ('e', 1) ]
          "hellohellohello";
    "Test overall_correct_letters | guessed word shorter than list of pairs"
    >:: overall_correct_letters_test
          [ ('h', 0); ('e', 1); ('l', 2); ('o', 3); ('x', 4) ]
          []
          [ ('h', 0); ('e', 1); ('l', 2); ('o', 3); ('x', 4) ]
          "hel";
    "Test overall_correct_letters | repeated characters in guessed word with \
     different positions"
    >:: overall_correct_letters_test
          [ ('l', 2); ('l', 3) ]
          []
          [ ('l', 2); ('l', 3) ]
          "hello";
    "Test overall_correct_letters | all characters correct but in wrong \
     positions"
    >:: overall_correct_letters_test
          [
            ('o', 0);
            ('l', 1);
            ('l', 2);
            ('e', 3);
            ('h', 4);
            ('o', 5);
            ('l', 6);
            ('l', 7);
            ('e', 8);
            ('h', 8);
          ]
          []
          [
            ('o', 0);
            ('l', 1);
            ('l', 2);
            ('e', 3);
            ('h', 4);
            ('o', 5);
            ('l', 6);
            ('l', 7);
            ('e', 8);
            ('h', 8);
          ]
          "hellohello";
    "Test overall_correct_letters | correct letters with non-sequential \
     positions"
    >:: overall_correct_letters_test
          [ ('h', 4); ('e', 2); ('l', 0); ('o', 3) ]
          []
          [ ('h', 4); ('e', 2); ('l', 0); ('o', 3) ]
          "hello";
    "Test overall_correct_letters | correct letters in reversed order"
    >:: overall_correct_letters_test
          [ ('o', 0); ('l', 1); ('e', 2); ('h', 3) ]
          []
          [ ('o', 0); ('l', 1); ('e', 2); ('h', 3) ]
          "hello";
  ]

(* map_yellow_red_chars tests *)
let map_yellow_red_chars_tests =
  [
    "Test map_yellow_red_chars | empty mapping of character to char \
     transformation with a zero index"
    >:: map_yellow_red_chars_test [] [] [] [] 0;
    "Test map_yellow_red_chars | empty mapping of character to char \
     transformation with a non-zero index"
    >:: map_yellow_red_chars_test [] [] [] [] 5;
    "Test map_yellow_red_chars | non-empty mapping of character to char \
     transformation with a zero index"
    >:: map_yellow_red_chars_test
          [
            (0, ('h', Yellow));
            (1, ('e', Yellow));
            (2, ('l', Yellow));
            (3, ('l', Yellow));
            (4, ('o', Yellow));
          ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [] 0;
    "Test map_yellow_red_chars | non-empty mapping of character to char \
     transformation with a non-zero index"
    >:: map_yellow_red_chars_test
          [
            (30, ('h', Yellow));
            (31, ('e', Yellow));
            (32, ('l', Yellow));
            (33, ('l', Yellow));
            (34, ('o', Yellow));
          ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [] 30;
    "Test map_yellow_red_chars | non-empty mapping of character to char \
     transformation with a non-zero index"
    >:: map_yellow_red_chars_test
          [
            (30, ('h', Yellow));
            (31, ('e', Yellow));
            (32, ('l', Yellow));
            (33, ('l', Yellow));
            (34, ('o', Yellow));
          ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [] 30;
    "Test map_yellow_red_chars | non-empty mapping of character to char \
     transformation with a non-zero index and inputting a Green character"
    >:: map_yellow_red_chars_test
          [
            (30, ('h', Yellow));
            (31, ('e', Yellow));
            (32, ('l', Yellow));
            (33, ('l', Yellow));
            (34, ('o', Yellow));
            (0, ('h', Green));
          ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [ (0, ('h', Green)) ]
          30;
    "Test map_yellow_red_chars | non-empty mapping of character to char \
     transformation with a non-zero index and inputting a Red character"
    >:: map_yellow_red_chars_test
          [
            (30, ('h', Yellow));
            (31, ('e', Yellow));
            (32, ('l', Yellow));
            (33, ('l', Yellow));
            (34, ('o', Yellow));
            (0, ('z', Red));
          ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [ (0, ('z', Red)) ]
          30;
    "Test map_yellow_red_chars | non-empty mapping of character to char \
     transformation with a non-zero index and inputting a Red character with \
     no target phrase found"
    >:: map_yellow_red_chars_test
          [ (2, ('z', Red)) ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          []
          [ (2, ('z', Red)) ]
          30;
    "Test map_yellow_red_chars | non-empty mapping of character to char \
     transformation with a non-zero index and inputting a Green character with \
     no target phrase found"
    >:: map_yellow_red_chars_test
          [ (2, ('e', Green)) ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          []
          [ (2, ('e', Green)) ]
          30;
    "Test map_yellow_red_chars | non-empty mapping of character to char \
     transformation with a non-zero index and inputting a Yellow character \
     with no target phrase"
    >:: map_yellow_red_chars_test
          [ (2, ('o', Yellow)) ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          []
          [ (2, ('o', Yellow)) ]
          30;
    "Test map_yellow_red_chars | non-empty mapping of character to char \
     transformation with a zero index and inputting a single Yellow character \
     with an identical target phrase"
    >:: map_yellow_red_chars_test [ (0, ('o', Yellow)) ] [ 'o' ] [ 'o' ] [] 0;
    "Test map_yellow_red_chars | non-empty mapping with multiple Green \
     characters"
    >:: map_yellow_red_chars_test
          [ (10, ('h', Green)); (11, ('i', Green)); (12, ('!', Green)) ]
          [ 'h'; 'i'; '!' ] [ 'h'; 'i'; '!' ]
          [ (10, ('h', Green)); (11, ('i', Green)); (12, ('!', Green)) ]
          10;
    "Test map_yellow_red_chars | alternating Yellow and Red characters"
    >:: map_yellow_red_chars_test
          [ (5, ('a', Yellow)); (6, ('b', Red)); (8, ('d', Red)) ]
          [ 'a'; 'b'; 'c'; 'd' ] [ 'a'; 'c' ]
          [ (6, ('b', Red)); (8, ('d', Red)) ]
          5;
    "Test map_yellow_red_chars | overlapping Yellow and Green characters"
    >:: map_yellow_red_chars_test
          [ (15, ('x', Green)); (17, ('z', Green)) ]
          [ 'x'; 'y'; 'z' ] [ 'y' ]
          [ (15, ('x', Green)); (17, ('z', Green)) ]
          15;
    "Test map_yellow_red_chars | large index with multiple character colors"
    >:: map_yellow_red_chars_test
          [ (100, ('p', Yellow)); (101, ('q', Red)); (102, ('r', Green)) ]
          [ 'p'; 'q'; 'r'; 's' ] [ 'p'; 's' ]
          [ (101, ('q', Red)); (102, ('r', Green)) ]
          100;
    "Test map_yellow_red_chars | non-sequential indices"
    >:: map_yellow_red_chars_test
          [ (3, ('u', Yellow)); (7, ('v', Green)); (12, ('w', Red)) ]
          [ 'u'; 'v'; 'w' ] [ 'u' ]
          [ (7, ('v', Green)); (12, ('w', Red)) ]
          3;
    "Test map_yellow_red_chars | characters mapped to all three colors"
    >:: map_yellow_red_chars_test
          [ (0, ('a', Yellow)); (1, ('a', Red)); (2, ('a', Green)) ]
          [ 'a'; 'a'; 'a' ] [ 'a' ]
          [ (1, ('a', Red)); (2, ('a', Green)) ]
          0;
    "Test map_yellow_red_chars | repeated characters with different indices"
    >:: map_yellow_red_chars_test
          [ (0, ('b', Yellow)); (1, ('b', Red)) ]
          [ 'b'; 'b'; 'b' ] [ 'b'; 'b' ]
          [ (1, ('b', Red)) ]
          0;
    "Test map_yellow_red_chars | overlapping indices with different characters \
     and colors"
    >:: map_yellow_red_chars_test
          [ (0, ('d', Green)) ]
          [ 'c'; 'd' ] [ 'c' ]
          [ (0, ('d', Green)) ]
          0;
    "Test map_yellow_red_chars | characters with no matching colors"
    >:: map_yellow_red_chars_test [] [ 'e' ] [] [] 0;
    "Test map_yellow_red_chars | boundary conditions with high indices"
    >:: map_yellow_red_chars_test
          [ (1000, ('f', Yellow)) ]
          [ 'f'; 'g' ] [ 'f' ] [] 1000;
  ]

(* map_chars_to_color tests *)
let map_chars_to_color_tests =
  [
    "Test map_chars_to_color | empty mapping of character to color \
     transformation"
    >:: map_chars_to_color_test [] [] [];
    "Test map_chars_to_color | mapping of character to one green color \
     transformation"
    >:: map_chars_to_color_test [ (0, ('x', Green)) ] [ 'x' ] [ 'x' ];
    "Test map_chars_to_color | mapping of character to one yellow color \
     transformation"
    >:: map_chars_to_color_test
          [ (0, ('e', Yellow)) ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [ 'e' ];
    "Test map_chars_to_color | mapping of character to one grey color  \
     transformation"
    >:: map_chars_to_color_test
          [ (0, ('x', Red)) ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [ 'x' ];
    "Test map_chars_to_color | mapping of characters to green and yellow color \
     transformation of differing character list lengths"
    >:: map_chars_to_color_test
          [ (1, ('x', Yellow)); (0, ('x', Green)) ]
          [ 'x'; 'y'; 'z' ] [ 'x'; 'x' ];
    "Test map_chars_to_color | mapping of characters to green and yellow \
     color  transformation with one instance of the same character"
    >:: map_chars_to_color_test
          [ (1, ('x', Yellow)); (2, ('x', Yellow)); (0, ('x', Green)) ]
          [ 'x'; 'y'; 'z' ] [ 'x'; 'x'; 'x' ];
    "Test map_chars_to_color | mapping of all green color characters \
     transformation with identical character lists"
    >:: map_chars_to_color_test
          [
            (0, ('h', Green));
            (1, ('e', Green));
            (2, ('l', Green));
            (3, ('l', Green));
            (4, ('o', Green));
          ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ];
    "Test map_chars_to_color | mapping of multiple characters to red color \
     transformation"
    >:: map_chars_to_color_test
          [ (0, ('d', Red)); (1, ('e', Red)); (2, ('f', Red)) ]
          [ 'a'; 'b'; 'c' ] [ 'd'; 'e'; 'f' ];
    "Test map_chars_to_color | mapping of characters to alternating green and \
     yellow color transformation"
    >:: map_chars_to_color_test
          [ (1, ('o', Yellow)); (0, ('m', Green)); (2, ('o', Green)) ]
          [ 'm'; 'n'; 'o' ] [ 'm'; 'o'; 'o' ];
    "Test map_chars_to_color | mapping of characters with non-consecutive \
     indices"
    >:: map_chars_to_color_test
          [ (1, ('v', Yellow)); (0, ('s', Green)) ]
          [ 's'; 't'; 'u'; 'v'; 'w' ]
          [ 's'; 'v' ];
    "Test map_chars_to_color | mapping a single character in a longer list"
    >:: map_chars_to_color_test
          [ (0, ('p', Yellow)) ]
          [ 'm'; 'n'; 'o'; 'p'; 'q' ]
          [ 'p' ];
    "Test map_chars_to_color | mapping with no color match found"
    >:: map_chars_to_color_test [] [ 'a'; 'b'; 'c' ] [];
    "Test map_chars_to_color | mapping with overlapping indices and different \
     colors"
    >:: map_chars_to_color_test
          [ (0, ('y', Yellow)); (1, ('y', Green)) ]
          [ 'x'; 'y'; 'z' ] [ 'y'; 'y' ];
    "Test map_chars_to_color | characters mapped to multiple colors"
    >:: map_chars_to_color_test
          [ (1, ('y', Yellow)); (0, ('y', Green)) ]
          [ 'y' ] [ 'y'; 'y' ];
  ]

(* map_green_chars tests *)
let map_green_chars_tests =
  [
    "Test map_green_chars | empty mapping of character to char transformation \
     with a zero index"
    >:: map_green_chars_test [] [] [] [] 0;
    "Test map_green_chars | empty mapping of character to char transformation \
     with a non-zero index"
    >:: map_green_chars_test [] [] [] [] 5;
    "Test map_green_chars | non-empty mapping of character to char \
     transformation with a zero index"
    >:: map_green_chars_test
          [
            (0, ('h', Green));
            (1, ('e', Green));
            (2, ('l', Green));
            (3, ('l', Green));
            (4, ('o', Green));
          ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [] 0;
    "Test map_green_chars | non-empty mapping of character to char \
     transformation with a non-zero index"
    >:: map_green_chars_test
          [
            (30, ('h', Green));
            (31, ('e', Green));
            (32, ('l', Green));
            (33, ('l', Green));
            (34, ('o', Green));
          ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [ 'h'; 'e'; 'l'; 'l'; 'o' ]
          [] 30;
    "Test map_green_chars | mapping of a single character to Green"
    >:: map_green_chars_test
          [ (0, ('a', Green)) ]
          [ 'a'; 'b'; 'c' ] [ 'a' ] [] 0;
    "Test map_green_chars | mixed mapping of Green and non-Green characters"
    >:: map_green_chars_test
          [ (0, ('x', Green)) ]
          [ 'x'; 'y'; 'z' ] [ 'x'; 'z' ] [] 0;
    "Test map_green_chars | non-sequential indices with Green characters"
    >:: map_green_chars_test [] [ 'l'; 'm'; 'n'; 'o'; 'p' ] [ 'm'; 'o' ] [] 2;
    "Test map_green_chars | all characters Green at a high index"
    >:: map_green_chars_test
          [ (100, ('u', Green)); (101, ('v', Green)); (102, ('w', Green)) ]
          [ 'u'; 'v'; 'w' ] [ 'u'; 'v'; 'w' ] [] 100;
    "Test map_green_chars | overlapping Green characters at different indices"
    >:: map_green_chars_test
          [ (4, ('p', Green)) ]
          [ 'o'; 'p'; 'q' ] [ 'p'; 'p' ] [] 3;
    "Test map_green_chars | no Green characters found"
    >:: map_green_chars_test [] [ 'a'; 'b'; 'c' ] [] [] 1;
    "Test map_green_chars | only Green characters in a long list"
    >:: map_green_chars_test
          [
            (0, ('a', Green));
            (1, ('b', Green));
            (2, ('c', Green));
            (3, ('d', Green));
            (4, ('e', Green));
          ]
          [ 'a'; 'b'; 'c'; 'd'; 'e' ]
          [ 'a'; 'b'; 'c'; 'd'; 'e' ]
          [] 0;
    "Test map_green_chars | repeated characters with Green and non-Green mix"
    >:: map_green_chars_test
          [ (0, ('a', Green)); (1, ('a', Green)) ]
          [ 'a'; 'a'; 'a' ] [ 'a'; 'a' ] [] 0;
    "Test map_green_chars | characters with multiple colors"
    >:: map_green_chars_test
          [ (0, ('b', Green)); (1, ('b', Green)) ]
          [ 'b'; 'b'; 'b' ] [ 'b'; 'b' ] [] 0;
    "Test map_green_chars | characters after Green characters"
    >:: map_green_chars_test
          [ (0, ('e', Green)); (1, ('f', Green)) ]
          [ 'e'; 'f'; 'g'; 'h' ] [ 'e'; 'f' ] [] 0;
  ]

(* sort_bindings tests *)
let sort_bindings_tests =
  [
    "Test sort_bindings | sorting an empty binding of pairings"
    >:: sort_bindings_test [] [];
    "Test sort_bindings | sorting a one element binding of pairings"
    >:: sort_bindings_test [ (0, ('a', Red)) ] [ (0, ('a', Red)) ];
    "Test sort_bindings | sorting a multi-element binding of pairings with the \
     same color"
    >:: sort_bindings_test
          [ (0, ('a', Red)); (1, ('b', Red)) ]
          [ (0, ('a', Red)); (1, ('b', Red)) ];
    "Test sort_bindings | sorting a multi-element binding of pairings with \
     different colors"
    >:: sort_bindings_test
          [ (0, ('a', Red)); (1, ('b', Green)); (2, ('c', Yellow)) ]
          [ (0, ('a', Red)); (1, ('b', Green)); (2, ('c', Yellow)) ];
    "Test sort_bindings | sorting a multi-element binding of pairings with \
     different colors of the same index"
    >:: sort_bindings_test
          [ (0, ('a', Red)); (1, ('b', Green)) ]
          [ (0, ('a', Red)); (1, ('b', Green)); (0, ('c', Yellow)) ];
    "Test sort_bindings | sorting a multi-element binding of pairings with \
     different colors of the same index in alternating sequencing"
    >:: sort_bindings_test
          [ (0, ('a', Red)); (1, ('b', Yellow)) ]
          [
            (0, ('a', Red));
            (1, ('b', Yellow));
            (0, ('c', Yellow));
            (1, ('d', Yellow));
          ];
    "Test sort_bindings | sorting a multi-element binding with mixed indices \
     and colors"
    >:: sort_bindings_test
          [
            (1, ('c', Yellow));
            (2, ('b', Green));
            (3, ('a', Red));
            (4, ('d', Red));
            (5, ('e', Yellow));
          ]
          [
            (5, ('e', Yellow));
            (3, ('a', Red));
            (2, ('b', Green));
            (1, ('c', Yellow));
            (4, ('d', Red));
          ];
    "Test sort_bindings | sorting a multi-element binding with duplicate colors"
    >:: sort_bindings_test
          [
            (0, ('a', Green));
            (1, ('b', Red));
            (2, ('c', Green));
            (3, ('d', Red));
            (4, ('e', Green));
          ]
          [
            (0, ('a', Green));
            (2, ('c', Green));
            (4, ('e', Green));
            (1, ('b', Red));
            (3, ('d', Red));
          ];
    "Test sort_bindings | sorting a large multi-element binding with duplicate \
     indices and colors"
    >:: sort_bindings_test
          [
            (0, ('a', Green));
            (1, ('b', Red));
            (2, ('f', Red));
            (3, ('d', Red));
            (4, ('e', Green));
            (5, ('g', Yellow));
          ]
          [
            (0, ('a', Green));
            (1, ('b', Red));
            (0, ('c', Green));
            (3, ('d', Red));
            (4, ('e', Green));
            (2, ('f', Red));
            (5, ('g', Yellow));
            (1, ('h', Red));
            (2, ('i', Red));
            (0, ('j', Yellow));
          ];
    "Test sort_bindings | sorting with all elements having the same index"
    >:: sort_bindings_test
          [ (0, ('a', Grey)) ]
          [ (0, ('a', Grey)); (0, ('b', Green)); (0, ('c', Yellow)) ];
    "Test sort_bindings | sorting with randomized order of inputs"
    >:: sort_bindings_test
          [ (0, ('a', Red)); (1, ('c', Yellow)); (2, ('b', Green)) ]
          [ (2, ('b', Green)); (0, ('a', Red)); (1, ('c', Yellow)) ];
    "Test sort_bindings | sorting with repeated elements"
    >:: sort_bindings_test
          [ (0, ('a', Red)); (1, ('b', Green)) ]
          [ (0, ('a', Red)); (0, ('a', Red)); (1, ('b', Green)) ];
    "Test sort_bindings | sorting with no sorting needed"
    >:: sort_bindings_test
          [ (0, ('a', Red)); (1, ('b', Green)); (2, ('c', Yellow)) ]
          [ (0, ('a', Red)); (1, ('b', Green)); (2, ('c', Yellow)) ];
  ]

(* word_list tests *)
let word_list_tests =
  [
    "Test word_list | Pairing Classic game category with the dictionary of \
     five-letter words"
    >:: word_list_test (read_file "data/sgb-5-letter.txt") Classic;
    "Test word_list | Pairing Four game category with the dictionary of \
     four-letter words"
    >:: word_list_test (read_file "data/sgb-4-letter.txt") Four;
    "Test word_list | Pairing Six game category with the dictionary of \
     six-letter words"
    >:: word_list_test (read_file "data/sgb-6-letter.txt") Six;
  ]

(* match_color tests *)
let match_color_tests =
  [
    "Test match_color | Pairing Green char_color with the ANSITerminal.green \
     color"
    >:: match_color_test [ ANSITerminal.green ] Green;
    "Test match_color | Pairing Yellow char_color with the ANSITerminal.green \
     color"
    >:: match_color_test [ ANSITerminal.yellow ] Yellow;
    "Test match_color | Pairing Red char_color with the ANSITerminal.yellow \
     color"
    >:: match_color_test [ ANSITerminal.red ] Red;
    "Test match_color | Pairing Red char_color with the ANSITerminal.yellow \
     color"
    >:: match_color_test [ ANSITerminal.default ] Grey;
  ]

(* select_random_word tests *)
let select_random_word_tests =
  [
    "Test select_random_word | randomness check for 5 letter word"
    >:: select_random_word_test 5 true Classic;
    "Test select_random_word | randomness check for 4 letter word"
    >:: select_random_word_test 4 true Four;
    "Test select_random_word | randomness check for 6 letter word"
    >:: select_random_word_test 6 true Six;
    "Test select_random_word | randomness check for Blank category"
    >:: test_invalid_category (Failure "not a category") Blank;
  ]

(* iter tests *)
let iter_tests =
  [
    "Test iter_tests | iterate through a sequence of an empty list with empty \
     strings" >:: iter_test [] "" "" "" [];
    "Test iter_tests | iterate through a sequence of a one element non-empty \
     list with empty strings"
    >:: iter_test [ ('a', 0) ] "" "" "" [ ('a', 0) ];
    "Test iter_tests | iterate through a sequence of a multi-element non-empty \
     list with empty strings at different indexes"
    >:: iter_test
          [ ('a', 0); ('b', 1); ('c', 2) ]
          "" "" ""
          [ ('a', 0); ('b', 1); ('c', 2) ];
    "Test iter_tests | iterate through a sequence of an empty list with empty \
     strings but a value for the head"
    >:: iter_test [] "abc" "" "" [];
    "Test iter_tests | iterate through a sequence of a non-empty list with \
     empty strings at the same index and string values"
    >:: iter_test
          [ ('a', 0); ('b', 1); ('c', 2); ('a', 0) ]
          "abc" "" ""
          [ ('a', 0); ('b', 1); ('c', 2); ('a', 0) ];
    "Test iter_tests | iterate through a sequence of a non-empty list with \
     empty strings all at the same index and string values"
    >:: iter_test
          [ ('a', 8); ('b', 8); ('c', 8); ('a', 8) ]
          "abc" "" ""
          [ ('a', 8); ('b', 8); ('c', 8); ('a', 8) ];
    "Test iter_tests | iterate through a sequence of a non-empty list with \
     empty strings all with the same character and string values"
    >:: iter_test
          [ ('z', 0); ('z', 1); ('z', 2); ('z', 3) ]
          "abc" "" ""
          [ ('z', 0); ('z', 1); ('z', 2); ('z', 3) ];
    "Test iter_tests | iterate through a sequence of a non-empty list with \
     empty strings at the same index and empty string values"
    >:: iter_test
          [ ('a', 0); ('f', 5); ('e', 14); ('a', 0) ]
          "" "" ""
          [ ('a', 0); ('f', 5); ('e', 14); ('a', 0) ];
    "Test iter_tests | iterate through a sequence of a non-empty list with \
     empty strings all at the same index and empty string values"
    >:: iter_test
          [ ('a', 26); ('b', 26); ('c', 26); ('a', 26) ]
          "" "" ""
          [ ('a', 26); ('b', 26); ('c', 26); ('a', 26) ];
    "Test iter_tests | iterate through a sequence of a non-empty list with \
     empty strings all with the same character and empty string values"
    >:: iter_test
          [ ('i', 3); ('i', 2); ('i', 1); ('i', 0) ]
          "" "" ""
          [ ('i', 3); ('i', 2); ('i', 1); ('i', 0) ];
  ]

(* update_colors tests *)
let update_colors_tests =
  [
    "Test update_colors | empty list of pairings with a standardized \
     qwerty_hash letter value"
    >:: update_colors_test [] [] qwerty_hash;
    "Test update_colors | non-empty list of a one-element pairing with a \
     standardized qwerty_hash letter value and an index of zero"
    >:: update_colors_test [ ('a', Green) ] [ (0, ('a', Green)) ] qwerty_hash;
    "Test update_colors | non-empty list of a one-element pairing with a \
     standardized qwerty_hash letter value and a non-zero index"
    >:: update_colors_test [ ('a', Green) ] [ (5, ('a', Green)) ] qwerty_hash;
    "Test update_colors | non-empty list of multi-element pairings with a \
     standardized qwerty_hash letter value with indexes of zero"
    >:: update_colors_test
          [ ('a', Yellow); ('b', Grey) ]
          [ (0, ('a', Yellow)); (0, ('b', Grey)) ]
          qwerty_hash;
    "Test update_colors | non-empty list of multi-element pairings with a \
     standardized qwerty_hash letter value with indexes of varying values"
    >:: update_colors_test
          [ ('a', Green); ('b', Yellow); ('c', Grey) ]
          [ (56, ('a', Green)); (0, ('b', Yellow)); (8, ('c', Grey)) ]
          qwerty_hash;
    "Test update_colors | non-empty list of repeated multi-element pairings \
     with a standardized qwerty_hash letter value"
    >:: update_colors_test
          [ ('a', Red); ('b', Red); ('c', Red) ]
          [ (56, ('a', Red)); (0, ('b', Red)); (8, ('c', Red)) ]
          qwerty_hash;
  ]

(* combine_list tests *)
let combine_list_tests =
  [
    "Test combine_list | combining an empty list with that of an empty string "
    >:: combine_list_test [] "" [];
    "Test combine_list | combining a list of one element with type int and a \
     string with its corresponding type"
    >:: combine_list_test [ ('5', 5) ] "5" [ 5 ];
    "Test combine_list | combining a list of one element with type string and \
     a string with its corresponding type"
    >:: combine_list_test [ ('h', "hello") ] "h" [ "hello" ];
    "Test combine_list | combining a list of one element with type int and a \
     string with its corresponding type"
    >:: combine_list_test [ ('2', []) ] "2" [ [] ];
    "Test combine_list | combining a list of one element with type float and a \
     string with its corresponding type"
    >:: combine_list_test [ ('8', 5.5) ] "8" [ 5.5 ];
  ]

let tests =
  List.flatten
    [
      is_valid_word_tests;
      is_correct_guess_tests;
      get_max_attempts_tests;
      read_file_tests;
      correct_letters_score_tests;
      char_list_to_string_tests;
      remove_duplicates_tests;
      overall_correct_letters_tests;
      map_yellow_red_chars_tests;
      map_chars_to_color_tests;
      map_green_chars_tests;
      sort_bindings_tests;
      word_list_tests;
      match_color_tests;
      select_random_word_tests;
      iter_tests;
      update_colors_tests;
      combine_list_tests;
    ]

let suite = "test suite" >::: tests
let () = run_test_tt_main suite
