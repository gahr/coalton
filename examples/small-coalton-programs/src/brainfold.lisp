;;;;
;;;; A Brainfold/Brainf*** interpreter implemented in Coalton
;;;;
;;;; This interpreter handles all standard bf commands:
;;;; ( > < + - . , [ ] ).
;;;;
;;;;
;;;; Run Brainfold programs with (run-program "+++++[-]+++")
;;;;
;;;; or
;;;;
;;;; (run-file "/path/to/your/file.bf")
;;;;
;;;; try (coalton (hello-world)) in the REPL!
;;;;

(cl:defpackage #:brainfold
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:iter #:coalton-library/iterator)
   (#:cell #:coalton-library/cell)
   (#:char #:coalton-library/char)
   (#:str #:coalton-library/string)
   (#:list #:coalton-library/list)
   (#:arith #:coalton-library/math))

  (:export
   #:eval
   #:run-program
   #:run-file

   ;; Examples
   #:hello-world
   #:gnarly-hello-world
   #:squares))

(in-package #:brainfold)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; State/Env
;;;

(coalton-toplevel

  (define-type State
    (State (Vector Integer) (Cell UFix) (Cell String))) ; Memory, Pointer, Print-buffer

  (define (state-memory (State memory _ _))
    "Accessor for the State memory."
    memory)
  
  (define (state-pointer (State _ pointer _))
    "Accessor for the State pointer."
    pointer)

  (define (state-print-buffer (State _ _ buffer))
    "Accessor for the State buffer."
    buffer)

  ;;
  ;; Generating a Brainfold memory vector
  ;;

  (declare bf-vector-size UFix)
  (define bf-vector-size 1000)
  
  (define (generate-bf-vector)
    "Initializes the brainfold array."
    (let v = (vec:new))
    (vec:extend!  v (iter:repeat-for 0 bf-vector-size))
    v)

  ;;
  ;; Generating a default State:
  ;;

  (define (new-state)
    "Generates a new, blank Brainfold State."
    (State (generate-bf-vector) (cell:new 0) (cell:new "")))

  ;;
  ;; Accessing the current value
  ;;

  (declare value-at-pointer (State -> Integer))
  (define (value-at-pointer (State memory pointer _))
    "Returns the value at the current pointer."
    (unwrap (vec:index (cell:read pointer)
		       memory))))


;;;
;;; Commands (Functions called by Brainfold Cmds)
;;;

(coalton-toplevel

  ;;
  ;; Navigating through bf-cells (> <)
  ;;
  
  (declare move-right (State -> State))
  (define (move-right s)
    "Moves the pointer one bf-cell to the right."
    (cell:increment! (state-pointer s))
    s)

  (declare move-left (State -> State))
  (define (move-left s)
    "Moves the pointer one bf-cell to the left."
    (cell:decrement! (state-pointer s))
    s)

  ;;
  ;; Changing bf-cell values (+ -)
  ;;

  (declare incr (State -> State))
  (define (incr s)
    "Increments the value for the current bf-cell."
    (vec:set! (cell:read (state-pointer s))
              (1+ (value-at-pointer s))
              (state-memory s))
    s)

  (declare decr (State -> State))
  (define (decr s)
    "Decrements the value for the current bf-cell."
    (vec:set! (cell:read (state-pointer s))
              (1- (value-at-pointer s))
              (state-memory s))
    s)

  ;;
  ;; Printing Cells (.)
  ;;
  
  (declare find-ascii (UFix -> String))
  (define (find-ascii ascii-value)
    "Converts a UFix into an ASCII string."
    (into (make-list (unwrap (char:code-char ascii-value)))))

  (declare print (State -> State))
  (define (print s)
    "Prints the current bf-cell."
    (cell:write! (state-print-buffer s)
                 (str:concat
                  (cell:read (state-print-buffer s))
                  (find-ascii
                   (fromint (abs (value-at-pointer s))))))
    s)

  ;;
  ;; Taking Input (,)
  ;; 
  ;; Currently takes individual characters one at a time as prompted

  (define (character-prompt)
    "A prompt for obtaining one character as input."
    (Lisp Char ()
      (cl:format cl:*query-io* "Input a character: ")
      (cl:finish-output cl:*query-io*)
      (cl:read-char cl:*query-io*)))

  (declare take-input (State -> State))
  (define (take-input s)
    "Takes and stores a character at the input-index as an ascii code at
the pointer."
    (vec:set! (cell:read (state-pointer s))
              (into (char:char-code (character-prompt)))
              (state-memory s))
    s))


;;;
;;; Parsing/Lexing
;;;

(coalton-toplevel

  (define-type Cmd
    BFRight
    BFLeft
    BFPlus
    BFMinus
    BFPrint
    BFInput
    (BFLoop (Vector Cmd)))

  (declare parse (String -> (Vector Cmd)))
  (define (parse input-string)
    "Parses a Brainfold instruction string, returns a Vector of Brainfold Commands."
    (let cmds = (vec:new))
    (let vecs = (vec:new))
    (let ((parser (fn (input-string v)
                    (let ((head-tail (str:split 1 input-string)))
                      (match (fst head-tail)
                        ("" cmds)
                        (">"
                         (vec:push! BFRight v)
                         (parser (snd head-tail) v))
                        ("<"
                         (vec:push! BFLeft v)
                         (parser (snd head-tail) v))
                        ("+"
                         (vec:push! BFPlus v)
                         (parser (snd head-tail) v))
                        ("-"
                         (vec:push! BFMinus v)
                         (parser (snd head-tail) v))
                        ("."
                         (vec:push! BFPrint v)
                         (parser (snd head-tail) v))
                        (","
                         (vec:push! BFInput v)
                         (parser (snd head-tail) v))
                        ("["
                         (vec:push! v vecs)
                         (parser (snd head-tail) (vec:new)))
                        ("]"
                         (vec:push! (BFLoop v) (unwrap (vec:last vecs)))
                         (parser (snd head-tail) (unwrap (vec:pop! vecs))))
                        (_ (parser (snd head-tail) v)))))))
      (parser input-string cmds))))


;;;
;;; Evaluation
;;;

(coalton-toplevel
  
  (declare exec (Cmd -> State -> State))
  (define (exec cmd s)
    "Executes a given bf command string."
    (match cmd
      ((BFRight) (move-right s))
      ((BFLeft) (move-left s))
      ((BFPlus) (incr s))
      ((BFMinus) (decr s))
      ((BFPrint) (print s))
      ((BFInput) (take-input s))
      ((BFLoop v) (exec-loop v s))))

  (declare exec-cmds ((Vector Cmd) -> State -> State))
  (define (exec-cmds cmds s)
    (let l = (1- (vec:length cmds)))
    (let ((f (fn (index s)
                (if (== l index)
                    (exec (unwrap (vec:index index cmds)) s)
                    (f (1+ index) (exec (unwrap (vec:index index cmds)) s))))))
      (f 0 s)))

  (declare exec-loop ((Vector Cmd) -> State -> State))
  (define (exec-loop cmds s)
    "Loops commands until the value at the pointer is 0."
    (match (value-at-pointer s)
      (0 s)
      (_ (exec-loop cmds (exec-cmds cmds s)))))

  (define (eval input-string s)
    "Parses and evaluates a string of brainfold input."
    (exec-cmds (parse input-string) s)))


;;;
;;; Top Level
;;;

(coalton-toplevel

  (declare run-program (String -> String))
  (define (run-program bf-string)
    "Takes a bf-command string and evaluates and executes its components
after restarting the environment."
    (cell:read (state-print-buffer (eval bf-string (new-state)))))

  (define (run-file filepath)
    "Loads and executes the brainfold file at the given filepath."
    (run-program (Lisp String (filepath)
                   (uiop:read-file-string filepath)))))


;;;
;;; Sample test programs
;;;

(coalton-toplevel

  ;; from https://esolangs.org/wiki/Brainfuck
  
  (define (hello-world)
    (run-program
     "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+."))

  (define (gnarly-hello-world)
    (run-program
     ">++++++++[-<+++++++++>]<.>>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.>->+++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+."))

  ;; from https://github.com/saulpw/brainfuck/tree/master/tests

  (define (squares)
    (run-program "++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+>>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]<<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]")))
