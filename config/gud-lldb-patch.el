;; ======================================================================
;; lldb functions

;; History of argument lists passed to lldb.
(defvar gud-lldb-history nil)

;; Keeps track of breakpoint created.  In the following case, the id is "1".
;; It is used to implement temporary breakpoint.
;; (lldb) b main.c:39
;; breakpoint set --file 'main.c' --line 39
;; Breakpoint created: 1: file ='main.c', line = 39, locations = 1
(defvar gud-breakpoint-id nil)

(defun lldb-extract-breakpoint-id (string)
  ;; Search for "Breakpoint created: \\([^:\n]*\\):" pattern.
  ;(message "gud-marker-acc string is: |%s|" string)
  (if (string-match "Breakpoint created: \\([^:\n]*\\):" string)
      (progn
        (setq gud-breakpoint-id (match-string 1 string))
        (message "breakpoint id: %s" gud-breakpoint-id)))
)

(defun gud-lldb-marker-filter (string)
  (setq gud-marker-acc
	(if gud-marker-acc (concat gud-marker-acc string) string))
  (lldb-extract-breakpoint-id gud-marker-acc)
  (let (start)
    ;; Process all complete markers in this chunk
    (while (or
            ;; (lldb) r
            ;; Process 15408 launched: '/Volumes/data/lldb/svn/trunk/test/conditional_break/a.out' (x86_64)
            ;; (lldb) Process 15408 stopped
            ;; * thread #1: tid = 0x2e03, 0x0000000100000de8 a.out`c + 7 at main.c:39, stop reason = breakpoint 1.1, queue = com.apple.main-thread
            (string-match " at \\([^:\n]*\\):\\([0-9]*\\), stop reason = .*\n"
                          gud-marker-acc start)
            ;; (lldb) frame select -r 1
            ;; frame #1: 0x0000000100000e09 a.out`main + 25 at main.c:44
            (string-match "^frame.* at \\([^:\n]*\\):\\([0-9]*\\)\n"
                           gud-marker-acc start))
      ;(message "gud-marker-acc matches our pattern....")
      (setq gud-last-frame
            (cons (match-string 1 gud-marker-acc)
                  (string-to-number (match-string 2 gud-marker-acc)))
            start (match-end 0)))

    ;; Search for the last incomplete line in this chunk
    (while (string-match "\n" gud-marker-acc start)
      (setq start (match-end 0)))

    ;; If we have an incomplete line, store it in gud-marker-acc.
    (setq gud-marker-acc (substring gud-marker-acc (or start 0))))
  string)

;; Keeps track of whether the Python lldb_oneshot_break function definition has
;; been exec'ed.
(defvar lldb-oneshot-break-defined nil)

;;;###autoload
(defun lldb (command-line)
  "Run lldb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive (list (gud-query-cmdline 'lldb)))

  (gud-common-init command-line nil 'gud-lldb-marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'lldb)
  (setq lldb-oneshot-break-defined nil)

  ;; Make lldb dump fullpath instead of basename for a file.
  ;; See also gud-lldb-marker-filter where gud-last-frame is grokked from lldb output.
  (progn
    (gud-call "settings set frame-format frame #${frame.index}: ${frame.pc}{ ${module.file.basename}{`${function.name}${function.pc-offset}}}{ at ${line.file.fullpath}:${line.number}}\\n")
    (sit-for 1)
    (gud-call "settings set thread-format thread #${thread.index}: tid = ${thread.id}{, ${frame.pc}}{ ${module.file.basename}{`${function.name}${function.pc-offset}}}{ at ${line.file.fullpath}:${line.number}}{, stop reason = ${thread.stop-reason}}\\n")
    (sit-for 1))

  (gud-def gud-listb  "breakpoint list"
                      "l"    "List all breakpoints.")
  (gud-def gud-bt     "thread backtrace"
                      "b"    "Show stack for the current thread.")
  (gud-def gud-bt-all "thread backtrace all"
                      "B"    "Show stacks for all the threads.")

  (gud-def gud-break  "breakpoint set -f %f -l %l"
                      "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak
	   (progn (gud-call "breakpoint set -f %f -l %l")
                  (sit-for 1)
                  (if (not lldb-oneshot-break-defined)
                      (progn
                        ;; The "\\n"'s are required to escape the newline chars
                        ;; passed to the lldb process.
                        (gud-call (concat "script exec \"def lldb_oneshot_break(frame, bp_loc):\\n"
                                                        "    target=frame.GetThread().GetProcess().GetTarget()\\n"
                                                        "    bp=bp_loc.GetBreakpoint()\\n"
                                                        "    print 'Deleting oneshot breakpoint:', bp\\n"
                                                        "    target.BreakpointDelete(bp.GetID())\""))
                        (sit-for 1)
                        ;; Set the flag since Python knows about the function def now.
                        (setq lldb-oneshot-break-defined t)))
                  (gud-call "breakpoint command add -p %b -o 'lldb_oneshot_break(frame, bp_loc)'"))
	              "\C-t" "Set temporary breakpoint at current line.")
  (gud-def gud-remove "breakpoint clear -f %f -l %l"
                      "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "thread step-in"
                      "\C-s" "Step one source line with display.")
  (gud-def gud-stepi  "thread step-inst"
                      "\C-i" "Step one instruction with display.")
  (gud-def gud-next   "thread step-over"
                      "\C-n" "Step one line (skip functions).")
  (gud-def gud-nexti  "thread step-inst-over"
                      nil    "Step one instruction (skip functions).")
  (gud-def gud-cont   "process continue"
                      "\C-r" "Continue with display.")
  (gud-def gud-finish "thread step-out"
                      "\C-f" "Finish executing current function.")
  (gud-def gud-up
           (progn (gud-call "frame select -r 1")
                  (sit-for 1))
                      "<"    "Up 1 stack frame.")
  (gud-def gud-down
           (progn (gud-call "frame select -r -1")
                  (sit-for 1))
                      ">"    "Down 1 stack frame.")
  (gud-def gud-print  "expression -- %e"
                      "\C-p" "Evaluate C expression at point.")
  (gud-def gud-pstar  "expression -- *%e"
                      nil    "Evaluate C dereferenced pointer expression at point.")
  (gud-def gud-run    "run"
                      "r"    "Run the program.")
  (gud-def gud-stop-subjob    "process kill"
                      "s"    "Stop the program.")

  (setq comint-prompt-regexp  "\\(^\\|\n\\)\\*")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'lldb-mode-hook)
  )
