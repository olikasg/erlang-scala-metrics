;; This file is part of RefactorErl.
;;
;; RefactorErl is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; RefactorErl is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with RefactorErl.  If not, see <http://plc.inf.elte.hu/erlang/>.
;;
;; The Original Code is RefactorErl.
;;
;; The Initial Developer of the Original Code is Eötvös Loránd University.
;; Portions created  by Eötvös Loránd University and ELTE-Soft Ltd.
;; are Copyright 2007-2013 Eötvös Loránd University, ELTE-Soft Ltd.
;; and Ericsson Hungary. All Rights Reserved.

(provide 'refactorerl-depanal)

(require 'cl)
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar levelvar "mod")
(defvar typevar "all")
(defvar otpvar :true)

(defvar gnodevar "")
(defvar exceptionvar "")
(defvar leavesvar "")
(defvar dotvar "")

;;
;;  FUN MOD WIDGET (FORM)
;;
(defun refactorerl-depanal-funmod ()
  "Creates a form for function and module dependency analysis"
  (interactive)
  (switch-to-buffer "*Function and module dependencies*")
  (kill-all-local-variables)
  (make-local-variable 'levelvar)
  (make-local-variable 'typevar)
  (make-local-variable 'otpvar)
  (make-local-variable 'gnodevar)
  (make-local-variable 'exceptionvar)
  (make-local-variable 'leavesvar)          
  (make-local-variable 'dotvar)   
       
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    ;; Delete all the overlays.
    (mapcar 'delete-overlay (car all))
    (mapcar 'delete-overlay (cdr all)))
  (widget-insert " * Function and module dependencies *\n\n")
  ;; Level
  (widget-insert " Level: \t ")
  (widget-create 'radio-button-choice
       :value "mod"
       :notify (var-setter-form levelvar)
       '(choice-item :format " Mod \t " "mod") '(choice-item :format " Func\n" "func"))
  (widget-insert "\n")
    ;; Type
  (widget-insert " Type:  \t ")
  (widget-create 'radio-button-choice
       :value "all"
       :notify (var-setter-form typevar)
       '(choice-item :format " All \t " "all") '(choice-item :format " Cycles\n" "cycles"))
  (widget-insert "\n")
    ;; Otp
  (widget-insert " Exclude Otp: \t ")
  (widget-create 'radio-button-choice
       :value :true
       :notify (var-setter-form otpvar)
       '(choice-item :format "True \t " :true) '(choice-item :format "False\n" :false))
  (widget-insert "\n\n")
    ;;
  (widget-insert " Node options\n\n  Nodes should be specified with graph nodes or with identifier\n"
                 "  - Modules can be specified with their names as atoms (e.g. 'mnesia'),\n" 
                 "  - Functions are specified by their MFA descriptor as a string\n" 
                 "    (e.g. \"io:format/2\")\n"
                 "  - Entities can be separated in the lists with commas: ,\n\n")
    ;; Gnode
    ;; List of entity or entities that should be the starting point of the analysis
  (widget-insert " List of entities that should be the starting point of the analysis\n ")
  (widget-create 'editable-field
     :size 13
     :value ""
     :notify (var-setter-form gnodevar)
     "")
  (widget-insert "\n\n")
    ;; Exception
    ;; List of entities excluded from the analysis
  (widget-insert " List of entities excluded from the analysis\n ")
  (widget-create 'editable-field
     :size 13
     :value ""
     :notify (var-setter-form exceptionvar)
     "")
  (widget-insert "\n\n")
    ;; Leaves
    ;; List of those entities which should be included in the analysis, but their children should not
  (widget-insert " List of those entities which should be included in the analysis,\n but their children should not\n ")
  (widget-create 'editable-field
     :size 13
     :value ""
     :notify (var-setter-form leavesvar)
     "")
  (widget-insert "\n\n")
    ;; Dot
    ;; The file path of the generated .dot graph description
  (widget-insert " The file name of the generated .dot graph description (without extension)\n ")
  (widget-insert " (Example: my_graph, Location: RefactorErl data directory)\n ")
  (widget-create 'editable-field
     :size 13
     :value ""
       :notify (var-setter-form dotvar))
  (widget-insert "\n\n")
    ;;
  (widget-create 'push-button
     :notify ;(lambda (&rest ignore)
     ;(message ;(prin1-to-string (list levelvar typevar otpvar gnodevar
               ;exceptionvar leavesvar dotvar))))
               (create-analiser-lambda (current-buffer)
                 (funmod-refac-call levelvar typevar otpvar 
                    (to-entity-list gnodevar)
                    (to-entity-list exceptionvar) 
                    (to-entity-list leavesvar) dotnamenoslashes))
     "Analise")
  (widget-insert " ")
  (widget-create 'push-button
     :notify (lambda (&rest ignore) 
         (kill-this-buffer))
     "Cancel")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup))
  

;;
;;  DEPANAL FBLOCK DEFAULT WIDGET (FORM)
;;
(defvar optionvar "draw")
(defvar fblistvar "")
(defvar othervar :true)

(defun refactorerl-depanal-fblock-default ()
  "Crates a form for function block dependency analysis (no regexps)"
  (interactive)
  (switch-to-buffer "* Function block dependency analysis *")
  (kill-all-local-variables)
  (make-local-variable 'optionvar)
  (make-local-variable 'fblistvar)
  (make-local-variable 'othervar)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    ;; Delete all the overlays.
    (mapcar 'delete-overlay (car all))
    (mapcar 'delete-overlay (cdr all)))
  (widget-insert " * Function block dependency analysis *\n\n")
  ;; Level
  (widget-insert " Options\n ")
  (widget-create 'radio-button-choice
       :value "draw"
       :notify (lambda (widget &rest ignore) 
           (setq optionvar (widget-value widget))
           (widget-setup))
       '(choice-item :format " Print out the entire graph or creates a\n subgraph drawing from the given function block list \n " "draw") 
       '(choice-item :format " Print out a subgraph which contains the\n cycle(s)\n\n" "draw_cycle"))
    ;; Dot
    ;; The file path of the generated .dot graph description
  (widget-insert " Chosen function block lists for further examinations\n ")
    (widget-insert " (Delimiter character is comma: , Example: fblock01,fblock02)\n ")
  (widget-create 'editable-field
     :size 13
     :value ""
       :notify (var-setter-form fblistvar)
     "")
  (widget-insert "\n\n")
    ;; Level
  (widget-insert " Other (take other category into consideration or not?)\n ")
  (widget-create 'radio-button-choice
       :value :true
       :notify (var-setter-form othervar)
       '(choice-item :format " True \t " :true) '(choice-item :format " False\n\n" :false))
    ;; Dot
    ;; The file path of the generated .dot graph description
  (widget-insert " The file name of the generated .dot graph description (without extension)\n ")
  (widget-insert " (Example: my_graph, Location: RefactorErl data directory)\n ")
  (widget-create 'editable-field
     :size 13
     :value ""
      :notify (var-setter-form dotvar)
     "")
  (widget-insert "\n\n")
  (widget-create 'push-button
     :notify (create-analiser-lambda (current-buffer)
              (fblock-refac-call optionvar fblistvar othervar dotnamenoslashes))
     "Analise")
  (widget-insert " ")
  (widget-create 'push-button
     :notify (lambda (&rest ignore) 
                (kill-this-buffer))
     "Cancel")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup))

;;
;;  DEPANAL FBLOCK WITH REGEXPS WIDGET (FORM)
;;
(defvar rgexpvar "")
(defvar typvar :draw)
(defvar rgexptypevar "file")
(defun refactorerl-depanal-fblock-regexp ()
  "Creates a form for function block analysis with regular expressions"
  (interactive)
  (switch-to-buffer "* Function block dependency analysis *")
  (kill-all-local-variables)
  (make-local-variable 'rgexpvar)
  (make-local-variable 'rgexptypevar)
  (make-local-variable 'typvar)
  (let 
      ((inhibit-read-only t))
      (erase-buffer))
  (let 
      ((all (overlay-lists)))
  ;; Delete all the overlays.
      (mapcar 'delete-overlay (car all))
      (mapcar 'delete-overlay (cdr all)))
  (widget-insert "*** Defining function blocks with regular expressions ***\n\n")
  (setf typvar :draw)
  (widget-insert "Regexp\n\n ")
  (widget-insert "Value type:\n ")
  (widget-create 'radio-button-choice
     :value "file"
     :notify (var-setter-form rgexptypevar)
     '(choice-item :format " A file (its filename) \n " "file") 
     '(choice-item :format " Regular expression list separated with commas \n\n " 
                                  "regexp"))
  (widget-insert "Value:\n ")
  (widget-create 'editable-field
   :size 13
   :value ""
     :notify (var-setter-form rgexpvar)
   "")
  (widget-insert "\n\n")
  (widget-insert " The file name of the generated .dot graph description (without extension)\n ")
  (widget-insert " (Example: my_graph, Location: RefactorErl data directory)\n ")
  (widget-create 'editable-field
   :size 13
   :value ""
     :notify (var-setter-form dotvar)
   "")
  (widget-insert "\n\n")
  (widget-create 'push-button
   :notify (create-analiser-lambda (current-buffer)
            (fblock-regexp-refac-call rgexpvar rgexptypevar typvar dotvar))
   "Analise")
  (widget-insert " ")
  (widget-create 'push-button
   :notify (lambda (&rest ignore) 
              (kill-this-buffer))
   "Cancel")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)
)

(defun fblock-refac-call (options fblist other dotfilename)
  "Calls the proper refactorerl function via the emacs server for fblock dep anal"
  (setq arglist (makearglist `( (:gnode ,(to-entity-list fblist)))))
  (cond 
    ((equal options "draw") 
        (depanal-refac-call-and-draw* 'fb_relations_draw arglist dotfilename))    
    ((equal options "draw_cycle") 
        (depanal-refac-call-and-draw* 'fb_relations_draw_cycles arglist dotfilename))                    
))

(defun fblock-refac-call* (referlfun fblist &rest opt)
  (lexical-let* 
    ((params 
        (if (equal fblist "") 
            nil 
            (split-string fblist)))
      (fun-and-args (append (list 'referlfun params) opt))
    (fun
      `(refac-send/callbacks ,fun-and-args
          (:reply (ok status-lines)
                    (depanal-window 'ok status-lines)))))
    (eval fun))    
)  

(defun funmod-refac-call (level type otp gnode exception leaves dotfilename)
    "Calls the proper refactorerl function via the emacs server for fun/mod dep anal"
  (message (prin1-to-string (list level type otp gnode exception leaves dotfilename)))
  (setq arglist
    (makearglist `( (:otp ,otp) (:gnode ,gnode) (:exception ,exception) (:leaves ,leaves))))
    (cond
      ((equal level "mod") 
          (if (equal type "cycles") 
              (depanal-refac-call-and-draw* 'cyclic_mod_draw_cycles arglist dotfilename) 
              (depanal-refac-call-and-draw* 'cyclic_mod_draw arglist dotfilename)))
      ((equal level "func") 
          (if (equal type "cycles") 
              (depanal-refac-call-and-draw* 'cyclic_fun_draw_cycles arglist dotfilename) 
              (depanal-refac-call-and-draw* 'cyclic_fun_draw arglist dotfilename)))
      (t 
          (message "some bug %s" level))
    )
)

(defun depanal-refac-call-and-draw* (referlfun arglist-no-dot dotfilename &rest rst)
  "Shows dependency analysis results, writes dot graph results to a file and converts to to pdf then opens it."
  (setq proper-dotfilename (make-proper-dot-name dotfilename))
  (refac-send/callbacks ('do_depanal_and_transfer referlfun arglist-no-dot proper-dotfilename)
      (:reply (ok (status-lines data))
                (progn
              	 (delete-other-windows) 
              	 (setf reswin (selected-window))
              	 (setf filewin (split-window-vertically))
              	 (select-window reswin)
              	 (switch-to-buffer "Dependency analysis result")
              	 (let 
              	    ((inhibit-read-only t))
              	   (erase-buffer)
              	   (set (make-local-variable 'last-file-dir) ""))
              	 (widget-insert "*** Dependency analysis result ***\n\n")
              	 (widget-insert (prin1-to-string status-lines))
              	 (widget-insert "\n")
              	 (use-local-map widget-keymap)
              	 (widget-setup)
              	 (sit-for 1)
              	 (let* 
              	    ((dotpath (concat (file-name-as-directory "~") proper-dotfilename))
              	     (cmd (concatenate 'string "dot -Tpdf " dotpath ".dot -o " dotpath ".pdf")))
              	   (progn
              	     (with-temp-file (concat dotpath ".dot")
              	       (insert data))
              	     (setq return (shell-command cmd))
              	     (if (or 
                          (equal 0 return) 
                          (equal "" return) 
                          (equal nil return))
              	       (progn 
              	         (select-window filewin) 
              	         (find-file (concatenate 'string dotpath ".pdf")))
              	       (message "An error occured during the generation of pdf with dot"))))))))
     
(defun fblock-regexp-refac-call (regexp regexptype type dot)
    "Makes an argument list and calls the proper function for regexp fblock depanal"
    (let* 
        ((value 
            (if (equal regexptype "regexp")
                 (to-entity-list regexp)  ;; regexps separated with ,
                regexp))            ;; a single file
        (arglist (makearglist `((:type ,type) (:regexp ,value)))))
        (progn
          (if (equal type :draw)
              (depanal-refac-call-and-draw* 'refusr_fb_regexp_re arglist dot "fb_relations")
              (refac-send/callbacks ('refusr_fb_regexp_re arglist) 
                                   (:reply (ok status-lines)
                                           (depanal-window 'ok status-lines)))))
    )
)

(defun makearglist (lst)
  "Makes an argument list:
  ( (1 2) (1 2 3) ) -> ( #(1 2) #(1 3) )"
  (interactive)
  (remove-if-not 
    (lambda (arg) 
        (not (equal arg nil)))
    (loop for elem in lst collect 
        (cond 
          ((equal 2 (length elem)) 
            (destructuring-bind (a b) elem (ifnotnil b (vector a b))))
          ((equal 3 (length elem)) 
            (destructuring-bind (a b c) elem (ifnotnil b (vector a c)))))
  ))
)

(defun ifnotnil (elem repl)
    "If elem is empty then replaces it with nil, else replaces it with repl"
    (interactive)
    (if (equal elem "")
      nil
      repl))

(defun depanal-window (status result)
  "Makes a window for dependency analysis TEXT resuls (nothing is drawn)"
  (delete-other-windows)
  (switch-to-buffer "DepAnal Result")
  (let 
    ((inhibit-read-only t))
    (erase-buffer)
    (set (make-local-variable 'last-file-dir) ""))
  (message "Analysis finished")
  (widget-insert "*** Dependency Analisys results ***\n\n")
  (widget-insert (prin1-to-string status-lines))
  (widget-insert "\n")
)

(defun make-proper-dot-name (dotfilename)
  "Gives dotfilename or a default value" 
  (if (equal dotfilename "")
      "emacs_dep_file"
      dotfilename))

(defun to-entity-list (string)
  "Splits a string and makes a list of function blocks / functions / modules"
  (setq splitted (split-string string "\\,"))
  (if (equal splitted (list ""))
      nil
      splitted))

(defun suitable-filenamep (dotname)
  "A filename is suitable if it is not empty"
  (not (equal dotname ""))
)

(defun rem-slashes (str)
  "Removes slashes from a string01."
  (apply 'concatenate
    (concatenate 'list 
      (list 'string)
      (mapcar
        (lambda (x) 
          (if (equal x ?\/) 
              ""
              (string x))) 
        str)))
)

(defmacro* create-analiser-lambda (buffer call)
  `(lambda (&rest ignore) 
      (setf buff ,buffer)
      (setf dotnamenoslashes (rem-slashes dotvar))
      (if (or 
              (suitable-filenamep dotnamenoslashes)
              (not (equal optionvar "draw")))
          (progn
              (message "Analysis in progress...")
              ,call
              (kill-buffer buff))
          (error "Please give a proper name for dot files!"))))

(defmacro* var-setter-form (var)
  `(lambda (widget &rest ignore) 
                (setq ,var (widget-value widget))
                (message (prin1-to-string ,var))
                (widget-setup)))