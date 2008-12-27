;;; wid-keymap.el --- display keymaps using widgets

;; Copyright (C) 2008 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoulli.cc>
;; Created: 20080830
;; Updated: 20080830
;; Version: 0.0.1
;; Homepage: http://artavatar.net
;; Keywords: extensions

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'wid-table)
(require 'keymap-utils)

;;; The `keymap' Widget.

(define-widget 'keymap 'default
  "A keymap."
  :format "%{%t%}:\n%v"
  :value-create 'widget-keymap-value-create
  :value-to-internal 'widget-keymap-value-to-internal
  :value-to-external 'widget-keymap-value-to-external
  :value-get 'widget-keymap-value-get
  :match (lambda (widget value)
	   (keymapp value)))

(defun widget-keymap-value-create (widget)
  (let* ((children (widget-get widget :children))
	 (value (widget-get widget :value)))
    (push (widget-create-child-and-convert
	   widget 'menu-choice
	   :tag "Type"
	   :format "%{%t%}:    %[>%] %v"
	   :value (nth 0 value)
	   '(const :tag "full" t)
	   '(const :tag "sparse" nil))
	  children)
    (push (widget-create-child-and-convert
	   widget 'menu-choice
	   :tag "Prompt"
	   :format "%{%t%}:  %[>%] %v"
	   :value (nth 1 value)
	   '(const :tag "none"
		   :menu-tag "None"
		   nil)
	   '(string))
	  children)
    (push (widget-create-child-and-convert
	   widget 'menu-choice
	   :tag "Parent"
	   :format "%{%t%}:  %[>%] %v"
	   :value (nth 2 value)
	   '(const :tag "none"
		   :menu-tag "None"
		   nil)
	   '(symbol :format "%{%t%}: %v")
	   '(sexp :tag "Annonymous"
		  :menu-tag "Annonymous"))
	  children)
    (insert "\n")
    (push (widget-create-child-and-convert
	   widget 'keyboard
	   :value value)
	  children)
    (insert "\n")
    (push (widget-create-child-and-convert
	   widget 'event-binding-list
	   :format "%v%i\n"
	   :value (nthcdr 3 value)
	   :value-exclude (widget-value (car children)))
	  children)
    (widget-put widget :children (nreverse children))))

(defun widget-keymap-value-to-internal (widget value)
  (setq value (copy-keymap value))
  (nconc (list (kmu-full-keymap-p value)
	       (keymap-prompt value)
	       (kmu-keymap-parent value))
	 (kmu-keymap-event-bindings value)))

(defun widget-keymap-value-to-external (widget value)
  (let ((keymap (funcall (if (nth 0 value)
			     'make-keymap
			   'make-sparse-keymap)
			 (nth 1 value)))
 	(parent (nth 2 value)))
    (when parent
      ;; If parent is a symbol store that.
      (nconc keymap (list parent)))
    (mapc (lambda (binding)
	    (define-key keymap (car binding) (cdr binding)))
	  (nthcdr 3 value))
    keymap))

(defun widget-keymap-value-get (widget)
  (let ((children (widget-get widget :children)))
    (nconc (list (widget-value (nth 0 children))
		 (widget-value (nth 1 children))
		 (widget-value (nth 2 children)))
	   (widget-value (nth 3 children))
	   (widget-value (nth 4 children)))))

;;; The `keyboard' Widget.

(define-widget 'keyboard 'table
  "A keyboard.  (A table of key bindings)."
  :convert-table 'widget-keyboard-convert
  :cell-create 'widget-keyboard-cell-create
  :value-get 'widget-keyboard-value-get
  :keyboard 'keyboard-layout
  :column-size-default 27
  :column-size-minimum 10
  :column-size-maximum 30)

(defun widget-keyboard-convert (widget)
  (let* ((keyboard (widget-get-indirect widget :keyboard))
	 (keydepth (length (aref (aref keyboard 0) 0)))
	 (rows (* (length keyboard) keydepth))
	 (cols (length (aref keyboard 0))))
    (widget-put widget :keydepth keydepth)
    (widget-put widget :rows rows)
    (widget-put widget :columns cols)
    (widget-put widget :cells (make-vector rows nil))
    (widget-put widget :column-size (make-vector cols nil))
    (widget-put widget :column-hidden (make-vector cols nil))
    (widget-put widget :id (incf widget-tables))))

(defun widget-keyboard-cell-create (widget row col)
  (let* ((children (widget-get widget :children))
	 (keyboard (widget-get-indirect widget :keyboard))
	 (keydepth (widget-get widget :keydepth))
	 (value (widget-get widget :value))
	 (key (aref (aref (aref keyboard (/ row keydepth)) col)
		    (% row keydepth))))
    (if (stringp key)
	(widget-create-child-and-convert
	 widget 'item-cell
	 :position (cons row col)
	 :value-face 'widget-push-cell
	 :value key)
      (let* ((entry (car (member* (key-description key)
				  (nthcdr 3 value)
				  :test #'equal
				  :key (lambda (elt)
					 (key-description (car elt))))))
	     (child (widget-create-child-and-convert
		     widget 'sexp-cell
		     :position (cons row col)
		     :event key
		     :value (if entry (cdr entry) 'unbound)
		     :value-to-internal
		     (lambda (widget value)
		       (if (eq value 'unbound) ""
			 (prin1-to-string value)))
		     :value-to-external
		     (lambda (widget value)
		       (if (equal value "") 'unbound
			 (read value))))))
	(widget-put widget :children
		    (cons child (widget-get widget :children)))
	child))))

(defun widget-keyboard-value-get (widget)
  (mapcan (lambda (child)
	    (let ((event (widget-get child :event))
		  (value (widget-value child)))
	      (unless (eq value 'unbound)
		(list (cons event value)))))
	  (widget-get widget :children)))

;;; The `event-binding-list' Widget.

(define-widget 'event-binding-list 'editable-list
  "A list of event bindings."
  :value-to-internal 'widget-event-binding-list-value-to-internal
  :value-get 'widget-event-binding-list-value-get
  :args '((cons :format "%v"
		(sexp :tag "Event"
		      :size 25
		      :format "%{%t%}: %v "
		      :value-to-internal
		      (lambda (widget value)
			(key-description value))
		      :value-to-external
		      (lambda (widget value)
			(read-kbd-macro value t)))
		(sexp :tag "Definition"))))

(defun widget-event-binding-list-value-to-internal (widget value)
  (let ((exclude (widget-get widget :value-exclude)))
    (mapcan (lambda (binding)
	      (unless (member* (key-description (car binding))
			       exclude
			       :test #'equal
			       :key (lambda (elt)
				      (key-description (car elt))))
		(list binding)))
	    value)))

(defun widget-event-binding-list-value-get (widget)
  (mapcan (lambda (child)
	    (let ((value (widget-value child)))
	      (unless (eq (cdr value) 'unbound)
		(list value))))
	  (widget-get widget :children)))

(defun widget-event-binding-value-get (widget)
  (let ((children (widget-get widget :children)))
    (cons (if (= (length children) 1)
	      (car (widget-get widget :value))
	    (widget-value (car children)))
	  (widget-value (car (last children))))))

(defun widget-event-binding-match (widget value)
  (and (listp value)
       (arrayp (car value))))

;;; Keyboard Layouts.

(defvar keyboard-layout
  [[["1"     [?1]  [f1]            ""    [(control ?1)]  [(meta ?1)]]
    ["2"     [?2]  [f2]            ""    [(control ?2)]  [(meta ?2)]]
    ["3"     [?3]  [f3]            ""    [(control ?3)]  [(meta ?3)]]
    ["4"     [?4]  [f4]            ""    [(control ?4)]  [(meta ?4)]]
    ["5"     [?5]  [f5]            ""    [(control ?5)]  [(meta ?5)]]
    ["6"     [?6]  [f6]            ""    [(control ?6)]  [(meta ?6)]]
    ["7"     [?7]  [f7]            ""    [(control ?7)]  [(meta ?7)]]
    ["8"     [?8]  [f8]            ""    [(control ?8)]  [(meta ?8)]]
    ["9"     [?9]  [f9]            ""    [(control ?9)]  [(meta ?9)]]
    ["0"     [?0]  [f10]           ""    [(control ?0)]  [(meta ?0)]]]
   [["Q"     [?q]  [?Q]            [?^]  [(control ?q)]  [(meta ?q)]]
    ["W"     [?w]  [?W]            [?&]  [(control ?w)]  [(meta ?w)]]
    ["E"     [?e]  [?E]            [?*]  [(control ?e)]  [(meta ?e)]]
    ["R"     [?r]  [?R]            [?+]  [(control ?r)]  [(meta ?r)]]
    ["T"     [?t]  [?T]            [?ü]  [(control ?t)]  [(meta ?t)]]
    ["Y"     [?y]  [?Y]            [?Ü]  [(control ?y)]  [(meta ?y)]]
    ["U"     [?u]  [?U]            [?']  [home]          [(control home)]]
    ["I"     [?i]  [?I]            [?\\] [f13]           [(meta ?i)]]
    ["O"     [?o]  [?O]            [??]  [(control ?o)]  [(meta ?o)]]
    ["P"     [?p]  [?P]            [?€]  [end]           [(control end)]]]
   [["A"     [?a]  [?A]            [?~]  [(control ?a)]  [(meta ?a)]]
    ["S"     [?s]  [?S]            [?$]  [(control ?s)]  [(meta ?s)]]
    ["D"     [?d]  [?D]            [?_]  [(control ?d)]  [(meta ?d)]]
    ["F"     [?f]  [?F]            [?-]  [(control ?f)]  [(meta ?f)]]
    ["G"     [?g]  [?G]            [?ä]  [(control ?g)]  [(meta ?g)]]
    ["H"     [?h]  [?H]            [?Ä]  [(control ?h)]  [(meta ?h)]]
    ["J"     [?j]  [?J]            [?\"] [left]          [(control left)]]
    ["K"     [?k]  [?K]            [?/]  [up]            [(control up)]]
    ["L"     [?l]  [?L]            [?!]  [down]          [(control down)]]
    ["space" [?\s] [(shift ?\s)]   ""    [right]         [(control right)]]]
   [["Z"     [?z]  [?Z]            [?@]  [(control ?z)]  [(meta ?z)]]
    ["X"     [?x]  [?X]            [?#]  [(control ?x)]  [(meta ?x)]]
    ["C"     [?c]  [?C]            [?%]  [(control ?c)]  [(meta ?c)]]
    ["V"     [?v]  [?V]            [?=]  [(control ?v)]  [(meta ?v)]]
    ["B"     [?b]  [?B]            [?ö]  [(control ?b)]  [(meta ?b)]]
    ["N"     [?n]  [?N]            [?Ö]  [(control ?n)]  [(meta ?n)]]
    ["M"     [?m]  [?M]            [?`]  [backspace]     [(control backspace)]]
    [","     [?\,] [?\;]           [?|]  [f17]           [f18]]
    ["."     [?\.] [?\:]           ""    [f19]           [f20]]
    ["tab"   [tab] [(shift tab)]   ""    [delete]        [(control delete)]]]
   [["["     [?\[] ""              ""    ""              [(meta ?\[)]]
    ["{"     [?\{] ""              ""    [(control ?\{)] [(meta ?\{)]]
    ["}"     [?\}] ""              ""    [(control ?\})] [(meta ?\})]]
    ["]"     [?\]] ""              ""    [(control ?\])] [(meta ?\])]]
    [""      ""    ""              ""    ""              ""]
    [""      ""    ""              ""    ""              ""]
    ["("     [?\(] ""              ""    [(control ?\()] [(meta ?\()]]
    ["<"     [?\<] ""              ""    [(control ?\<)] [(meta ?\<)]]
    [">"     [?\>] ""              ""    [(control ?\>)] [(meta ?\>)]]
    [")"     [?\)] ""              ""    [(control ?\))] [(meta ?\))]]]])

(provide 'wid-keymap)
;;; wid-keymap.el ends here
