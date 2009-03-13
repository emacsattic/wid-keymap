;;; wid-keymap.el --- display keymaps using widgets

;; Copyright (C) 2008, 2009 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Updated: 20090313
;; Version: 0.0.2
;; Homepage: https://github.com/tarsius/wid-keymap
;; Keywords: extensions, keymaps

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
    (dolist (layout keyboard-layout)
      (push (widget-create-child-and-convert
	     widget 'keyboard
	     :keyboard layout
	     :value (nthcdr 3 value))
	    children)
      (insert "\n"))
    (push (widget-create-child-and-convert
	   widget 'event-binding-list
	   :format "%v%i\n"
	   :value (nthcdr 3 value)
	   ;; Exclude all events displayed above.
	   :value-exclude (mapcan #'widget-value
				  (butlast children 3)))
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
    (apply #'nconc (list (widget-value (nth 0 children))
			 (widget-value (nth 1 children))
			 (widget-value (nth 2 children)))
	   (mapcar #'widget-value (nthcdr 3 children)))))

;;; The `keyboard' Widget.

(define-widget 'keyboard 'table
  "A keyboard.  (A table of key bindings)."
  :convert-table 'widget-keyboard-convert
  :cell-create 'widget-keyboard-cell-create
  :value-get 'widget-keyboard-value-get
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
      (let* ((entry (car (member* (key-description key) value
				  :test #'equal
				  :key (lambda (elt)
					 (key-description (car elt))))))
	     (child (widget-create-child-and-convert
		     widget 'keyboard-key
		     :position (cons row col)
		     :event key
		     :value (if entry (cdr entry) 'unbound))))
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

;;; The `keyboard-key' Widget.

(defvar widget-keyboard-force-cmds nil)

(define-widget 'keyboard-key 'sexp-cell
  "A key on a keyboard."
  :value-to-internal 'widget-keyboard-key-value-to-internal
  :value-to-external 'widget-keyboard-key-value-to-external)

(defun widget-keyboard-key-value-to-internal (widget value)
  (case value
    (unbound "")
    (self-insert-command (key-description (widget-get widget :event)))
    (t (prin1-to-string value))))

(defun widget-keyboard-key-value-to-external (widget value)
  (cond ((equal value "") 'unbound)
	((equal value (key-description (widget-get widget :event)))
	 'self-insert-command)
	(t
	 (let ((cmd (read value)))
	   (when (and (= 1 (length value))
		      (not (commandp cmd))
		      (not (memq cmd widget-keyboard-force-cmds)))
	     (lwarn '(wid-keymap cus-keymap) :warning "\
Are you sure you want to bind \"%s\" to `%s'?

No such command exists, or the feature that provides it is not loaded.

When customizing keymaps whenever a key is bound to `self-insert-command'
the character that would be inserted is displayed instead of the that
command.

However this has the potential of the following problem:

You intend to bind \"M\" to `self-insert-command' but instead of entering
that command (which is the save thing to do) or \"M\" you actually enter
\"m\".

In order not to see this warning for `%s' again add it to
`widget-keyboard-force-cmds'."
		    event value value))
	     cmd)))))

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

(defvar keyboard-layout nil)

(provide 'wid-keymap)
;;; wid-keymap.el ends here
