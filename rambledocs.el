;; rambledocs.el --- Code to assist on-the-fly documentation

;; Copyright (C) 1999,2000 by Tom Breton

;; Author: Tom Breton;; <Tehom@localhost>
;; Keywords: docs, lisp, local
;; Version: 1.3

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To use this package, you need:

;; psgml (http://www.lysator.liu.se/projects/about_psgml.html)
;; the rambledocs dtd (bundled with this distribution)
;; the outline dtd (bundled with this distribution)
;; tehom-psgml, my psgml extensions.
;; emacs (Of course)
;; imenu (Bundled with emacs 19+)

;; Rationale:

;; This package is meant to help organize on-the-fly ramblings that
;; occur in the course of contructing code.  

;; Writing documentation seems to be the bane of most programmers.  Of
;; course when you're actually writing code, you know why you're doing
;; what you're doing.  But often it never gets written down, or only
;; gets jotted down later according to memory.

;; I find that I tend to write the most and best commentary when I'm
;; actually in the midst of constructing source code.  Basically I'm
;; face to face with whatever needs work or doesn't work, at which
;; time it's easy to say what the trouble is and what needs to be done
;; about it.

;; The problem with this approach is that it tends to leave commentary
;; lying haphazardly around, spread between comments in the source and
;; half-hearted attempts to organize the commentary into some sort of
;; coherency in other files.

;; The idea of the rambledocs package is that you can instantly jump
;; between your source code and a dedicated, structured doc file, so
;; you can document your thinking and your plans in the context of
;; your actual coding work.

;; It's not meant as a substitute for flowing commentary that explains
;; individual lines of code, which properly belongs in the source code
;; file.  It is appropriate for a hyer level of documentation, to
;; explain overall design, rationale, musings, pitfalls, and future
;; potentials and plans.

;; It's not a substitute for changelogs either, but there is some
;; overlap.

;; I deliberately made the rambledocs dtd quite freeform, because IMO for
;; this purpose it's better to err on the side of underconstraining
;; the programmer than overconstraining.

;; Limitations:

;; For the time being, rambledocs only supports elisp.  It could be made
;; to support other languages as well by rewriting just the elisp
;; section and changing a few explicit calls into funcall's on
;; suitable variables.

;; Praise:

;; imenu was extremely easy to borrow from. I commend Ake and Lars for
;; the cleanness of their code.

;;;;

;;; Code:

(require 'local-vars)
(require 'cl)
(require 'psgml)       ;;For managing the doc file.
;(require 'psgml-edit)  ;;Part of psgml, needed for rewritten stuff.
(require 'tehom-psgml)
(require 'imenu)  ;;For scanning the elisp source file.
(require 'tehom-4);;For tehom-completing-read-assoc
;(require 'rtest)


;;;;;;;;;;;;;;;;;;;
;;Customization

(defgroup rambledocs nil
  "Organize on-the-fly commentary."
  :group 'tools)

(defcustom rmb-doc-dtd-local-variables 
  nil
  "*Local variables which will be automatically inserted into the doc
file by rmb-insert-doc-dtd." 
  :tag "Local variables for all rambledocs doc files"
  :type 
  '(repeat 
     sexp)
  :group 'rambledocs)

(defcustom rmb-dtd-filename 
  "~/dtd/rambledocs-3.dtd"
  "*Path to the file where the rambledocs dtd lives"
  :tag "Where the rambledocs dtd lives."
  :type '(file :must-match t)
  :group 'rambledocs)




;;;;;;;;;;;;;;;;
;;Constants

(defconst rmb-dtd-name "Rambledocs" 
  "The name of the top-level node in the rambledocs dtd")

(defconst rmb-dtd-component-element-name "Component" 
  "The name of the node in the rambledocs dtd specific to a function")

(defconst rmb-dtd-group-element-name "Group" 
  "The name of the intermediate node in the rambledocs dtd")

(defconst rmb-skip-imenu-name-list 
  '("*Rescan*" "Variables" "Types" "Syntax-unknown") 
  "Names to skip in imenu's return value." )


(defconst rmb-docfile-extension ".rmb.xml" 
  "The extension added to source files to indicate related doc files." )

;;;;;;;;;;;;;;;;
;;Buffer-local variables

(defvar rmb-docfile-name nil
  "The name of the doc file associated with this source code buffer" )
(make-variable-buffer-local 'rmb-docfile-name)


(defvar rmb-srcfile-name nil
  "The name of the source code file associated with this doc buffer" )
(make-variable-buffer-local 'rmb-srcfile-name)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Functions to get related filename

(defun rmb-get-docfile-name (&optional buffer)
  "Get the doc file name associated with a buffer visiting a source file."

  (rmb-must-not-be-in-docfile)

  (or
    ;;If rmb-docfile-name is defined, use it.
    rmb-docfile-name

    ;;Otherwise transform this buffer's name.
    (using-local-vars
      (local-var base-name (buffer-file-name))
      (if (not base-name) (error "This buffer isn't visiting any file" ))
      (local-var 
	docfile-name
	(concat base-name rmb-docfile-extension))
    
      docfile-name)))


(defun rmb-get-srcfile-name (&optional buffer)
  "Get the source file name associated with a buffer visiting a doc file."

  (rmb-must-be-in-docfile)

  ;;If rmb-srcfile-name is defined, use it.
  (or
    rmb-srcfile-name

    ;;Otherwise transform this buffer's name.
    (using-local-vars
      (local-var base-name (buffer-file-name))
      (if 
	(not base-name) 
	(error "This buffer isn't visiting any file"))
    
      ;;Strip off the additions we make in generating docfile name.
      ;;Note that the regexp needs "\\" to generate a regexp magic
      ;;backslash and "\\\\" to generate a normal, non-magic backslash. 
      (local-var 
	srcfile-name
	(progn
	  (string-match 
	    (concat "\\(.*\\)" rmb-docfile-extension)
	    base-name)
	  (match-string 1 base-name)))
    
      srcfile-name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Functions to visit the related files.


(defun rmb-set-up-doc-buffer ()
  "Change the current buffer into a docfile buffer if it isn't already
one. 

Careful: Only call this when the current buffer should become a
docfile buffer or already is one."

  ;;If we're not already in one, insert the appropriate dtd, which
  ;;should be sufficient to make it a rambledocs buffer.
  (if (not (rmb-in-rambledocs-buffer-p) )
    (progn
      (rmb-insert-doc-dtd)
      (xml-mode)))
  

  ;;Now we surely have the rambledocs dtd.
  (assert (rmb-in-rambledocs-buffer-p))

  ;;If we don't have a top element, make an appropriate one.
  (sgml-need-dtd)
  (if (not (sgml-top-element))
    (progn
      ;;It's less risky to do this at the end of buffer, despite the
      ;;possible presence of local variables.  Searching backwards
      ;;so we can insert before local variables would be nice.
      (goto-char (point-max))
      (tehom-psgml-insert-element rmb-dtd-name t t '() )))

  ;;Now we surely a top element.
  (assert (sgml-top-element))

  t)


;;Utility
(defun rmb-file-finder-func (arg)
  ""
  (if arg #'find-file-other-window #'find-file))

(defun rmb-find-docfile-somehow (arg)
  ""
  
  (rmb-must-not-be-in-docfile)
  
  (let*
    (
      (docfile-name (rmb-get-docfile-name))
      (find-file-not-found-hooks find-file-not-found-hooks)
      (file-finder-func (rmb-file-finder-func arg)))
    
    (add-hook 
      'find-file-not-found-hooks
      'rmb-set-up-doc-buffer)
    
    (funcall file-finder-func docfile-name)
    (rmb-doc-minor-mode t)))


;;;###autoload
(defun rmb-find-docfile ()
  "Find the docfile associated with this source file."
  (interactive)

  (rmb-find-docfile-somehow nil))


;;;###autoload
(defun rmb-find-docfile-other-window ()
  "Find the docfile associated with this source file, in another window." 
  (interactive)

  (rmb-find-docfile-somehow t))

(defun rmb-find-srcfile-somehow (arg)
  "Find the srcfile associated with this doc file."
  (interactive)

  (rmb-must-be-in-docfile)

  (let*
    (
      (srcfile-name (rmb-get-srcfile-name))
      (file-finder-func (rmb-file-finder-func arg)))
    
    (funcall file-finder-func srcfile-name)
    (rmb-src-minor-mode t)))


;;;###autoload
(defun rmb-find-srcfile ()
  "Find the srcfile associated with this doc file."
  (interactive)

  (rmb-find-srcfile-somehow nil))


;;;###autoload
(defun rmb-find-srcfile-other-window ()
  "Find the srcfile associated with this source file, in another window." 
  (interactive)

  (rmb-find-srcfile-somehow t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Elisp-grasping functions


(defun rmb-src-get-function-name ()
  "Find the name of the function that we'd look in the docs for."
  
  (if (not (rmb-in-rambledocs-buffer-p))
    (save-excursion
      (using-local-vars

	;;Find a defun we're in or after.
	(beginning-of-defun 1)

	;;Enter it
	(down-list 1)

	;;Skip the word "defun"
	(forward-sexp 1)

	;;Read the name.
	(local-var ob (read (current-buffer)))

	;;Turn the name into a string.
	(local-var function-name (format "%S" ob))

	function-name))))



(defun rmb-go-to-src-element (name)
  ""
  
  (if (not (rmb-in-rambledocs-buffer-p))
    (cond

      ;;Make an error on cases we can't handle yet.
      ((member name rmb-skip-imenu-name-list )
	(error "Can't use %s as a target" name))

      (t
	(let*
	  (
	    (alist 
	      (rmb-imenu--make-index-alist))
	    
	    (cell
	      (assoc name alist)))
      
	  (if
	    cell
	    (goto-char (cdr cell)))
    
	  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PSGML-grasping functions


;;;###autoload
(defun rmb-in-rambledocs-buffer-p ()
  "Return t if we're in a rambledocs buffer, otherwise nil."

  (if
    ;;The buffer has to be in the proper major mode.
    (eq major-mode 'xml-mode)

    (let
      (dtd-failed)

      ;;Make sure we've parsed, which psgml requires.  But that causes
      ;;its own problems.
      (condition-case err
	(sgml-need-dtd)
	(error (setq dtd-failed t)))
      
      (unless
	dtd-failed

	;;Say we're OK if the doctype has the correct name.
	(equal
	  (sgml-dtd-doctype (sgml-pstate-dtd sgml-buffer-parse-state))
	  rmb-dtd-name)))

    nil))


(defun rmb-must-be-in-docfile ()
  ""

  (if 
    (not (rmb-in-rambledocs-buffer-p)) 
    (error "You're not in a rambledocs buffer" )))

(defun rmb-must-not-be-in-docfile ()
  ""
  (if 
    (rmb-in-rambledocs-buffer-p) 
    (error "You're already in a docfile buffer" )))


(defun rmb-insert-doc-dtd ()
  "Insert the rambledocs DTD declaration in the current file"
  
  (interactive)
  (let*
    ( 
      (dtd-declaration 
	(format "<!DOCTYPE %s SYSTEM  \"%s\" >" 
	  rmb-dtd-name rmb-dtd-filename)))
    
    (sgml-doctype-insert dtd-declaration rmb-doc-dtd-local-variables)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;Make a new doc entry for a given function
(defun rmb-make-new-doc-entry (name)
  "Add a psgml element corresponding to NAME.

Return the new element.

Do nothing if we're not in a rambledocs buffer."
  
  (interactive "sfunction name: " )
  (if
    (rmb-in-rambledocs-buffer-p)

    (let*
      ( 
	;;The enclosing element is the top element.  The element can
	;;be moved later if needed.
	(el (sgml-top-element))
	(controller
	  (list 
	    ( cons "Name" name ) ))
	position)
    
    
      (setq position
	(tehom-psgml-add-element-to-element rmb-dtd-component-element-name nil controller el))

      ;;Return the new element.
      (sgml-find-element-of position))))



;;;;;;;;;;;;;;;
;;Find elements

  
(defun rmb-find-doc-element (name)
  "Return the element corresonding to the given function, if any.

This should only be called in a rambledocs buffer."

  (rmb-must-be-in-docfile)

  (tehom-psgml-get-child-by-test-recursive
    ;;Start from the top.
    (sgml-top-element)
    ;;Element test
    #'(lambda (c)
	(and
	  (string= (sgml-element-gi c) rmb-dtd-component-element-name )
	  (string= (sgml-element-attval c "Name" ) name)))
    ;;Tree-descent test
    #'(lambda (c)
	(string= (sgml-element-gi c) rmb-dtd-group-element-name))))




(defun rmb-go-to-doc-component (name)
  "Go to the element documenting function NAME if it exists."
  
  (interactive "sfunction name: " )

  (rmb-must-be-in-docfile)

  (sgml-need-dtd)
  (let*
    ((el (rmb-find-doc-element name)))

    (goto-char 
      (sgml-element-stag-end el))))


(defun rmb-force-go-to-doc-name (name)
  "Go to the element documenting function NAME, create it if neccessary."

  
  (interactive "sfunction name: " )

  (rmb-must-be-in-docfile)

  (let*
    ((el (rmb-find-doc-element name)))

    ;;If the component doesn't exist in the docfile, create it.
    (if
      (null el)
      (setq el (rmb-make-new-doc-entry name)))
    
    ;;Move the cursor there.
    (if el
      (goto-char 
	(sgml-element-stag-end el)))))


;;;;

(defun rmb-doc-get-component-name ()
  "Find the name of the function that we'd look in the source for."
  
  (rmb-must-be-in-docfile)

  (let* 
    (
      (el 
	(tehom-psgml-get-enclosing-element-by-name
	  (sgml-find-element-of (point))
	  rmb-dtd-component-element-name)))
      
    
    ;;Return the final element's name, if any.
    (if 
      el
      (sgml-element-attval el "Name" )
      (error "Point is not inside a function element" ))))


(defun rmb-doc-get-backref-component-name ()
  "Find the name of the function that we'd look in the source for."
  
  (rmb-must-be-in-docfile)

  (let* 
    (
      (el 
	(tehom-psgml-get-enclosing-element-by-name
	  (sgml-find-element-of (point))
	  rmb-dtd-component-element-name)))
      
    
    (if 
      el
      
      (let* 
	((backref-list (tehom-psgml-get-child-by-name el "backrefs"))
	  alist
	  the-backref-el)
	
	(let 
	   ((c (sgml-element-content backref-list)))

	   (while 
	     c
	     (push
	       (cons (sgml-element-attval c "Name" ) c)
	       alist)
      
	     (setq c (sgml-element-next c))))

	(setq 
	  the-backref-el  
	  (cond

	    ((equal 0 (length alist )) 
	      ( error "%s has no backward references"
		(sgml-element-attval el "Name" )))
	  
	    ((equal 1 (length alist )) 
	      (cdr (car alist)))
	  
	    (t
	      (tehom-completing-read-assoc 
		"Use which back reference?" 
		alist))))
	
	the-backref-el

	;;For now, so we know what happened if we call this
	;;(error "rmb-doc-get-backref-component-name is not implemented.")
	)

      (error "Point is not inside a function element" ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Hyer functions


(defun rmb-go-to-match-in-doc ()

  "Go to the matching place in the associated doc buffer. 

This is computed wrt the cursor position in the source buffer"
  
  (interactive)

  (rmb-must-not-be-in-docfile)

  (let
    (( name (rmb-src-get-function-name)))
    
    (rmb-find-docfile-other-window)

    ;;Now we're in the docfile;;

    (rmb-force-go-to-doc-name name)))


(defun rmb-go-to-match-in-src ()
  "Go to the matching place in the associated src buffer. 

This is computed wrt the cursor position in the source buffer"

  
  (interactive)

  (rmb-must-be-in-docfile)

  (using-local-vars
    (local-var name (rmb-doc-get-component-name))
    
    (rmb-find-srcfile-other-window)

    ;;Now we're in the srcfile;;

    (rmb-go-to-src-element name)))



(defun rmb-fill-docfile ()
  "Make sure the docfile is setup sensibly. 

More precisely, make sure it has the appropriate mode and dtd, and
make sure it has an entry for each function in the source.

This should only be called from the source buffer"
  
  (interactive)
  
  (rmb-must-not-be-in-docfile)

  (let*
    ( (alist 
	(rmb-imenu--make-index-alist)))
    

    (rmb-find-docfile-other-window)
    (rmb-set-up-doc-buffer)
    ;;Now we should be in a valid docfile buffer.
    
    (dolist (cell alist)
      
      (let* 
	((name (car cell)))

	;;Skip troublesome elements.
	(unless
	  (member name rmb-skip-imenu-name-list )

	  ;;Create any elements that don't already exist.
	  (if
	    (not (rmb-find-doc-element name))
	    (rmb-make-new-doc-entry name)))))
    
    ;;Turn doc mode on if not already on.
    (rmb-doc-minor-mode t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Two minor modes, for the source file and for the doc file.


;;;###autoload
(easy-mmode-define-minor-mode rmb-doc-minor-mode
  "Toggle rambledocs doc minor mode. 
See also rmb-src-minor-mode." 
     
  nil 
  " rmb" 
  '(
     ("\C-c;."  . rmb-go-to-match-in-src)
     ("\C-c;n"  . rmb-force-go-to-doc-name)
     ("\C-c;g"  . rmb-go-to-doc-component)
     ("\C-c;f"  . rmb-find-srcfile)
     ("\C-c;2f" . rmb-find-srcfile-other-window)
     ("\C-c;d"  . rmb-insert-doc-dtd)
     ))

;;;###autoload
(easy-mmode-define-minor-mode rmb-src-minor-mode
  "Toggle rambledocs source minor mode. 
See also rmb-doc-minor-mode." 

  nil 
  " rmb" 
  '(
     ("\C-c;."  . rmb-go-to-match-in-doc)
     ("\C-c;m"  . rmb-fill-docfile)
     ("\C-c;f"  . rmb-find-docfile)
     ("\C-c;2f" . rmb-find-docfile-other-window)
     ))


;;When elisp starts, start rmb-src-minor-mode.
;;;###autoload ( add-hook 'emacs-lisp-mode-hook 
;;;###autoload   #'(lambda () (rmb-src-minor-mode t)))


;;When psgml starts, only start rmb-doc-minor-mode if we're using the
;;rambledocs dtd.   
;;;###autoload ( add-hook 'sgml-mode-hook
;;;###autoload   #'(lambda () 
;;;###autoload     (if (rmb-in-rambledocs-buffer-p) (rmb-doc-minor-mode t))))




;;;;;;;;;;;;;;;;;;;;;;;;
;;imenu modification: We need to force rescans sometimes
;;imenu is welcome to borrow these.

(defvar rmb-buffer-modified-tick 0 "" )
(make-variable-buffer-local 'rmb-buffer-modified-tick)

(defun rmb-imenu--make-index-alist ()
  ""
  (let*
    ((last-mod (buffer-modified-tick))
      ;;Temporarily disable message logging.
      message-log-max)
    (if 
      (not (equal last-mod rmb-buffer-modified-tick))
      (progn
	(setq rmb-buffer-modified-tick last-mod)
	(rmb-imenu-force-rescan)))

    (imenu--make-index-alist)))



(defun rmb-imenu-force-rescan ()
  "Force a rescan of the current buffer"
  (imenu--cleanup)
  (setq imenu--index-alist nil))

(provide 'rmb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Local variables:
;;End:
;;; rambledocs.el ends here
