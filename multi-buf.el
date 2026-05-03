;;; multi-buf.el --- Buffer multiplexer -*- lexical-binding: t; -*-
;; Copyright (C) 2026 David J. Rosenbaum

;; Author: David J. Rosenbaum <djr7c4@gmail.com>

;; Keywords: buffers, convenience, processes, terminals, tools
;; URL: https://github.com/djr7C4/multi-buf
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; TODO

;;; Code:
(require 'cl-lib)
(require 'eieio)
(require 'project)

;;; Utilities
(defun multi-buf-project-root ()
  (file-truename (or (and-let* ((proj (project-current))) (project-root proj))
                     default-directory)))

;;; Low-level library
;; This variable shouldn't have the same name as the `multi-buf-backend' class
;; because `defclass' defines a variable with the same name as the class.
(defvar-local multi-buf-backend-instance nil)

(defclass multi-buf-backend ()
  ((buffers :initarg :buffers :initform nil)
   (name :initarg :name :initform (error "Name is required"))
   (include-in-general-switch-p :initarg :include-in-general-switch-p :initform t)))

(cl-defgeneric multi-buf-new (backend)
  (:documentation
   "Create a new buffer for BACKEND.

The return value is the new buffer. This function should not
switch to the new buffer. It should only create it."))

(cl-defgeneric multi-buf-cleanup (backend buf)
  (:documentation
   "Perform cleanup when a buffer is killed."))

(cl-defmethod multi-buf-cleanup ((backend multi-buf-backend) buf)
  (oset backend buffers (delete buf (oref backend buffers))))

(defun multi-buf-cleanup-wrapper ()
  (when-let* ((backend multi-buf-backend-instance))
    (multi-buf-cleanup backend (current-buffer))))

(defun multi-buf-register (backend buf)
  (with-current-buffer buf
    (unless multi-buf-backend-instance
      ;; Add new buffers at the end to preserve the order.
      (oset backend buffers (append (oref backend buffers) (list buf)))
      (setq-local multi-buf-backend-instance backend)
      (add-hook 'kill-buffer-hook #'multi-buf-cleanup-wrapper nil t))))

(cl-defmethod multi-buf-new :around ((backend multi-buf-backend))
  (let ((buf (cl-call-next-method)))
    (multi-buf-register backend buf)
    buf))

(cl-defgeneric multi-buf-category (backend buf)
  (:documentation
   "Return the category of BUF for BACKEND.

This can be used to refine the buffers that are considered
candidates when cycling or switching to a buffer using
`completing-read'. If no such refinement is needed for a backend,
then there is no need to implement this method."))

(cl-defmethod multi-buf-category ((_backend multi-buf-backend) _buf)
  nil)

(cl-defgeneric multi-buf-use-category-default (backend action-type)
  (:documentation
   "Return the default value of the `:use-category' for BACKEND.

ACTION-TYPE indicates the type of action and can be either
`cycle' or `switch'. `cycle' indicates a \"next\" or \"previous\"
style action such as `multi-buf-next' or `multi-buf-previous'
while `switch' indicates a buffer switching action such as
`multi-buf-switch'."))

(cl-defmethod multi-buf-use-category-default ((_backend (eql nil)) _action-type)
  nil)

(cl-defmethod multi-buf-use-category-default ((_backend multi-buf-backend) _action-type)
  t)

(cl-defgeneric multi-buf-include-in-general-switch-p (backend buf)
  (:documentation
   "Determine if BUF should be an option when switching any buffer."))

(cl-defmethod multi-buf-include-in-general-switch-p ((backend multi-buf-backend) _buf)
  (oref backend include-in-general-switch-p))

(defvar multi-buf-display-buffer-same-window-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (inhibit-same-window . nil))
  "The `display-buffer' action to show a buffer in the same window.")

(cl-defgeneric multi-buf-display-buffer-action-type (backend buf action-type)
  (:documentation
   "Return the `display-buffer' action to use for BUF with BACKEND.

The argument ACTION-TYPE indicates the type of action and can be
either `new', `cycle', `switch'. `new' indicates that a buffer
was just created while `cycle' indicates a \"next\" or
\"previous\" style action such as `multi-buf-next' or
`multi-buf-previous'. `switch' indicates a buffer switching
action such as `multi-buf-switch'."))

(cl-defmethod multi-buf-display-buffer-action-type ((backend multi-buf-backend) _buf action-type)
  (cl-ecase action-type
    (new nil)
    ;; When cycling from a buffer for this backend, use the same window.
    ;; Otherwise, use the other window.
    (cycle (and (multi-buf-match-p backend (current-buffer))
                multi-buf-display-buffer-same-window-action))
    (switch multi-buf-display-buffer-same-window-action)))

(cl-defgeneric multi-buf-match-p (backend buf)
  "Determine if BUF belongs to BACKEND.")

(cl-defmethod multi-buf-match-p ((backend multi-buf-backend) buf)
  (with-current-buffer buf
    (eq backend multi-buf-backend-instance)))

(defun multi-buf-all ()
  (cl-remove-if-not (lambda (buf)
                      (with-current-buffer buf
                        (and-let* ((backend multi-buf-backend-instance))
                          (multi-buf-include-in-general-switch-p backend buf))))
                    (buffer-list)))

(cl-defgeneric multi-buf-filter (backend use-category))

(cl-defmethod multi-buf-filter ((_backend (eql nil)) _use-category)
  (multi-buf-all))

(cl-defmethod multi-buf-filter ((backend multi-buf-backend) use-category)
  (let ((category (multi-buf-category backend (current-buffer))))
    (cl-remove-if-not (lambda (buf)
                        (and (multi-buf-match-p backend buf)
                             (or (not use-category)
                                 (equal category (multi-buf-category backend buf)))))
                      (oref backend buffers))))

(cl-defgeneric multi-buf-pop-to (backend buf action-type))

(cl-defmethod multi-buf-pop-to ((_backend (eql nil)) buf action-type)
  (with-current-buffer buf
    (if-let* ((backend multi-buf-backend-instance))
        (multi-buf-pop-to backend buf action-type)
      (error "No backend found for %S" buf))))

(cl-defmethod multi-buf-pop-to ((backend multi-buf-backend) buf action-type)
  (pop-to-buffer buf (multi-buf-display-buffer-action-type backend buf action-type)))

;;; Commands
(defun multi-buf-create (backend)
  "Create a new buffer for BACKEND.

This is a convenience function for implementating multi-buf-new-*
commands for specific backends."
  (multi-buf-pop-to backend (multi-buf-new backend) 'new))

(cl-defun multi-buf-next (backend &key (offset 1) (use-category (multi-buf-use-category-default backend 'cycle)))
  "Switch to the next buffer for BACKEND.

If no backend is available for the current buffer, switch to the
next buffer for any backend. OFFSET specifies how many positions
to move from the current buffer. If USE-CATEGORY is non-nil, then
only buffers with the same category will be considered."
  (interactive (let ((backend multi-buf-backend-instance))
                 (list backend
                       :use-category (xor (multi-buf-use-category-default backend 'cycle)
                                          current-prefix-arg))))
  (let* ((bufs (multi-buf-filter backend use-category))
         (k (cl-position (current-buffer) bufs))
         (buf (cond
               ;; If the current buffer doesn't belong to backend, use the
               ;; most recently displayed buffer for backend.
               ((not (memq (current-buffer) bufs))
                (cl-find-if (lambda (b)
                              (memq b bufs))
                            (buffer-list)))
               ;; When the current buffer is the only buffer for the backend,
               ;; don't do anything. We return nil in this case since no switch
               ;; was performed.
               ((= (length bufs) 1)
                nil)
               ;; Otherwise, go forward offset buffers.
               (t
                (elt bufs (mod (+ k offset) (length bufs)))))))
    (when buf
      (multi-buf-pop-to backend buf 'cycle))
    buf))

(cl-defun multi-buf-previous (backend &key (offset 1) (use-category (multi-buf-use-category-default backend 'cycle)))
  "Switch to the previous buffer for BACKEND.

OFFSET specifies how many positions to move from the current
buffer. If USE-CATEGORY is non-nil, then only buffers with the
same category will be considered."
  (interactive (let ((backend multi-buf-backend-instance))
                 (list backend
                       :use-category (xor (multi-buf-use-category-default backend 'cycle)
                                          current-prefix-arg))))
  (multi-buf-next backend :offset (- offset) :use-category use-category))

(cl-defun multi-buf-switch (backend &key (use-category (multi-buf-use-category-default backend 'switch)))
  "Switch to a buffer for BACKEND.

If USE-CATEGORY is non-nil, then only buffers with the same
category will be considered."
  (interactive (let ((backend multi-buf-backend-instance))
                 (list backend
                       :use-category (xor (multi-buf-use-category-default backend 'switch)
                                          current-prefix-arg))))
  (let* ((bufs (multi-buf-filter backend use-category))
         (buf (read-buffer (format "Choose %sbuffer: "
                                   (if backend
                                       (concat (oref backend name) " ")
                                     ""))
                           nil
                           t
                           (lambda (b)
                             (memq (or (cdr-safe b) b) bufs)))))
    (multi-buf-pop-to backend buf 'switch)))

(defun multi-buf-dwim (backend arg)
  "Cycle to, switch to or create a buffer for BACKEND.

ARG is the prefix argument. If it is nil or an integer, then
perform cycling according to its numeric value if a buffer exists
for BACKEND other than the current buffer. If no such buffer
exists, create a new one. With a universal prefix argument,
always create a new buffer. With two universal prefix arguments,
switch to a buffer for BACKEND using completion. With three
universal prefix arguments, switch to a buffer for BACKEND using
completion with the default value of `:use-category' negated.
With a negative universal prefix argument, switch to a buffer for
any backend."
  (cond
   ((or (null arg) (eq arg '-) (integerp arg))
    (or (multi-buf-next backend :offset (prefix-numeric-value arg))
        ;; If there is no buffer to switch to other than the current one, create
        ;; a new buffer.
        (multi-buf-pop-to backend (multi-buf-new backend) 'new)))
   ((equal arg '(16))
    (multi-buf-switch backend))
   ((equal arg '(64))
    (let ((use-category (multi-buf-use-category-default backend 'switch)))
      (multi-buf-switch backend :use-category (not use-category))))
   ((and (consp arg) (< (prefix-numeric-value arg) 0))
    (multi-buf-switch nil))
   (t
    (multi-buf-pop-to backend (multi-buf-new backend) 'new))))

;;; Default backends
;;; eshell
(defclass multi-buf-eshell-backend (multi-buf-backend) ())

(defvar multi-buf-eshell-backend-instance (multi-buf-eshell-backend :name "eshell"))

(declare-function eshell "eshell")

(cl-defmethod multi-buf-new ((_backend multi-buf-eshell-backend))
  (prog2
      (eshell '-)
      (current-buffer)
    (bury-buffer)))

;; `eshell' buffers belong to their directory.
(cl-defmethod multi-buf-category ((_backend multi-buf-eshell-backend) buf)
  (with-current-buffer buf
    default-directory))

(defun multi-buf-new-eshell ()
  "Create a `eshell' buffer."
  (interactive)
  (multi-buf-create multi-buf-eshell-backend-instance))

(defun multi-buf-eshell-dwim (&optional arg)
  "Cycle to, switch to or create a new `eshell' buffer.

If no prefix argument ARG is provided then cycle forward to the
next eshell buffer. If the prefix argument is an integer, then
perform cycling according to its numeric value. If no eshell buffer
buffer exists other than the current buffer, create a new one.
With a universal prefix argument, always create a new eshell buffer. With
two universal prefix arguments, switch to a eshell buffer in the same
directory using completion. With three universal prefix
arguments, switch to any eshell buffer using completion. With a negative
universal prefix argument, switch to a buffer for any backend."
  (interactive "P")
  (multi-buf-dwim multi-buf-eshell-backend-instance arg))

;;; vterm
(defclass multi-buf-vterm-backend (multi-buf-backend) ())

(defvar multi-buf-vterm-backend-instance (multi-buf-vterm-backend :name "vterm"))

(declare-function vterm "vterm")

(cl-defmethod multi-buf-new ((_backend multi-buf-vterm-backend))
  (prog2
      (vterm '-)
      (current-buffer)
    (bury-buffer)))

;; `vterm' buffers belong to their directory.
(cl-defmethod multi-buf-category ((_backend multi-buf-vterm-backend) buf)
  (with-current-buffer buf
    default-directory))

(defun multi-buf-new-vterm ()
  "Create a `vterm' buffer."
  (interactive)
  (multi-buf-create multi-buf-vterm-backend-instance))

(defun multi-buf-vterm-dwim (&optional arg)
  "Cycle to, switch to or create a new `vterm'.

If no prefix argument ARG is provided then cycle forward to the
next vterm. If the prefix argument is an integer, then perform
cycling according to its numeric value. If no vterm exists other
than the current buffer, create a new one. With a universal
prefix argument, always create a new vterm. With two universal
prefix arguments, switch to a vterm in the same directory using
completion. With three universal prefix arguments, switch to any
vterm using completion. With a negative universal prefix
argument, switch to a buffer for any backend."
  (interactive "P")
  (multi-buf-dwim multi-buf-vterm-backend-instance arg))

;;; gptel
(defclass multi-buf-gptel-backend (multi-buf-backend) ())

(defvar multi-buf-gptel-backend-instance (multi-buf-gptel-backend :name "gptel"))

(declare-function gptel "gptel")

(cl-defmethod multi-buf-new ((_backend multi-buf-gptel-backend))
  (let ((name (generate-new-buffer-name "*gptel*")))
    (gptel name
           nil
           ;; Support the `gptel' feature for inserting regions into the buffer.
           (and (use-region-p) (buffer-substring (region-beginning) (region-end)))
           t)
    (bury-buffer)
    (get-buffer name)))

;; `gptel' buffers belong to their project.
(cl-defmethod multi-buf-category ((_backend multi-buf-gptel-backend) buf)
  (with-current-buffer buf
    (multi-buf-project-root)))

(defun multi-buf-new-gptel ()
  "Create a `gptel' buffer."
  (interactive)
  (multi-buf-create multi-buf-gptel-backend-instance))

(defun multi-buf-gptel-dwim (&optional arg)
  "Cycle to, switch to or create a new `gptel' buffer.

If no prefix argument ARG is provided then cycle forward to the
next gptel buffer for the current project. If the prefix argument
is an integer, then perform cycling according to its numeric
value. If no gptel buffer for the current project exists other
than the current buffer, create a new one. With a universal
prefix argument, always create a new gptel buffer. With two
universal prefix arguments, switch to a gptel buffer in the same
project using completion. With three universal prefix arguments,
switch to any gptel buffer using completion. With a negative
universal prefix argument, switch to a buffer for any backend."
  (interactive "P")
  (multi-buf-dwim multi-buf-gptel-backend-instance arg))

;;; gptel-agent
(defclass multi-buf-gptel-agent-backend (multi-buf-backend) ())

(defvar multi-buf-gptel-agent-backend-instance (multi-buf-gptel-agent-backend :name "gptel-agent"))

(declare-function gptel-agent "gptel-agent")

(cl-defmethod multi-buf-new ((_backend multi-buf-gptel-agent-backend))
  ;; `gptel-agent' doesn't provide a nice way to get the buffer so we
  ;; temporarily rebind the `gptel' function like `noflet' does.
  (let (buf (orig-gptel (symbol-function 'gptel)))
    (unwind-protect
        (progn
          (setf (symbol-function 'gptel)
                (lambda (&rest args)
                  (setq buf (apply orig-gptel args))))
          (gptel-agent (multi-buf-project-root)))
      (setf (symbol-function 'gptel) orig-gptel))
    (bury-buffer)
    buf))

;; `gptel-agent' buffers belong to their project.
(cl-defmethod multi-buf-category ((_backend multi-buf-gptel-agent-backend) buf)
  (with-current-buffer buf
    (multi-buf-project-root)))

(defun multi-buf-new-gptel-agent ()
  "Create a `gptel-agent' buffer."
  (interactive)
  (multi-buf-create multi-buf-gptel-agent-backend-instance))

(defun multi-buf-gptel-agent-dwim (&optional arg)
  "Cycle to, switch to or create a new `gptel-agent' buffer.

If no prefix argument ARG is provided then cycle forward to the
next gptel-agent buffer for the current project. If the prefix
argument is an integer, then perform cycling according to its
numeric value. If no gptel-agent buffer for the current project
exists other than the current buffer, create a new one. With a
universal prefix argument, always create a new gptel-agent
buffer. With two universal prefix arguments, switch to a
gptel-agent buffer in the same project using completion. With
three universal prefix arguments, switch to any gptel-agent
buffer using completion. With a negative universal prefix
argument, switch to a buffer for any backend."
  (interactive "P")
  (multi-buf-dwim multi-buf-gptel-agent-backend-instance arg))

;;; Indirect buffers
(defclass multi-buf-indirect-backend (multi-buf-backend) ())

(defvar multi-buf-indirect-backend-instance
  (multi-buf-indirect-backend :name "indirect"
                              :include-in-general-switch-p nil))

(cl-defmethod multi-buf-new ((backend multi-buf-indirect-backend))
  (let ((buf (or (buffer-base-buffer) (current-buffer))))
    ;; Include the base buffer.
    (multi-buf-register backend buf)
    (make-indirect-buffer buf (generate-new-buffer-name (buffer-name buf)))))

(cl-defmethod multi-buf-new :around ((backend multi-buf-indirect-backend))
  ;; Prevent the user from creating multi-buf-managed indirect buffers from
  ;; buffers with another backend.
  (if (and multi-buf-backend-instance
           (not (multi-buf-match-p backend (current-buffer))))
      (user-error "Cannot create an indirect buffer for a buffer with another backend")
    (cl-call-next-method)))

(cl-defmethod multi-buf-category ((_backend multi-buf-indirect-backend) buf)
  ;; Indirect buffers belong to their base buffer. Base buffers belong to
  ;; themselves.
  (or (buffer-base-buffer buf) buf))

(cl-defmethod multi-buf-cleanup ((backend multi-buf-indirect-backend) buf)
  (cl-call-next-method)
  ;; When only the base buffer is left, remove it. It is no longer considered a
  ;; base buffer when all its indirect buffers are gone.
  (letrec ((base-buf (or (buffer-base-buffer buf) buf))
           (base-or-indirect-p (lambda (b)
                                 (or (eq b base-buf)
                                     (eq (buffer-base-buffer b) base-buf)))))
    (when (= (cl-count-if base-or-indirect-p
                          (oref backend buffers))
             1)
      (with-current-buffer base-buf
        (kill-local-variable 'multi-buf-backend-instance))
      (oset backend
            buffers
            (cl-remove-if base-or-indirect-p (oref backend buffers))))))

(defun multi-buf-new-indirect ()
  "Create an indirect buffer."
  (interactive)
  (multi-buf-create multi-buf-indirect-backend-instance))

(defun multi-buf-indirect-dwim (&optional arg)
  "Cycle to, switch to or create a new indirect buffer.

The indirect buffer is created for the current buffer or the base
buffer of the current buffer if the current buffer is indirect.

If no prefix argument ARG is provided then cycle forward among
the current base buffer and its indirect buffers. If the prefix
argument is an integer, then perform cycling according to its
numeric value. If no indirect buffer exists, create a new one.
With a universal prefix argument, always create a new indirect
buffer. With two universal prefix arguments, switch to the base
buffer or an indirect buffer using completion. With three
universal prefix arguments, switch to any base buffer or indirect
buffer using completion. With a negative universal prefix
argument, switch to a buffer for any backend."
  (interactive "P")
  (multi-buf-dwim multi-buf-indirect-backend-instance arg))

(provide 'multi-buf)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("dflet" . "noflet")
;;   ("plet" . "pcase-let")
;;   ("plet*" . "pcase-let*")
;;   ("psetq*" . "pcase-setq")
;;   ("pdolist" . "pcase-dolist")
;;   ("plambda" . "pcase-lambda")
;;   ("pdefmacro" . "pcase-defmacro")
;;   ("epcase" . "pcase-exhaustive")
;;   ("dsb" . "cl-destructuring-bind")
;;   ("mvb" . "cl-multiple-value-bind")
;;   ("mvs" . "cl-multiple-value-setq")
;;   ("with-gensyms" . "cl-with-gensyms")
;;   ("once-only" . "cl-once-only")
;;   ("fn" . "rem-fn")
;;   ("fn1" . "rem-fn1")
;;   ("fn2" . "rem-fn2")
;;   ("fn3" . "rem-fn3")
;;   ("fn4" . "rem-fn4")
;;   ("fn5" . "rem-fn5")
;;   ("fn6" . "rem-fn6")
;;   ("fn7" . "rem-fn7")
;;   ("fn8" . "rem-fn8")
;;   ("fn9" . "rem-fn9")
;;   ("fn10" . "rem-fn10"))
;; End:
;;; multi-buf.el ends here
