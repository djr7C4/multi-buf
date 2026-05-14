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

(defmacro multi-buf-with-displayed-buffer (&rest body)
  "Return the last buffer BODY attempted to display.

No buffers are displayed within BODY."
  (declare (indent 0))
  (with-gensyms (buf remember)
    `(let* (,buf
            (,remember (lambda (buffer alist)
                         (display-buffer-no-window (setq ,buf buffer) alist)))
            (display-buffer-overriding-action `(,,remember (allow-no-window . t))))
       ,@body
       ,buf)))

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

(cl-defmethod multi-buf-category ((_backend multi-buf-backend) buf)
  (with-current-buffer buf
    (multi-buf-project-root)))

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

(cl-defmethod multi-buf-include-in-general-switch-p ((backend multi-buf-backend) buf)
  (or (oref backend include-in-general-switch-p)
      ;; Buffers that match the current buffer are always included.
      (multi-buf-match-p multi-buf-backend-instance buf)))

(defvar multi-buf-display-buffer-same-window-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (inhibit-same-window . nil))
  "The `display-buffer' action to show a buffer in the same window.")

(cl-defgeneric multi-buf-display-buffer-action (backend buf action-type)
  (:documentation
   "Return the `display-buffer' action to use for BUF with BACKEND.

The argument ACTION-TYPE indicates the type of action and can be
either `new', `cycle', `switch'. `new' indicates that a buffer
was just created while `cycle' indicates a \"next\" or
\"previous\" style action such as `multi-buf-next' or
`multi-buf-previous'. `switch' indicates a buffer switching
action such as `multi-buf-switch'."))

(cl-defmethod multi-buf-display-buffer-action ((backend multi-buf-backend) _buf action-type)
  (cl-ecase action-type
    (new nil)
    ;; When cycling from a buffer for this backend, use the same window.
    ;; Otherwise, use the other window.
    (cycle (and (multi-buf-match-p backend (current-buffer))
                multi-buf-display-buffer-same-window-action))
    (switch nil)))

(cl-defgeneric multi-buf-match-p (backend buf)
  "Determine if BUF belongs to BACKEND.")

(cl-defmethod multi-buf-match-p ((backend multi-buf-backend) buf)
  (with-current-buffer buf
    (eq backend multi-buf-backend-instance)))

(defun multi-buf-all ()
  (cl-remove-if-not (lambda (buf)
                      (and-let* ((backend (with-current-buffer buf
                                            multi-buf-backend-instance)))
                        (multi-buf-include-in-general-switch-p backend buf)))
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
  (pop-to-buffer buf (multi-buf-display-buffer-action backend buf action-type)))

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

(cl-defun multi-buf-switch (backend &key (use-category (multi-buf-use-category-default backend 'switch)) all)
  "Switch to a buffer for BACKEND.

If USE-CATEGORY is non-nil (interactively with a universal prefix
argument), switch to buffers with the same category. When ALL is
non-nil (interactively with two universal prefix arguments), then
switch to any buffer for any backend."
  (interactive (let ((backend multi-buf-backend-instance))
                 (list backend
                       :use-category (xor (multi-buf-use-category-default backend 'switch)
                                          (and (consp current-prefix-arg)
                                               (not (equal current-prefix-arg '(16)))))
                       :all (equal current-prefix-arg '(16)))))
  (let* ((bufs (if all
                   (multi-buf-all)
                 (multi-buf-filter backend use-category)))
         (buf (read-buffer (format "Choose %sbuffer: "
                                   (if backend
                                       (format "a %s " (oref backend name))
                                     "any "))
                           nil
                           t
                           (lambda (b)
                             (memq (or (cdr-safe b) b) bufs)))))
    (multi-buf-pop-to backend buf 'switch)))

(cl-defun multi-buf-dwim-docstring
    (&key
     name
     (command-phrase (format "`%s'" name))
     (buffer-name (format "%s buffer" name))
     region-force-new)
  (let ((docstring (format "\"Cycle to, switch to or create a new %1$s.

If no prefix argument ARG is provided then cycle forward to the
next %2$s. If the prefix argument is an integer, then perform
cycling according to its numeric value. If no %2$s exists other
than the current buffer, create a new one. With a universal
prefix argument, always create a new %2$s. With two universal
prefix arguments, switch to a %2$s in the same project using
completion. With three universal prefix arguments, switch to any
%2$s using completion. With a negative universal prefix argument,
switch to a buffer for any backend.%s\""
                           command-phrase
                           buffer-name
                           (if region-force-new
                               (format "\n\nWhen REGION-FORCE-NEW is non-nil, always create a new %s." buffer-name)
                             ""))))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert docstring)
      (goto-char (point-min))
      (forward-line 2)
      (fill-paragraph)
      ;; Remove quotes from the docstring. These were included initially so that
      ;; `fill-paragraph' would work correctly.
      (goto-char (point-min))
      (delete-char 1)
      (goto-char (1- (point-max)))
      (delete-char 1)
      (substring-no-properties (buffer-string)))))

(cl-defun multi-buf-dwim (backend arg &key region-force-new)
  (:documentation (multi-buf-dwim-docstring :command-phrase "BACKEND buffer"
                                            :buffer-name "BACKEND buffer"
                                            :region-force-new t))
  (cond
   ((or (null arg) (eq arg '-) (integerp arg))
    (or (and (not (and region-force-new
                       (use-region-p)))
             (multi-buf-next backend :offset (prefix-numeric-value arg)))
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

(cl-defmacro multi-buf-define-backend
    (name
     &key
     (backend-class (intern (format "multi-buf-%s-backend" name)))
     (backend-instance (intern (format "multi-buf-%s-backend-instance" name)))
     (backend-parent-classes '(multi-buf-backend))
     new-form
     (command-phrase (format "`%s'" name))
     (buffer-name (format "%s buffer" name))
     region-force-new)
  (declare (indent 1))
  `(progn
     ,(and backend-class
           `(defclass ,backend-class (,@backend-parent-classes) ()))
     ,(and backend-instance
           backend-class
           `(defvar ,backend-instance (,backend-class :name ,name)))
     ,(and backend-class
           new-form
           `(cl-defmethod multi-buf-new ((_backend ,backend-class))
              ,new-form))
     ,(and backend-instance
           `(defun ,(intern (format "multi-buf-new-%s" name)) ()
              ,(format "Create a new %s buffer." command-phrase)
              (interactive)
              (multi-buf-create ,backend-instance)))
     ,(and backend-instance
           `(defun ,(intern (format "multi-buf-%s-dwim" name)) (&optional arg)
              ,(multi-buf-dwim-docstring :name name
                                         :command-phrase command-phrase
                                         :buffer-name buffer-name)
              (interactive "P")
              (multi-buf-dwim ,backend-instance arg :region-force-new ,region-force-new)))))

;;; Default backends
(multi-buf-define-backend "eshell"
  :new-form (multi-buf-with-displayed-buffer (eshell '-)))

(multi-buf-define-backend "shell"
  :new-form (multi-buf-with-displayed-buffer
              (shell (generate-new-buffer-name "*shell*"))))

(multi-buf-define-backend "term"
  ;; `make-term' adds earmuffs to the name so we can't use
  ;; `generate-new-buffer-name'.
  :new-form (let* ((base-name "terminal")
                   (name base-name)
                   (suffix 1))
              (while (get-buffer (format "*%s*" name))
                (setq name (format "%s<%d>" base-name (cl-incf suffix))))
              (make-term name
                         ;; Copied from `term'.
                         (or explicit-shell-file-name
                             (getenv "ESHELL")
                             shell-file-name))))

(multi-buf-define-backend "vterm"
  :new-form (multi-buf-with-displayed-buffer (vterm '-)))

(multi-buf-define-backend "vterm"
  :new-form (multi-buf-with-displayed-buffer (vterm '-)))

(multi-buf-define-backend "chatgpt-shell"
  :new-form (multi-buf-with-displayed-buffer (chatgpt-shell t)))

(multi-buf-define-backend "agent-shell"
  ;; `multi-buf-with-displayed-buffer' doesn't work with `agent-shell'.
  :new-form (prog1
                (agent-shell '(4))
              (bury-buffer)))

(multi-buf-define-backend "gptel"
  :new-form (let ((name (generate-new-buffer-name "*gptel*")))
              (multi-buf-with-displayed-buffer
                (gptel name
                       nil
                       ;; Support the `gptel' feature for inserting regions into
                       ;; the buffer.
                       (and (use-region-p)
                            (buffer-substring (region-beginning) (region-end)))
                       t)))
  ;; When a region is selected, always create a new gptel with the selected
  ;; region as the initial prompt.
  :region-force-new t)

(multi-buf-define-backend "gptel-agent"
  :new-form (multi-buf-with-displayed-buffer
              (gptel-agent (multi-buf-project-root)))
  :region-force-new t)

;;; Indirect buffers
(defclass multi-buf-indirect-backend (multi-buf-backend) ())

(defvar multi-buf-indirect-backend-instance
  (multi-buf-indirect-backend :name "indirect"
                              :include-in-general-switch-p nil))

(cl-defmethod multi-buf-cleanup ((backend multi-buf-indirect-backend) buf)
  (cl-call-next-method)
  ;; When only the base buffer is left, remove it. It is no longer considered a
  ;; base buffer when all its indirect buffers are gone.
  (let* ((base-buf (or (buffer-base-buffer buf) buf))
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

(cl-defmethod multi-buf-new ((backend multi-buf-indirect-backend))
  (let ((base-buf (or (buffer-base-buffer) (current-buffer))))
    ;; Include the base buffer.
    (multi-buf-register backend base-buf)
    (with-current-buffer
        (make-indirect-buffer base-buf (generate-new-buffer-name (buffer-name base-buf)) t)
      ;; Buffer-local variables are copied from the base buffer via
      ;; `make-indirect-buffer' since the clone argument is t above. We remove
      ;; the local binding for `multi-buf-backend-instance' so that
      ;; `multi-buf-register' won't think that the buffer has already been
      ;; registered.
      (kill-local-variable 'multi-buf-backend-instance)
      (current-buffer))))

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

;; When the current buffer is a base buffer and we are cycling, display the
;; indirect buffer in the other window.
(cl-defmethod multi-buf-display-buffer-action ((backend multi-buf-indirect-backend) buf action-type)
  (unless (and (eq action-type 'cycle)
               (multi-buf-match-p backend (current-buffer))
               (not (buffer-base-buffer (current-buffer))))
    (cl-call-next-method)))

(defun multi-buf-new-indirect ()
  "Create an indirect buffer."
  (interactive)
  (multi-buf-create multi-buf-indirect-backend-instance))

(defun multi-buf-indirect-dwim (&optional arg)
  (:documentation (multi-buf-dwim-docstring :command-phrase "indirect buffer"
                                            :buffer-name "indirect buffer"))
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
