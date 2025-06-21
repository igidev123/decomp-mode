;; Define the minor mode
(define-minor-mode decomp-mode
  "A minor mode for compiling C code to assembly and navigating it."
  :lighter " decomp"
  (if c-assembly-mode
      (progn
        ;; Set up keybindings
        (local-set-key (kbd "C-c C-a") 'c-assembly-compile-and-show)
        (local-set-key (kbd "C-c C-j") 'c-assembly-jump-to-line))
    ;; Cleanup if necessary
    ))


(defvar *c-assembly-debug* nil)
(defun debug-message (&rest body)
  (if *c-assembly-debug*
      (apply 'message body)))

(defmacro defmemoized (name parameters &rest body)
  "Define a memoized function NAME with PARAMETERS and BODY"
  (let ((memo-var (gensym)))
    `(progn
       (defvar ,memo-var nil)
       (defun ,name ,parameters
         (or ,memo-var
             (setq ,memo-var (progn ,@body)))))))

(defclass asm-line ()
  ((src-line :initarg :src-line :accessor asm-line-src-line)
   (address  :initarg :address  :accessor asm-line-address)
   (base-instruction :initarg :base-instruction :accessor asm-line-base-instruction)
   (full-instruction :initarg :full-instruction :accessor asm-line-full-instruction)))

(defun asm-compare (a b)
  (equal (asm-line-base-instruction a) (asm-line-base-instruction b)))
;;  (cond ((eq (asm-line-full-instruction a) (asm-line-full-instruction b))
;;         :full-match)
;;        ((eq (asm-line-base-instruction a) (asm-line-base-instruction b))
;;         :base-match)
;;        (t nil)))

(defmemoized c-assembly-get-base-obj ()
             (concat (make-temp-file "c-asm-") ".o"))
(defmemoized c-assembly-get-base-asm ()
             (concat (make-temp-file "c-asm-") ".s"))

(defmemoized c-assembly-get-target-obj ()
             (concat (make-temp-file "c-asm-") ".o"))
(defmemoized c-assembly-get-target-asm ()
             (concat (make-temp-file "c-asm-") ".s"))
(defvar *src-target-file* "./target.s")
(defvar *loaded-asm* nil)
(defvar *base-asm-buffer-name* "*Base*")
(defvar *target-asm-buffer-name* "*Target*")

(defun c-assembly-dump-asm-file (obj-file asm-file)
  (shell-command
   (format "arm-none-eabi-objdump -l --no-show-raw-insn -d %s > %s"
           obj-file asm-file)))
(defun c-assembly-compile-file (src-file obj-file)
  (let ((default-directory (file-name-directory (buffer-file-name))))
    (shell-command (format "./compile.sh %s -o %s -g" src-file obj-file))))

(defun c-assembly-compile ()
  "Compile the current buffer to assembly."
  ;;(interactive)
  (let ((obj-file (c-assembly-get-base-obj))
        (asm-file (c-assembly-get-base-asm)))
    (c-assembly-compile-file (buffer-file-name) obj-file)
    (c-assembly-dump-asm-file obj-file asm-file)))

(defun c-assembly-assemble-file (src-file obj-file)
  (shell-command
   (format "arm-none-eabi-as -march=armv5te %s -o %s"
           src-file obj-file)))

(defun c-assembly-dump-asm ()
  "Assemble and dump target assembly."
  (let ((obj-file (c-assembly-get-target-obj))
        (asm-file (c-assembly-get-target-asm)))
    (c-assembly-assemble-file *src-target-file* obj-file)
    (c-assembly-dump-asm-file obj-file asm-file)))


;; Source-line '^[^\s]*:([0-9]*)$'
;; Label       '^([0-9a-f]*)\s(.*):$'
;; Instruction '^\s*([0-9a-f]+):\s*(([a-z]+)\s+.*)$'
(defun load-asm (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((lines (split-string (buffer-string) "\n" t))
          (source-ln 0)
          (result '()))
      (dolist (line lines result)
        (cond
         ((string-match "^[^[:space:]]*:\\([0-9]+\\)$" line)
          ;; Line number
          (setq source-ln (string-to-number (match-string 1 line))))
         ((string-match "^([0-9a-f]*)\s(.*):$" line)
          ;; Label
          (debug-message line))
         ((string-match "^[[:space:]]*\\([0-9a-f]+\\):[[:space:]]*\\(\\([a-z]+\\).*\\)$" line)
          ;; Instruction
          (setq result (nconc result
                              (list (make-instance 'asm-line
                                     :src-line source-ln
                                     :address (string-to-number (match-string 1 line) 16)
                                     :base-instruction (match-string 3 line)
                                     :full-instruction (match-string 2 line))))))))
      result)))


(defun format-asm-line (asm)
  (format "%4d %4x:  %s"
          (asm-line-src-line asm)
          (asm-line-address asm)
          (asm-line-full-instruction asm)))
(defun format-asm-no-line (asm)
  (format "%4x:  %s"
          (asm-line-address asm)
          (asm-line-full-instruction asm)))

(defun string-to-exact-length (string target-length)
  (let ((current-length (string-width string)))
    (cond ((> current-length target-length)
         (truncate-string-to-width string target-length nil ?\…))
        ((< current-length target-length)
         (concat string (make-string (- target-length current-length) ?\s)))
        (t string))))


(defun format-diff-line (type left right)
  (let* ((width (window-width))
         (half-width (/ (- width 3) 2))
         (left-string (string-to-exact-length (if left
                                                  (format-asm-line left)
                                                ">")
                                              half-width))
         (right-string (string-to-exact-length (if right
                                                   (format-asm-no-line right)
                                                 "<")
                                               half-width)))
    (format "%s | %s\n" left-string right-string)))


(defun display-asm (asm-diff)
  "Display the assembly in a buffer."
  (let ((buffer (get-buffer-create *base-asm-buffer-name*)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (dolist (item asm-diff)
        (let ((type (car item))
              (left (cadr item))
              (right (caddr item)))
          (insert (format-diff-line type left right)))))))

(message "%s" *base-asm-buffer-name*)

(defun jump-to-asm (asm line)
  (let ((buffer (get-buffer *base-asm-buffer-name*)))
    (if buffer
        (with-current-buffer buffer
          (let ((index 0))
            (catch 'found
              (dolist (item asm)
                (setq index (1+ index))
                (when (equal line (asm-line-src-line (cadr item)))
                  (goto-char (point-min))
                  (forward-line (1- index))
                  (debug-message "Index %d" index)
                  (throw 'found index)))))))))


(defun c-assembly-compile-and-show ()
  "Assembles and dump target file."
  (interactive)
  (let ((asm-buffer-name *base-asm-buffer-name*)
        (target-asm-file (c-assembly-get-target-asm))
        (base-asm-file (c-assembly-get-base-asm)))
    (c-assembly-compile)
    (c-assembly-dump-asm)
    
    (setq *loaded-asm* (diff (load-asm base-asm-file)
                             (load-asm target-asm-file)
                             #'asm-compare))
    (display-asm *loaded-asm*)))


(defun c-assembly-jump-to-line ()
  "Jump from the current line of C code to the corresponding assembly line."
  (interactive)
  (let ((line (line-number-at-pos))
        (asm-buffer-name *base-asm-buffer-name*))
    (switch-to-buffer-other-window (get-buffer asm-buffer-name))
    (jump-to-asm *loaded-asm* line)))

;; Enable the minor mode for C mode
(add-hook 'c-mode-hook (lambda () (decomp-mode 1)))
