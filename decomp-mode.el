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
(defvar *asm-buffer-name* "*Assembler*")

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
   (format "arm-none-eabi-as -mthumb -march=armv5te %s -o %s"
           src-file obj-file)))

(defun c-assembly-dump-asm ()
  "Assemble and dump target assembly."
  (let ((obj-file (c-assembly-get-target-obj))
        (asm-file (c-assembly-get-target-asm)))
    (c-assembly-assemble-file *src-target-file* obj-file)
    (c-assembly-dump-asm obj-file asm-file)))


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
          (message line))
         ((string-match "^[[:space:]]*\\([0-9a-f]+\\):[[:space:]]*\\(\\([a-z]+\\).*\\)$" line)
          ;; Instruction
          (setq result (nconc result
                              (list (make-instance 'asm-line
                                     :src-line source-ln
                                     :address (string-to-number (match-string 1 line) 16)
                                     :base-instruction (match-string 3 line)
                                     :full-instruction (match-string 2 line))))))))
      result)))

(defun display-asm (asm)
  "Display the assembly in a buffer."
  (let ((buffer (get-buffer-create *asm-buffer-name*)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (dolist (item asm)
        (insert (format "%s\n" (asm-line-full-instruction item))))
      (setq buffer-read-only t)
      (special-mode) ; Set a suitable mode for the buffer
      (goto-char (point-min))
      (display-buffer buffer))))



(defun jump-to-asm (asm line)
  (let ((buffer (get-buffer *asm-buffer-name*)))
    (if buffer
        (with-current-buffer buffer
          (let ((index 0))
            (catch 'found
              (dolist (item asm)
                (setq index (1+ index))
                (when (equal line (asm-line-src-line item))
                  (goto-char (point-min))
                  (forward-line index)
                  (message "Index %d" index)
                  (throw 'found index)))
              (message "Number %d not found in the list." line))))
    (message "Buffer %s does not exist." my-buffer-name))))


(defun c-assembly-compile-and-show ()
  "Assembles and dump target file."
  (interactive)
  (let ((asm-buffer-name *asm-buffer-name*)
        (asm-file (c-assembly-get-base-asm)))
    
    (let ((obj-file (c-assembly-get-base-obj))
          (asm-file (c-assembly-get-base-asm)))
      (c-assembly-compile-file (buffer-file-name) obj-file)
      (c-assembly-dump-asm-file obj-file asm-file))
    (setq *loaded-asm* (load-asm asm-file))
    (display-asm *loaded-asm*)))


(defun c-assembly-jump-to-line ()
  "Jump from the current line of C code to the corresponding assembly line."
  (interactive)
  (let ((line (line-number-at-pos))
        (asm-buffer-name *asm-buffer-name*))
    (switch-to-buffer-other-window (get-buffer asm-buffer-name))
    (jump-to-asm *loaded-asm* line)))

;; Enable the minor mode for C mode
(add-hook 'c-mode-hook (lambda () (decomp-mode 1)))
