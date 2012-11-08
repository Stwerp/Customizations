;;; Stewart customizations :: 
(setq sentence-end-double-space nil) ;period single space ends sentence

;;; Auctex items
(setq-default TeX-master nil) ; Query for master file
(setq TeX-parse-self t)
(setq TeX-auto-save t)

;; Stewart -- Feb 14 2012
;; open *help* in current frame (not a new frame/"window")
(setq special-display-regexps (remove "[ ]?\\*[hH]elp.*" special-display-regexps))

;; This should work, but the OSX system emacs (22) will not evaluate the 'emacs-major-version' command for some strange reason. So... see the hack below :(
;;(cond
;; ((>= 23 emacs-major-version)
;;  ;; evaluate if version is 23 or greater
;;  (global-visual-line-mode 1)
;;  ))

;; check if we are not running the system (v22) emacs version so we can enable global-visual-line mode
(if (not (string-match "ghosttown.apple.com" (emacs-version)))
    (global-visual-line-mode 1)
  )



;; <Window Registers>
;; < author: Travis Deyle (beambot@gmail.com) >
;; Miscellaneous Notes:
;; For emacs-x use 'C-x r w <reg>' to create registers (window layouts)
;;     'C-x r j <reg>' to restore register (window layout)

(defun save-window (arg)
  (interactive)
  (window-configuration-to-register arg )
  (message "Window configuration saved in register '%c'." arg))

(defun restore-window (arg)  ;; call using (restore-window ?i)
  (interactive)
  (jump-to-register arg)
  (message "Window configuration restored to register '%c'." arg))

(set-register ?i ?j)

(defun cycle-window (reg win)
  (interactive)
  (save-window (get-register ?i))
  (if (get-register reg)
      (restore-window reg)
    (set-register reg (get-register (get-register ?i))))
  (message "Changing to window register '%c'." reg )
  (setq frame-title-format (format "Window '%d'" win))
  (set-register ?i reg))

(global-set-key (kbd "\C-c 1") (lambda () (interactive) (cycle-window ?j 1))) (save-window ?j)
(global-set-key (kbd "\C-c 2") (lambda () (interactive) (cycle-window ?k 2))) (save-window ?k)
(global-set-key (kbd "\C-c 3") (lambda () (interactive) (cycle-window ?l 3))) (save-window ?l)
(global-set-key (kbd "\C-c 4") (lambda () (interactive) (cycle-window ?m 4))) (save-window ?m)

;; </ Window Registers>


;; --------------------
;; Git support -- I don't have this
;; --------------------
;; (load "/usr/share/doc/git-core/contrib/emacs/git.el")

;; Since I don't use 'column fill', set C-x f to open file the curser is on
(global-set-key (kbd "C-x f") 'find-file-at-point)

;; Auctex Preview stuff -- Good luck!
;; ;; (load "auctex.el" nil t t)
;; ;; (load "preview-latex.el" nil t t)
;; Open a LaTeX file in Emacs and press
;;    C-c C-p C-b (M-x preview-buffer) 
;; to generate math/graphics previews for the entire buffer. After you update an equation, press
;;    C-c C-p C-p (M-x preview-at-point)
;; to refresh the preview.

;; -------------------------
;; Don't have to use M-x to run commands
;; -------------------------
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Re-bind some of the word kill commands
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
