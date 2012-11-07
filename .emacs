;; Version Checking function (used in parts of this file)
;;  Returns t for version equal to or greater than arg
;;  Returns nil otherwise (version does not meet correct requirements
(defun version-check (arg)
  (string=
   (int-to-string arg)
   (substring (emacs-version) 10 12))
)

;; Load cedet (compiled only for v24, though!
(if (version-check 24)
    (progn
      ;; V24 code to execute
      (load-file "~/.emacs.d/elisp/cedet-1.1/common/cedet.el")
      (global-ede-mode t)
      (semantic-load-enable-code-helpers)
      (global-srecode-minor-mode 1)
      
      ;; Install matlab-emacs mode
      (add-to-list 'load-path "~/.emacs.d/elisp/matlab-emacs/")
      (require 'matlab-load)
      
      ;; Other Matlab mode commands
      (setq matlab-shell-command-switches '("-nodesktop -nosplash"))
      (matlab-cedet-setup)
      )
  ;;(Non v24 code here
)
;; Stewart -- Feb 14 2012
;; open *help* in current frame (not a new frame/"window")
(setq special-display-regexps (remove "[ ]?\\*[hH]elp.*" special-display-regexps))

;; This should work, but the OSX system emacs (22) will not evaluate the 'emacs-major-version' command for some strange reason. So... see the hack below :(
(if (not (version-check 22) )
    ;; Only set of v23 or higher
    (global-visual-line-mode 1))

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

;; -------------------------
;; Don't have to use M-x to run commands
;; -------------------------
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Re-bind some of the word kill commands
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Add Shift + Arrows for window switching
(windmove-default-keybindings)

;; M-Tab to perform command completion (instead of flyspell)
;; (setq flyspell-use-meta-tab nil)

;; Turn on Interactive DO mode (id0)
(ido-mode t)


;;; Emacs 24 Customizations addd in
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(display-time-mode t)
 '(global-hl-line-mode t)
 '(indicate-empty-lines t)
 '(mlint-programs (quote ("mlint" "mac/mlint" "maci64/mlint")))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Make sure Emacs has the same PATH as a regular terminal/shell
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ;; Added by SJT
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shell-PATH))

;; Some other aliases
;;(defalias 'open 'find-file)
;;(defalias 'emacs 'find-file)


;; Show line numbers
(global-linum-mode t)
