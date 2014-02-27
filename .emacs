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
      (setq matlab-shell-command-switches '("-maci -nodesktop -nosplash"))
      (matlab-cedet-setup)
      (autoload 'mlint-minor-mode "mlint" nil t)
      (add-hook 'matlab-mode-hook (lambda () (mlint-minor-mode 1)))

      ;; Start emacsclient server
      (server-start)
      )


  ;;(Non v24 code here
)
;; Load spice mode
(setq load-path (cons (expand-file-name "~/.emacs.d/elisp/spice-mode/") load-path))
(autoload 'spice-mode "spice-mode" "Spice/Layla Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.cir$"          . spice-mode))
(add-to-list 'auto-mode-alist '("\\.ckt$"          . spice-mode))
(add-to-list 'auto-mode-alist '("\\.inp$"          . spice-mode))
(add-to-list 'auto-mode-alist '("\\.spout$"        . spice-mode));hspice out
(add-to-list 'auto-mode-alist '("\\.pdir$"         . spice-mode))
;;; Intel formats
(add-to-list 'auto-mode-alist '("\\.[sS][pP]$"     . spice-mode))
(add-to-list 'auto-mode-alist '("\\.[sm]?t0$"      . spice-mode))
(add-to-list 'auto-mode-alist '("\\.[h]?spice$"    . spice-mode))
;;; ;;; ;;; ;;; ;;; ;;; ;;;
;;; Stewart customizations :: 
(setq sentence-end-double-space nil) ;period single space ends sentence

;;; Auctex items
(setq-default TeX-master nil) ; Query for master file
(setq TeX-parse-self t)
(setq TeX-auto-save t)
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list '("Make" "make" TeX-run-command nil t) t))
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list '("DVI->PDF" "simpdftexnodel latex --maxpfb --extratexopts '-file-line-error -synctex=1' %t" TeX-run-command nil t :help "Simpdftex") t))
; Turn on RefTeX for AUCTeX, http://www.gnu.org/s/auctex/manual/reftex/reftex_5.html
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
; Make RefTeX interact with AUCTeX, http://www.gnu.org/s/auctex/manual/reftex/AUCTeX_002dRefTeX-Interface.html
(setq reftex-plug-into-AUCTeX t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-view-program-list (quote (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b") ("Preview" "open -a Preview.app %o"))))
 '(TeX-view-program-selection (quote ((output-pdf "Skim") ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Evince") (output-html "xdg-open"))))
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(display-time-mode t)
 '(global-hl-line-mode t)
 '(indicate-empty-lines t)
 '(matlab-shell-command-switches (quote ("-nodesktop -nosplash")) t)
 '(matlab-show-mlint-warnings t)
 '(mlint-programs (quote ("mlint")))
 '(show-paren-mode t))
(setq TeX-source-correlate-method 'synctex)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

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


;;(global-set-key (kbd "\C-x x") (lambda () (interactive) (delete-region (region-beginning) (region-end))))

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
;; And ignore the horrib mail thing.
(global-unset-key (kbd "\C-x m" ))

;; Re-bind some of the word kill commands
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Add Shift + Arrows for window switching
(windmove-default-keybindings)
;; Added for terminal sessions that do not allow modified arrow keys
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
;; Turn on rectangle selection
;; Turn on cua mode (I think?)
;; (cua-mode) ;; Do this manually, it overwrites many keys!
(global-set-key (kbd "C-c RET") 'cua-set-rectangle-mark)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; M-Tab to perform command completion (instead of flyspell)
;; (setq flyspell-use-meta-tab nil)

;; Turn on Interactive DO mode (id0)
(ido-mode t)


;;; Emacs 24 Customizations addd in

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray20" :foreground "white smoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "apple" :family "Monaco")))))

;; Make sure Emacs has the same PATH as a regular terminal/shell
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ;; Added by SJT
    (setq exec-path (split-string path-from-shell path-separator))))
;; Also, set up mlint paths for matlab               
(setenv "matlabroot" "/Applications/MATLAB_R2012b.app")
(setenv "LD_LIBRARY_PATH" "/usr/local/lib:${matlabroot}/bin/:${matlabroot}/runtime/maci64:${matlabroot}/sys/os/maci64:${matlabroot}/bin/maci64" t)
(setenv "DYLD_FALLBACK_LIBRARY_PATH" "/usr/local/lib:/lib:/usr/lib:${matlabroot}/bin/maci64" t)

(if window-system (set-exec-path-from-shell-PATH))

;; Some other aliases
;;(defalias 'open 'find-file)
;;(defalias 'emacs 'find-file)

;; Spell checking -- requires cocoAspell
(if (eq system-type 'darwin)
     (if (file-executable-p "/usr/local/bin/aspell")
     (progn
       (setq ispell-program-name "/usr/local/bin/aspell")
       (setq ispell-extra-args '("-d" "/Library/Application Support/cocoAspell/aspell6-en-6.0-0/en.multi"))
       )))


;; Show line numbers
;;(global-linum-mode t)

;; Join lines
(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

(add-hook 'doc-view-mode-hook 'auto-revert-mode)


;; Add julia mode
(add-to-list 'load-path "~/.emacs.d/elisp/julia-mode/")
(require 'julia-mode)
