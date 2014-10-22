;; common lisp standart functions: loop etc.
(require 'cl)
(require 'assoc)


(setq tramp-default-method "ssh")


;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)


;; start with single window
(add-hook 'emacs-startup-hook 'delete-other-windows)


;; Load CEDET, required for ECB
(load-file "~/cedet-1.1/common/cedet.el")
;; ;;(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;; ;;(global-srecode-minor-mode 1)            ; Enable template insertion menu
(global-semantic-idle-scheduler-mode 1)
(global-cedet-m3-minor-mode 1)


(add-to-list 'load-path
	     "~/ecb-2.40")
(setq stack-trace-on-error t) 

(require 'ecb)
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
(setq ecb-tip-of-the-day nil)
(setq ecb-ping-program "ssh")
(setq ecb-ping-options (list "HOST" "ping" "-c" "1" "localhost"))
(setq ecb-source-path (list (list "/" "THIS-MACHINE") (list "/ap:/" "REMOTE-AP")))
;;(setq desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\)")
(setq desktop-files-not-to-save "^$")
(ecb-activate)


;;things that need additional packages
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") )
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/" ))
  (setq package-enable-at-startup nil)
  (package-initialize)
)


;;moving emacs own buffers around the screen
(require 'buffer-move)


;;paranteses aroud cursor position, if any
(require 'highlight-parentheses)
(global-highlight-parentheses-mode 1)


;;presere opened buffers on emacs restarts
(desktop-save-mode 1)


;;mouse and scroll fixes including console emacs
(xterm-mouse-mode 1)
(mouse-wheel-mode 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 't) 
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


;;colors, fonts and themes
(load-theme 'wombat 't)
;;font size
(set-face-attribute 'default nil :height 130)
;;show line numbers with fixed size
(global-linum-mode 1)
(set-face-attribute 'linum nil :height 110)
(setq-default left-fringe-width  10)
(setq-default right-fringe-width  0)


;;replace selected text
(delete-selection-mode 1)


;;no useless start buffer
(setq inhibit-splash-screen t)


;;thin cursor istead of rectangle
(setq-default cursor-type 'bar)


;;more compact appearing
;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;start with fullscreen window
(defun toggle-fullscreen-linux()
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  )

(defun toggle-fullscreen-windows()
  (w32-send-sys-command 61488)
  )

(defun toggle-fullscreen-mac()
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'maximized))
  )

(defun toggle-fullscreen ()
  (interactive)
  (cond
   ((eq system-type 'gnu/linux) (toggle-fullscreen-linux))
   ((eq system-type 'windows-nt) (toggle-fullscreen-windows))
   ((eq system-type 'darwin) (toggle-fullscreen-mac))
   )
)

(toggle-fullscreen)


;; cofig file easy access
(defun open-config()
  (interactive)
  (find-file "~/.emacs")
  (end-of-buffer))

(setq vc-follow-symlinks t)


;;compile with Makefile from any place
(defun save-all-and-compile()
  (interactive)
  (save-some-buffers 1)
  (compile "runcompile.sh")) ;custom script


;;smart filenames autocompletions
(require 'ido)
(ido-mode t)
(ido-mode 'both)

(setq 
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\\*"))


;;trick to use C- and M- with russian
(loop
 for from across "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
 for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
 do
 (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat     "C-" (string to)))))
 (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat     "M-" (string to))))))


;;goto visible bufer with <S-arrow>
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;;shortcut to copy line but not kill it
(defun quick-copy-line ()
  "Copy the whole line that point is on and move to the beginning of the next line.
    Consecutive calls to this command append each line to the
    kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
	(end (line-beginning-position 2)))
    (if (eq last-command 'quick-copy-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end))))
  (beginning-of-line 2))


;; keybindings
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "C-z") 'undo)
(define-key my-keys-minor-mode-map (kbd "C-d") 'backward-kill-word) 
(define-key my-keys-minor-mode-map (kbd "C-9") 'previous-buffer)
(define-key my-keys-minor-mode-map (kbd "C-0") 'next-buffer)
(define-key my-keys-minor-mode-map (kbd "C-7") 'hs-hide-block)
(define-key my-keys-minor-mode-map (kbd "C-8") 'hs-show-block)
(define-key my-keys-minor-mode-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key my-keys-minor-mode-map (kbd "C-=") 'text-scale-increase)
(define-key my-keys-minor-mode-map (kbd "C--") 'text-scale-decrease)
(define-key my-keys-minor-mode-map (kbd "C-\\") 'bookmark-bmenu-list)
(define-key my-keys-minor-mode-map (kbd "C-M-f") 'rgrep)
;;(define-key my-keys-minor-mode-map (kbd "M-g") 'gtags-find-tag)
;;(define-key my-keys-minor-mode-map (kbd "M-*") 'gtags-pop-stack)
(define-key my-keys-minor-mode-map (kbd "M-o") 'ido-find-file)
(define-key my-keys-minor-mode-map (kbd "M-R") 'replace-string)
;;(define-key my-keys-minor-mode-map (kbd "M-@") 'copy-word)
;;(define-key my-keys-minor-mode-map (kbd "M-i") 'occur)
(define-key my-keys-minor-mode-map (kbd "M-m") 'imenu-make-selection-buffer)
(define-key my-keys-minor-mode-map (kbd "C-<backspace>") 'pop-global-mark)

(define-key my-keys-minor-mode-map (kbd "<f3>") 'semantic-ia-fast-jump)
(define-key my-keys-minor-mode-map (kbd "<f4>") 'ff-find-other-file)
(define-key my-keys-minor-mode-map (kbd "<f5>") 'save-buffer)
(define-key my-keys-minor-mode-map (kbd "C-<f5>") 'revert-buffer-no-confirm)
(define-key my-keys-minor-mode-map (kbd "<f7>") 'save-all-and-compile)
(define-key my-keys-minor-mode-map (kbd "<f11>") 'bookmark-set)
(define-key my-keys-minor-mode-map (kbd "<f12>")     'open-config)

(define-key my-keys-minor-mode-map (kbd "M-q") 'save-buffers-kill-terminal)

(define-key my-keys-minor-mode-map (kbd "<tab>") 'ido-switch-buffer)
;;(define-key my-keys-minor-mode-map (kbd "C-c f") 'iy-go-to-char)
(define-key my-keys-minor-mode-map (kbd "<delete>") 'delete-char)



;;my keybindings
(define-key my-keys-minor-mode-map (kbd "M-k") 'kill-this-buffer)
(define-key my-keys-minor-mode-map (kbd "C-~") 'toggle-fullscreen)
(define-key my-keys-minor-mode-map (kbd "C-`") 'ecb-toggle-ecb-windows)
(define-key my-keys-minor-mode-map (kbd "C-S-k") 'quick-copy-line)

;;manual indentations
(define-key my-keys-minor-mode-map (kbd "<C-tab>")     'python-indent-shift-right)
(define-key my-keys-minor-mode-map (kbd "<backtab>")   'python-indent-shift-left)
(define-key my-keys-minor-mode-map (kbd "<C-S-iso-lefttab>")   'python-indent-shift-left)

(define-key my-keys-minor-mode-map  (kbd "C-,")   'buf-move-left)
(define-key my-keys-minor-mode-map  (kbd "C-.")  'buf-move-right)
(define-key my-keys-minor-mode-map (kbd "M-,") 'windmove-left)  
(define-key my-keys-minor-mode-map (kbd "M-.") 'windmove-right) 


(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(defun my-minibuffer-setup-hook ()  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
