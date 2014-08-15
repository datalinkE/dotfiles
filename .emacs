;;common lisp standart functions: loop etc.
(require 'cl)

;;presere opened buffers on emacs restarts
(desktop-save-mode 1)

;;mouse and scroll fixes
(xterm-mouse-mode 1)
(mouse-wheel-mode 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 't) 
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;show line numbers
(global-linum-mode 1)
(set-face-attribute 'linum nil :height 100)
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

;;font size
(set-face-attribute 'default nil :height 120)

;;start with fullscreen window
(defun toggle-fullscreen ()
  (interactive)
  (cond
   ((eq system-type 'gnu/linux)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			   '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			   '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
    )
   ((eq system-type 'windows-nt)
    (w32-send-sys-command 61488)
    )
   )
)

(toggle-fullscreen)

(defun open-config()
  (interactive)
  (find-file "~/.emacs")
  (end-of-buffer))

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
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
     "^\*compilation" "^\*GTAGS" "^session\.*" "^\*"))(require 'ido)

;;dired fixes and workarounds
(require 'dired )
(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (dired-find-alternate-file "..")))  ; was dired-up-directory
(defun pal-mouse-find-alternate-file (event)
  "In Dired, visit the file or directory name you click on."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos (posn-point (event-end event)))
      (if (not (windowp window))
          (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    (find-alternate-file file)))

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key [down-mouse-1] 'pal-mouse-find-alternate-file)))

;;trick to use C- and M- with russian
(loop
 for from across "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
 for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
 do
 (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat     "C-" (string to)))))
 (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat     "M-" (string to))))))
(put 'dired-find-alternate-file 'disabled nil)

;;more comfortable buffer selection
(require 'bs)

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

;;enabling of package manager
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") )
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/" ))
  (setq package-enable-at-startup nil)
  (package-initialize)
  
  ;;moving emacs own buffers around the screen
  (require 'buffer-move)

  ;;version control
  (require 'magit)

  ;;cmake-project
  ;; (require 'cmake-project)
  ;; (defun maybe-cmake-project-hook ()
  ;;   (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
  ;; (add-hook 'c-mode-hook 'maybe-cmake-project-hook)
  ;; (add-hook 'c++-mode-hook 'maybe-cmake-project-hook)

  ;;paranteses aroud cursor position, if any
  (require 'highlight-parentheses)
  (highlight-parentheses-mode 1)
 
;;
  (require 'sublime-themes)
  (load-theme 'wombat 't)
)

;;sgalustyan keybindings
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "C-z") 'undo)
(define-key my-keys-minor-mode-map (kbd "M-<f4>") 'save-buffers-kill-terminal)
(define-key my-keys-minor-mode-map (kbd "C-d") 'backward-kill-word)
(define-key my-keys-minor-mode-map (kbd "M-,") 'windmove-left)  
(define-key my-keys-minor-mode-map (kbd "M-.") 'windmove-right) 
;(define-key my-keys-minor-mode-map (kbd "C-M-p") 'windmove-up)    
;(define-key my-keys-minor-mode-map (kbd "C-M-[") 'windmove-down)   
(define-key my-keys-minor-mode-map (kbd "C-9") 'previous-buffer)
(define-key my-keys-minor-mode-map (kbd "C-0") 'next-buffer)
(define-key my-keys-minor-mode-map (kbd "C-7") 'hs-hide-block)
(define-key my-keys-minor-mode-map (kbd "C-8") 'hs-show-block)
(define-key my-keys-minor-mode-map (kbd "<f7>") 'save-all-and-compile)
(define-key my-keys-minor-mode-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key my-keys-minor-mode-map (kbd "C-=") 'text-scale-increase)
(define-key my-keys-minor-mode-map (kbd "C--") 'text-scale-decrease)
(define-key my-keys-minor-mode-map (kbd "C-\\") 'bookmark-bmenu-list)
(define-key my-keys-minor-mode-map (kbd "C-<f5>") 'revert-buffer-no-confirm)
(define-key my-keys-minor-mode-map (kbd "C-M-f") 'rgrep)
(define-key my-keys-minor-mode-map (kbd "M-o") 'ff-find-other-file)
(define-key my-keys-minor-mode-map (kbd "M-k") 'ido-kill-buffer)
(define-key my-keys-minor-mode-map (kbd "M-g") 'gtags-find-tag)
(define-key my-keys-minor-mode-map (kbd "M-*") 'gtags-pop-stack)
(define-key my-keys-minor-mode-map (kbd "M-O") 'ido-find-file)
(define-key my-keys-minor-mode-map (kbd "M-R") 'replace-string)
(define-key my-keys-minor-mode-map (kbd "M-@") 'copy-word)
(define-key my-keys-minor-mode-map (kbd "M-i") 'occur)
(define-key my-keys-minor-mode-map (kbd "M-m") 'imenu-make-selection-buffer)
;(define-key my-keys-minor-mode-map (kbd "M-n") 'speedbar)
(define-key my-keys-minor-mode-map (kbd "C-<backspace>") 'pop-global-mark)
(define-key my-keys-minor-mode-map (kbd "<f5>") 'kmacro-end-and-call-macro)
(define-key my-keys-minor-mode-map (kbd "<f2>") 'save-buffer)
;(define-key my-keys-minor-mode-map (kbd "<tab>") 'ido-switch-buffer)
;(define-key my-keys-minor-mode-map (kbd "<f12>") 'open-init-el)
(define-key my-keys-minor-mode-map (kbd "<f11>") 'bookmark-set)
(define-key my-keys-minor-mode-map (kbd "C-c f") 'iy-go-to-char)
(define-key my-keys-minor-mode-map (kbd "<delete>") 'delete-char)


;;my keybindings

;define-key my-keys-minor-mode-map (kbd "<f7>")  'compile)
(define-key my-keys-minor-mode-map (kbd "<f12>")     'open-config)
(define-key my-keys-minor-mode-map (kbd "C-`") 'toggle-fullscreen)
(define-key my-keys-minor-mode-map (kbd "C-x C-b") 'bs-show)
(define-key my-keys-minor-mode-map (kbd "C-S-k") 'quick-copy-line)
;;python indentations
(define-key my-keys-minor-mode-map (kbd "<C-tab>")     'python-indent-shift-right)
(define-key my-keys-minor-mode-map (kbd "<C-S-iso-lefttab>")   'python-indent-shift-left)

(define-key my-keys-minor-mode-map  (kbd "C-,")   'buf-move-left)
(define-key my-keys-minor-mode-map  (kbd "C-.")  'buf-move-right)


(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(defun my-minibuffer-setup-hook ()  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
