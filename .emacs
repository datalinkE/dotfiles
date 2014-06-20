;;common lisp standart functions: loop etc.
(require 'cl)

;;presere opened buffers on emacs restarts
(desktop-save-mode 1)

;;enable mouse in terminal
(xterm-mouse-mode 1)
(mouse-wheel-mode 1)

;;show line numbers
(global-linum-mode 1)

;;reload lisp file hotkey
(global-set-key (kbd "C-S-l")     'load-file)

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

;;smart filenames autocompletions
(require 'ido)
(ido-mode t)

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
(global-set-key (kbd "C-x C-b") 'bs-show)

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
(global-set-key (kbd "C-S-k") 'quick-copy-line)

;;enabling of package manager
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (setq package-enable-at-startup nil)
  (package-initialize)
  
  ;;moving emacs own buffers around the screen
  (require 'buffer-move)
  (global-set-key (kbd "<C-S-up>")     'buf-move-up)
  (global-set-key (kbd "<C-S-down>")   'buf-move-down)
  (global-set-key (kbd "<C-S-left>")   'buf-move-left)
  (global-set-key (kbd "<C-S-right>")  'buf-move-right)

  ;;version control
  (require 'magit)
  )
