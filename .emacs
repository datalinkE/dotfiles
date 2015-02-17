;; common lisp standart functions: loop etc.
(require 'cl)

(setq-default tramp-default-method "ssh")

;;presere opened buffers on emacs restarts
(setq-default
 desktop-dirname             "~/.emacs.d/desktop/"
 desktop-base-file-name      "emacs.desktop"
 desktop-base-lock-name      "lock"
 desktop-path                (list desktop-dirname)
 desktop-save                t
 desktop-files-not-to-save   "^$" ;reload tramp paths
 desktop-load-locked-desktop nil
 desktop-restore-eager       5)

(desktop-save-mode 1)

;; own keymap for use in this file
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-key my-keys-minor-mode-map [left] 'undefined)
(define-key my-keys-minor-mode-map [right] 'undefined)
(define-key my-keys-minor-mode-map [up] 'undefined)
(define-key my-keys-minor-mode-map [down] 'undefined)

;; start with single window
(add-hook 'emacs-startup-hook 'delete-other-windows)


;;things that need additional packages
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") )
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/" ))
  (setq-default package-enable-at-startup nil)
  (package-initialize)
  )



;; file navigation

;; (require 'sr-speedbar)
;; (setq-default speedbar-use-images nil)
;; (setq-default speedbar-show-unknown-files 1)
;; (setq-default sr-speedbar-width 50)
;; (setq-default sr-speedbar-auto-refresh t)
;; (sr-speedbar-open)

(require 'neotree)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-change-root)
            (define-key evil-normal-state-local-map (kbd "SPC") 'push-button)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'push-button)))


;;paranteses aroud cursor position, if any
(require 'highlight-parentheses)
(global-highlight-parentheses-mode 1)


;; autoclose braces
(require 'smartparens-config)
(sp-pair "'" nil :actions :rem)
(sp-pair "`" nil :actions :rem)
(sp-pair "\"" nil :actions :rem)
;(sp-autoescape-string-quote 0)
(setq-default sp-autoescape-string-quote nil)
(smartparens-global-mode t)

;; go-to symbol support (code navigation)
(require 'ggtags)
(setq-default ggtags-auto-jump-to-match nil)
(add-hook 'prog-mode-hook 'ggtags-mode)


;; vim-like navigation + emacs bindings in insert mode
(require 'evil)
;; remove all keybindings from insert-state keymap
(setcdr evil-insert-state-map nil)
;; but [escape] should switch back to normal state
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
(define-key evil-motion-state-map [left] 'undefined)
(define-key evil-motion-state-map [right] 'undefined)
(define-key evil-motion-state-map [up] 'undefined)
(define-key evil-motion-state-map [down] 'undefined)

(evil-mode 1)

;; company mode
(require 'company)
(add-to-list 'load-path "~/.emacs.d")
(autoload 'gtags-mode "gtags" "" t)

(require 'company-gtags)
(setq-default company-backends '(company-elisp
                         company-ropemacs
                         ;company-gtags
                         (company-dabbrev-code
                         company-keywords)
                         company-files
                         company-dabbrev))

(add-hook 'after-init-hook 'global-company-mode)

(define-key my-keys-minor-mode-map (kbd "C-SPC") 'company-complete)

(defun complete-or-indent ()
    (interactive)
    (if (company-manual-begin)
        (company-complete-common-or-cycle)
      (indent-according-to-mode)))

;(define-key my-keys-minor-mode-map (kbd "<tab>") 'complete-or-indent)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)


;; mouse and scroll fixes including console emacs
(xterm-mouse-mode 1)
(mouse-wheel-mode 1)
(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq-default mouse-wheel-progressive-speed 't)
(setq-default mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq-default scroll-step 1) ;; keyboard scroll one line at a time


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
(setq-default inhibit-splash-screen t)


;;thin cursor istead of rectangle
(setq-default cursor-type 'bar)


;;more compact appearing
;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; indentation in c/cpp/java etc.
(setq-default c-default-style "bsd"
      c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 120 4))

;; Set default tab space for various  python modes
(setq-default py-indent-offset 4)
(setq-default python-indent 4)
(setq-default python-indent-guess-indent-offset nil)


;; highlight incorrect blank symbols
(require 'whitespace)
(setq-default whitespace-style '(face tabs trailing lines space-before-tab newline indentation empty space-after-tab tab-mark))
(setq-default whitespace-line-column 160)
(global-whitespace-mode t)


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

(setq-default vc-follow-symlinks t)


;;compile with Makefile from any place
(defun save-all-and-compile()
  (interactive)
  (save-some-buffers 1)
  (compile "runcompile.sh")) ;custom script


;; smart filenames autocompletions
(require 'ido)
;(ido-mode t)
;(ido-mode 'both)
;;fuzzy-search for ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq-default ido-enable-flex-matching t)
(setq-default ido-use-faces nil)

(setq-default
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\\*"))

;; make ido display choices vertically
;(setq ido-separator "\n")
;; display any item that contains the chars you typed
(setq-default ido-enable-flex-matching t)

;;(require 'wgrep)

;;trick to use C- and M- with russian
(loop
 for from across "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
 for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
 do
 (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat     "C-" (string to)))))
 (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat     "M-" (string to))))))


;;goto visible bufer buferwith <S-arrow>
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )

(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe
         (lambda()
           (if (string= "shell-mode" major-mode)
               (progn (comint-next-prompt 25535) (yank))
             (progn (goto-char (mark)) (yank) )))))
    (if arg
        (if (= arg 1)
            nil
          (funcall pasteMe))
      (funcall pasteMe))
    ))

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

(defun quick-copy-word ()
  "Copy words at point into kill-ring"
  (interactive)
  (copy-thing 'backward-word 'forward-word)
  )

;; Shift the selected region right if distance is postive, left if
;; negative

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right ()
  (interactive)
  (shift-text 4))

(defun shift-left ()
  (interactive)
  (shift-text -4))

;; keybindings

(define-key my-keys-minor-mode-map (kbd "C-d") 'backward-kill-word)
(define-key my-keys-minor-mode-map (kbd "C-9") 'previous-buffer)
(define-key my-keys-minor-mode-map (kbd "C-0") 'next-buffer)
(define-key my-keys-minor-mode-map (kbd "C-7") 'hs-hide-block)
(define-key my-keys-minor-mode-map (kbd "C-8") 'hs-show-block)
(define-key my-keys-minor-mode-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key my-keys-minor-mode-map (kbd "C-=") 'text-scale-increase)
(define-key my-keys-minor-mode-map (kbd "C--") 'text-scale-decrease)
(define-key my-keys-minor-mode-map (kbd "C-\\") 'bookmark-bmenu-list)

(define-key my-keys-minor-mode-map (kbd "M-o") 'ido-find-file)
(define-key my-keys-minor-mode-map (kbd "M-R") 'replace-string)
(define-key my-keys-minor-mode-map (kbd "M-i") 'occur)
(define-key my-keys-minor-mode-map (kbd "M-m") 'imenu-make-selection-buffer)
(define-key my-keys-minor-mode-map (kbd "C-<backspace>") 'pop-global-mark)

(define-key my-keys-minor-mode-map (kbd "<f4>") 'ff-find-other-file)

(define-key my-keys-minor-mode-map (kbd "C-<f5>") 'revert-buffer-no-confirm)
(define-key my-keys-minor-mode-map (kbd "<f7>") 'save-all-and-compile)
(define-key my-keys-minor-mode-map (kbd "<f11>") 'bookmark-set)
(define-key my-keys-minor-mode-map (kbd "<f12>") 'open-config)

;;(define-key my-keys-minor-mode-map (kbd "C-c f") 'iy-go-to-char)
(define-key my-keys-minor-mode-map (kbd "<delete>") 'delete-char)

;;my keybindings
(define-key my-keys-minor-mode-map (kbd "M-k") 'kill-this-buffer)
(define-key my-keys-minor-mode-map (kbd "C-~") 'toggle-fullscreen)
(define-key my-keys-minor-mode-map (kbd "C-`") 'neotree-find)

;;manual indentations
(define-key my-keys-minor-mode-map (kbd "<C-tab>")     'shift-right)
(define-key my-keys-minor-mode-map (kbd "<backtab>")   'shift-left)
(define-key my-keys-minor-mode-map (kbd "<C-S-iso-lefttab>")   'shift-left)

;;(define-key my-keys-minor-mode-map  (kbd "M-[")   'buf-move-left)
;;(define-key my-keys-minor-mode-map  (kbd "M-]")  'buf-move-right)
(define-key my-keys-minor-mode-map (kbd "M-[") 'windmove-left)
(define-key my-keys-minor-mode-map (kbd "M-]") 'windmove-right)

;;traditional osx
(define-key my-keys-minor-mode-map (kbd "M-s") 'save-buffer)
(define-key my-keys-minor-mode-map (kbd "M-x") 'kill-region)
(define-key my-keys-minor-mode-map (kbd "M-c") 'kill-ring-save)
(define-key my-keys-minor-mode-map (kbd "M-v") 'yank)
(define-key my-keys-minor-mode-map (kbd "M-q") 'save-buffers-kill-terminal)
(define-key my-keys-minor-mode-map (kbd "M-z") 'undo)

(define-key my-keys-minor-mode-map (kbd "<f3>") 'execute-extended-command)
(define-key my-keys-minor-mode-map (kbd "C-/") 'rgrep)
(define-key my-keys-minor-mode-map (kbd "M-/") 'ggtags-grep)


(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(defun my-minibuffer-setup-hook ()  (my-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
(my-keys-minor-mode 1)

;; SHOW FILE PATH IN FRAME TITLE
;(setq-default frame-title-format "%b (%f)")
