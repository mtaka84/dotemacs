(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; yes/no のかわりに y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; スタートアップスクリーンやめ
(setq inhibit-startup-screen t)

;;
;; ロードパスの追加
;;
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "elisp" "conf" "public_repos")


;;
;; キーバインド
;;

;; インデントを入れる
(global-set-key (kbd "C-m") 'newline-and-indent)
;; C-t でwindowを回る
(define-key global-map (kbd "C-t") 'other-window)
;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)


;; 文字コード
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; Mac向けファイル名の設定
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;; Windowsの場合のファイル名の設定
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;; Mac用フォント設定
;; http://tcnksm.sakura.ne.jp/blog/2012/04/02/emacs/

;; 英語
(set-face-attribute 'default nil
            :family "Menlo" ;; font
            :height 110)    ;; font size

;; 日本語
(set-fontset-font
 nil 'japanese-jisx0208
;; (font-spec :family "Hiragino Mincho Pro")) ;; font
  (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font

;; 半角と全角の比を1:2にする
(setq face-font-rescale-alist
;;        '((".*Hiragino_Mincho_pro.*" . 1.2)))
      '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)));; Mac用フォント設定


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; フレームに関する設定                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; カラム番号も表示
(column-number-mode t)
;; ファイルサイズを表示
(size-indication-mode t)
;; 時計を表示
(setq display-time-day-and-date t) ; 曜日・月・日を表示
(setq display-time-24hr-format t) ; 24時表示
(display-time-mode t)
;; リージョン内の行数と文字数をモードラインに表示する（範囲指定時のみ）
;; http://d.hatena.ne.jp/sonota88/20110224/1298557375
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
      ;; これだとエコーエリアがチラつく
      ;;(count-lines-region (region-beginning) (region-end))
    ""))

(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

;;; P90 タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;; 対応するカッコを目立たせる
(show-paren-mode t)

;; 行番号を常に表示する
(require 'linum)
(global-linum-mode t)
;;(setq linum-format "%5d")

;; 全角スペース タブ trailing-spacesを目立たせる
(require 'whitespace)
;; space-markとtab-mark、それからspacesとtrailingを対象とする
(setq whitespace-style '(space-mark tab-mark face spaces trailing))
(setq whitespace-display-mappings
      '(
        ;; (space-mark   ?\     [?\u00B7]     [?.]) ; space - centered dot
        (space-mark   ?\xA0  [?\u00A4]     [?_]) ; hard space - currency
        (space-mark   ?\x8A0 [?\x8A4]      [?_]) ; hard space - currency
        (space-mark   ?\x920 [?\x924]      [?_]) ; hard space - currency
        (space-mark   ?\xE20 [?\xE24]      [?_]) ; hard space - currency
        (space-mark   ?\xF20 [?\xF24]      [?_]) ; hard space - currency
        (space-mark ?\u3000 [?\u25a1] [?_ ?_]) ; full-width-space - square
        ;; NEWLINE is displayed using the face `whitespace-newline'
        ;; (newline-mark ?\n    [?$ ?\n])  ; eol - dollar sign
        ;; (newline-mark ?\n    [?\u21B5 ?\n] [?$ ?\n]); eol - downwards arrow
        ;; (newline-mark ?\n    [?\u00B6 ?\n] [?$ ?\n]); eol - pilcrow
        ;; (newline-mark ?\n    [?\x8AF ?\n]  [?$ ?\n]); eol - overscore
        ;; (newline-mark ?\n    [?\x8AC ?\n]  [?$ ?\n]); eol - negation
        ;; (newline-mark ?\n    [?\x8B0 ?\n]  [?$ ?\n]); eol - grade
        ;;
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t]) ; tab - left quote mark
        ))
;; whitespace-spaceの定義を全角スペースにし、色をつけて目立たせる
(setq whitespace-space-regexp "\\(\u3000+\\)")
(set-face-foreground 'whitespace-space "cyan")
(set-face-background 'whitespace-space 'nil)
;; whitespace-trailingを色つきアンダーラインで目立たせる
(set-face-underline  'whitespace-trailing t)
(set-face-foreground 'whitespace-trailing "cyan")
(set-face-background 'whitespace-trailing 'nil)
(global-whitespace-mode 1)

;; PageUpキーでPageup
(global-set-key [pageup] 'pager-page-up)
;; PageDownキーPageDown
(global-set-key [pagedown] 'pager-page-down)

;; system-type predicates
;; from http://d.hatena.ne.jp/tomoya/20090807/1249601308
(setq darwin-p   (eq system-type 'darwin)
      linux-p    (eq system-type 'gnu/linux)
      carbon-p   (eq system-type 'mac)
      meadow-p   (featurep 'meadow))

; Emacs と Mac のクリップボード共有
; from http://hakurei-shain.blogspot.com/2010/05/mac.html
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(if (or darwin-p carbon-p)
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.5インデントの設定                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABの表示幅。初期値は8
(setq-default tab-width 4)


;; C、C++、JAVA、PHPなどのインデント
(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-style "java")))


;;twittering mode
(setq twittering-icon-mode t)                ; Show icons
(setq twittering-use-icon-storage t)
(setq twittering-timer-interval 300)         ; Update your timeline each 300 seconds (5 minutes)


;;
;; Erlang
;;

;; erlang-mode
(setq load-path (cons  "/usr/local/Cellar/erlang/R16B03/lib/erlang/lib/tools-2.6.13/emacs"
      load-path))
      (setq erlang-root-dir "/usr/local/Cellar/erlang/R16B03")
      (setq exec-path (cons "/usr/local/Cellar/erlang/R16B03/bin" exec-path))
      (require 'erlang-start)
;;      (require 'erlang-flymake)

;; distel
(add-to-list 'load-path "/usr/local/share/distel/elisp")
(require 'distel)
(distel-setup)

;; Some Erlang customazations
(add-hook 'erlang-mode-hook
  (lambda ()
    (setq inferior-erlang-machine-options `("-sname" "masanori"))
    (imenu-add-to-menubar "imenu")))


;;
;; Auto Complete
;;

;; http://dev.ariel-networks.com/Members/matsuyama/auto-complete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/etc/ac-dict")
(global-auto-complete-mode t)

