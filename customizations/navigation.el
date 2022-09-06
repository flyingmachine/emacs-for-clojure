;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.

;; which-key is the best feature for the discoverability and
;; usability of Emacs. When you start a key sequence, e.g. C-x,
;; a menu opens up that shows you what all your next options
;; are. It's a great way to find out what's in Emacs, and it
;; helps transfer commands from your short-term memory to
;; your long-term memory and (finally) your muscle memory.
(setup (:package which-key)
  (which-key-mode)
  (:option which-key-idle-delay 0.3))

;; ivy is the completion framework. This makes M-x much
;; more usable.
;; installing counsel brings ivy and swiper as dependencies
;; swiper is a powerful search-with-a-buffer capability.
;; https://github.com/abo-abo/swiper
(setup (:package counsel)
  (ivy-mode)
  (:option ivy-use-virtual-buffers t
           ivy-re-builders-alist '((t . ivy--regex-ignore-order))
           ivy-count-format "%d/%d ")
  (:global "C-s" swiper
           "s-f" swiper
           "C-x C-f" counsel-find-file
           "C-x C-b" counsel-switch-buffer
           "M-x" counsel-M-x))

(setup (:package ivy-rich))
