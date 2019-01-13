<p align="left"><a href="http://www.gnu.org/licenses/gpl-3.0.txt"><img src="https://img.shields.io/badge/license-GPL_3-green.svg" alt="License GPL 3" /></a>
<a href="https://melpa.org/#/idle-org-agenda"><img alt="MELPA" src="https://melpa.org/packages/idle-org-agenda-badge.svg"/></a> </p>


# `idle-org-agenda`


Basically, if you don't touch Emacs `idle-org-agenda` will display your org-agenda after certain time.
That can be useful to remember tasks after come back to work.

This project is comes from John Wiegley's mail at the gmane mailing list
http://article.gmane.org/gmane.emacs.orgmode/23047



# Installation

## MELPA

  Just install the idle-org-agenda package!

## Manual

   Put `idle-org-agenda.el` in your load-path, and eval `(require 'idle-org-agenda')`.
   
## use-package

   Easy try with `use-package`

``` elisp
   (use-package idle-org-agenda
     :after org-agenda
     :ensure t
     :config (idle-org-agenda-mode))
```

# How it works

  Run `idle-org-agenda-mode` and don't touch the keyboard. :)

![](docs/idle-org-agenda.gif)

# Additional Configuration

You can define different agendas by using [org-agenda-custom-commands](https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html).


```emacs-lisp
(setq org-agenda-custom-commands
   (quote
    (("c" . "Categories")
     ("ce" "Emacs" tags-todo "emacs" nil)
     ("cs" "School" tags-todo "school" nil)
     ("cw" "work" tags-todo "work" nil)
     ("cb" "BLOG" tags-todo "blog" nil)
     ("ci" "Important" tags-todo "important" nil)
     ("w" "Weekly agenda" agenda "You can write here your custom agenda"))))
```


```emacs-lisp
(custom-set-variables
 '(idle-org-agenda-interval 600) ;; 10 x 60 seconds
 '(idle-org-agenda-key "cb")     ;; Choose your specific agenda
 '(idle-org-agenda-mode t))
```
