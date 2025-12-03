# Anzu Overlay

![Anzu Overlay](https://raw.githubusercontent.com/kwrooijen/anzu-overlay/refs/heads/master/anzu-overlay.png)


### Install

use-package
```
(use-package anzu-overlay
  :straight (anzu-overlay :type git :host github :repo "kwrooijen/anzu-overlay")
  :init
  (require 'anzu)
  (require 'evil)
  (require 'iedit)
  (anzu-overlay-mode 1))
```

[wisdom](https://github.com/kwrooijen/wisdom)

```org
* Anzu Overlay
:PROPERTIES:
:PACKAGE: anzu-overlay
:STRAIGHT: (anzu-overlay :type git :host github :repo "kwrooijen/anzu-overlay")
:END:

** Init :init:

Require packages needed by Anzu Overlay

#+BEGIN_SRC emacs-lisp
(require 'anzu)
(require 'evil)
(require 'iedit)
#+END_SRC

Enable Anzu Overlay mode, globally

#+BEGIN_SRC emacs-lisp
(anzu-overlay-mode 1)
#+END_SRC
```

### Customize face

```
(set-face-attribute 'anzu-overlay-face nil
  :background "#000000"
  :foreground "#ffffff")
```
