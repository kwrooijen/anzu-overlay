# Anzu Overlay

![Anzu Overlay](https://raw.githubusercontent.com/kwrooijen/anzu-overlay/refs/heads/master/anzu-overlay.png)


### Install

```
(use-package anzu-overlay
  :straight (anzu-overlay :type git :host github :repo "kwrooijen/anzu-overlay")
  :init (anzu-overlay-mode 1))
```

### Customize face

```
(set-face-attribute 'anzu-overlay-face nil
  :background "#000000"
  :foreground "#ffffff")
```
