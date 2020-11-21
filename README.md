[![MELPA](https://melpa.org/packages/elscreen-tab-badge.svg)](https://melpa.org/#/elscreen-tab)

## elscreen-tab
Keep tabs of `elscreen` displayed on a dedicated window.

- If [elscreen](https://github.com/knu/elscreen) benefits you, but you have been annoyed by
header-line being used for elscreen, here's `elscreen-tab` for you.

### Demonstration
![demo](https://github.com/aki-s/elscreen-tab/blob/gh-pages/docs/images/elscreen-tab_demo.gif)

### Usage
```
(require 'elscreen)
(elscreen-start) ; Need to enable elscreen as a prerequisite of `elscreen-tab'.
(require 'elscreen-tab)
(elscreen-tab-mode)  ; Enable `elscreen-tab'.

(elscreen-tab-set-position 'right) ; Keep displaying on the right side.
(elscreen-tab-set-position 'top) ; Keep displaying on the top.
(elscreen-tab-set-position 'left) ; Keep displaying on the left side.
(elscreen-tab-set-position 'bottom) ; Keep displaying on the bottom.
(elscreen-tab-mode -1)  ; Disable `elscreen-tab'.
```

### Notes

- You can use any existing keybindings of elscreen, because
`elscreen-tab-mode` just keep displaying tabs of elscreen at the specified side.
- You may try `tab-bar-mode` as an alternative if you are using Emacs version 27.

