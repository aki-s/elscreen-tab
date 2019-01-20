## elscreen-tab
If you have benefits from [elscreen](https://github.com/knu/elscreen), but you worried about
header-line of a buffer is used for elscreen, here's `elscreen-tab` for you.

### Demonstration
![demo](https://github.com/aki-s/elscreen-tab/blob/gh-pages/docs/images/elscreen-tab_demo.gif)

### Usage
```
(require 'elscreen)
(elscreen-start)
(require 'elscreen-tab)
(elscreen-tab-mode)  ; Enable `elscreen-tab'.

(elscreen-tab:set-position 'right) ; Show on the right side.
(elscreen-tab:set-position 'top) ; Show at the top.
(elscreen-tab:set-position 'left) ; Show on the left side.
(elscreen-tab:set-position 'bottom) ; Show at the bottom.
(elscreen-tab-mode -1)  ; Disable `elscreen-tab'.
```

### Notes

- You can use any existing keybindings of elscreen, because
This mode just shows the dedicated buffer which displays tabs of elscreen.
