[![MELPA](http://melpa.milkbox.net/packages/org-edit-latex-badge.svg)](http://melpa.milkbox.net/#/org-edit-latex)

## Org Edit LaTeX

org-edit-latex.el is an extension for org-mode. It let you edit a latex fragment/environment just like editing a src block.

![org-edit-latex](screenshot.gif)

### Why?
Embedded LaTeX is a nice feature of orgmode. Unlike LaTeX src block or export block, you can preview a LaTeX fragment by simply hit `C-c C-x C-l`. But it's lacking an important feature, i.e. it cannot be edited in a dedicated buffer like src block or export block do. This means you are isolated from all those nice features that you'll get by editing in a dedicated buffer, including syntax highlighting, auto-indent and completion. Without those, it's intimidating to write long math equations as a LaTeX fragment, at least for me.

So I write this package to address above issue. With this package, you can edit a LaTeX fragment just like editing a src block with all the nifty features provided by AucTeX, like completion, highlighting, and auto-indentation.

### Install

First, download this package and include its path in your `load-path`. Then, you can add following in your init file:

```
(require 'org-edit-latex)
```

And don't forget to add `latex` to your `org-babel-load-languages` (below is for demonstration, your languages list may differ from it.)

```
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)   ;; <== add latex to the list
   (python . t)
   (shell . t)
   (ruby . t)
   (perl . t)))
```

### How to use?
First, you should enable org-edit-latex before using it by <kbd>M-x org-edit-latex-mode</kbd>.


Then you can move cursor to the fragment you want to change and use `org-edit-special` (by default, it is bound to <kbd>C-c '</kbd>)to edit. When you are done editing, just exit the buffer with `org-edit-src-exit` (the keybinding is also <kbd>C-c '</kbd>). And yes, all those are built-in commands from orgmode. No extra keybindings to memorize!

When you don't need org-edit-latex anymore and want to revert to orgmode's default behavior, just <kbd>M-x org-edit-latex-mode</kbd> again.


### Change Log
- 0.6.2 src blocks will be untouched.
- 0.6.1 Fix inline src block not recognized.
- 0.6.0 Editing of inline latex (including non math latex fragments) is supported.
...

### TODO
- [x] Add support for inline math.
- [x] Turn this feature into a minor mode. (by purcell)
