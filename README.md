## Org-LFE

Org-LFE (LaTeX Fragment Editor) is an extension for org-mode. It let you edit a latex fragment just like editing a src block.

![org-lfe](org-lfe.gif)

### Why?
LaTeX fragment is a nice feature of orgmode. Unlike LaTeX src block or export block, You can preview a fragment very easily by simply hit `C-c C-x C-l` when on one. But it's lacking an important feature, i.e. it cannot be edited in a dedicated buffer like src block or export block do. Without those nice features like syntax highlighting or auto-indent or completion that you'll usually get with a dedicated latex buffer, I used to find it's intimidating to write long math equations in a LaTeX fragment.

So I write this package to address above issue. Now I can edit a LaTeX fragment just like editing a src block with all the nifty features provided by AucTeX, like completion, highlighting, and auto-indentation.

### Install

First, download this package and include its path in your `load-path`. Then, you can add following in your init file:

```
(with-eval-after-load "org"
  (require 'org-lfe))
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
Just move the cursor to the fragment you want to change and use `org-edit-special` to edit. When you are done editing, just exit the buffer with `org-edit-src-exit`. And yes, all those commands are from orgmode. No extra keybindings to memorize!

### Caveat
Note that currently only latex environment or display math, i.e. latex fragments wrapped by `$$ ... $$` (double dollar), `\[ ... \]` and `\begin{} ... \end{}` are supported, since I don't think it's a good idea to use complicated inline equations and I want to keep this package simple. If you think different, please contact me.
