## Jitte
A major mode for `jj`. Currently implemented on top of `magit` primitives for rapid prototyping. Standalone comming soon(tm) (maybe never!). I'm adding features as I need them to get my work done. At the moment, this package is for me.

Currently only works in colocated repos as it depends on `magit`, sorry. 

> [!WARNING]  
> Severely incomplete

```
       
    │        _  _ _____ _____ _____
    │       / |/ Y__ __Y__ __Y  __/
    │ │     | || | / \   / \ |  \  
    │ ○  /\_| || | | |   | | |  /_ 
    ├─╯  \____/\_/ \_/   \_/ \____\
    │ 
     (get it? cause it looks like... nevermind...)
```

### Doom config

Here's what I do
``` elisp
(package! jitte
  :recipe (:host github :repo "snystrom/jitte")
  )
  
(map! :leader
      :desc "jj-log-default"
      "j j" #'jj-log-default
      )
```


The entrypoint to most actions is `jj-log-default`. Right now there is some jank, so you might have to call `jj-log` first.

The keybinds while you're in there...
``` elisp
  "RET" ; view commit at point
  "l"   ; modify log view
  "e"   ; edit commit at point
  "c"   ; `jj commit` on selected changes (edit description, add new commit on top)
  "d"   ; show diff at point
  "D"   ; edit commit description at point
  "n"   ; create new commit at point
  "u"   ; undo last op
  "R"   ; interactive rebase 
  "r"   ; rebase selected commit
  "g"   ; refresh the log buffer
  "q"   ; quit log buffer
```
