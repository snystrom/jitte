## Jitte
A major mode for `jj`.

Currently only works in colocated repos as it depends on `magit`, sorry.

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


The entrypoint to most actions is `jj-log-default`
