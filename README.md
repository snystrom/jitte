## Jitte
A major mode for `jj`.

Currently only works in colocated repos as it depends on `magit`, sorry.

```
            _  _ _____ _____ _____
           / |/ Y__ __Y__ __Y  __/
           | || | / \   / \ |  \  
    │   /\_| || | | |   | | |  /_ 
    │   \____/\_/ \_/   \_/ \____\
    │ │
    │ ○
    ├─╯
    │ (get it?)
```

### Doom config
Right now this package doesn't actually install properly, but you can clone the repo & load-file it. Yay *eyeroll*.

Here's what I do
``` elisp
(load-file "~/git/jitte/jj-describe.el")
(load-file "~/git/jitte/jj-log.el")

(map! :leader
      :desc "jj-log-default"
      "j j" #'jj-log-default
      )
```


The entrypoint to most actions is `jj-log-default`
