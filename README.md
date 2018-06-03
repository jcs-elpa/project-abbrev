# custom-abbrev #

Customize your own complete shortcut abbreviation in the project.<br/><br/>


## Configuration ##
Name your own customize complete config file. The default name
is `customize-complete.config`'.
```
(setq custom-abbrev-config-file "customize-shortcut.config")
```


## Key Bindings ##
Complete the current word that point currently on.
```
(define-key global-mode-map (kbd "C-S-o") #'custom-abbrev-swap-word)
```


## Screenshot ##


## Contribution ##
If you would like to contribute to this project. You may either
clone and make pull request to this repository. Or you can
clone the project and make your own branch of this tool. Any
methods are welcome!
