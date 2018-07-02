# project-abbrev #

Customize your own abbreviation expansion in the project.<br/><br/>


## Configuration ##
Name your own customize abbreviation config file. The default name
is `project-abbrev.config`.
```
(setq project-abbrev-config-file "project-abbrev.config")
```


## Key Bindings ##
Complete the current word that point currently on.
```
(define-key global-map (kbd "C-<return>") #'project-abbrev-complete-word)
```


## Config Example ##
This is example of the `project-abbrev.config` file. Notice this can be customize
in any programming language or even just a text file as long as you follow this format.
```
#
# Customize all the expand shortcut here.
#

# Java
sysout=System.out.println();
syserr=System.err.println();
```


## Screenshot ##
<img src="./screenshot/custom-abbrev-demo.gif" with="600" height="264"/>


## Contribution ##
If you would like to contribute to this project. You may either
clone and make pull request to this repository. Or you can
clone the project and make your own branch of this tool. Any
methods are welcome!
