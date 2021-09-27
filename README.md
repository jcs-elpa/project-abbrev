[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/project-abbrev-badge.svg)](https://melpa.org/#/project-abbrev)
[![MELPA Stable](https://stable.melpa.org/packages/project-abbrev-badge.svg)](https://stable.melpa.org/#/project-abbrev)

# project-abbrev
> Customize your own abbreviation expansion in the project.

[![CI](https://github.com/jcs-elpa/project-abbrev/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/project-abbrev/actions/workflows/test.yml)

<p align="center">
  <img src="./etc/custom-abbrev-demo.gif" with="600" height="264"/>
</p>

## Configuration

Name your own customize abbreviation config file. The default name
is `project-abbrev.config`.

```el
(setq project-abbrev-config-file "project-abbrev.config")
```

## Key Bindings

Complete the current word that point currently on.

```el
(define-key global-map (kbd "C-<return>") #'project-abbrev-complete-word)
```

## Config Example

This is example of the `project-abbrev.config` file. Notice this can be customize
in any programming language or even just a text file as long as you follow this format.

```ini
#
# Customize all the expand shortcut here.
#

# Java
sysout=System.out.println();
syserr=System.err.println();
```

## Contribution

If you would like to contribute to this project, you may either 
clone and make pull requests to this repository. Or you can 
clone the project and establish your own branch of this tool. 
Any methods are welcome!
