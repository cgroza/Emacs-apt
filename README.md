Emacs-apt
=========
##Description
Emacs-apt is a project providing apt-mode in order to facilitate
tasks using apt-get and apt-cache. Currently, it does not support
operations that require root access. Others are implemented and commands
are available for them.

Emacs-apt allows you to simply execute a key sequence over a package
name and the respective apt-get or apt-cache command will be invoked,
displaying and highlighting the output.

For example, you could do apt-search package.
A buffer displaying the results of the search will be created.
You will be able to position your point over an element and 
run an operation such as apt-get download on it with a single
key sequence.

##Configuration 

Users should define their own keybindings. Note that apt-mode-* functions
execute their actions on a symbol at point, while apt-* functions ask the user
for a string in the minibuffer.

###apt-mode-* commands:
* apt-mode-search
* apt-mode-source
* apt-mode-pkgnames
* apt-mode-policy
* apt-mode-showpkg
* apt-mode-dotty
* apt-mode-xvcg
* apt-mode-change-log
* apt-mode-depends
* apt-mode-download
* apt-mode-rdepends
* apt-mode-madison
###apt-* commands
* apt-search 
* apt-download 
* apt-changelog 
* apt-source 
* apt-showpkg 
* apt-stats 
* apt-showsrc 
* apt-dump 
* apt-dumpavail 
* apt-depends 
* apt-rdepends 
* apt-pkgnames 
* apt-dotty 
* apt-xvcg 
* apt-policy 
* apt-madison 
