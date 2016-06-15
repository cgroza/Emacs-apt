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
* apt-mode-change-log
* apt-mode-depends
* apt-mode-dotty
* apt-mode-download
* apt-mode-install
* apt-mode-madison
* apt-mode-pkgnames
* apt-mode-policy
* apt-mode-purge
* apt-mode-rdepends
* apt-mode-remove
* apt-mode-search
* apt-mode-showpkg
* apt-mode-source
* apt-mode-xvcg

###apt-* commands
* apt-autoremove
* apt-changelog 
* apt-depends 
* apt-dotty 
* apt-download 
* apt-dump 
* apt-dumpavail 
* apt-install
* apt-madison
* apt-pkgnames 
* apt-policy 
* apt-purge
* apt-rdepends 
* apt-remove
* apt-search 
* apt-showpkg 
* apt-showsrc 
* apt-source 
* apt-stats 
* apt-update
* apt-upgrade
* apt-xvcg 
