Emacs-apt
=========

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

