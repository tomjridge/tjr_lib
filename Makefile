SHELL:=bash

all:
	$(MAKE) -C src

install: all
	$(MAKE) -C src install

uninstall: 
	$(MAKE) -C src uninstall

clean: 
	$(MAKE) -C src clean 
