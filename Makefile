SHELL:=bash

build:
	$(MAKE) -C src build

install: 
	$(MAKE) -C src install

uninstall: 
	$(MAKE) -C src uninstall

clean: 
	$(MAKE) -C src clean 
