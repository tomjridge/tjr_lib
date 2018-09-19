SUBDIRS:=core non_core

build: 
	for f in $(SUBDIRS); do $(MAKE) -C $$f $@; done


install:
	for f in $(SUBDIRS); do $(MAKE) -C $$f $@; done


uninstall:
	for f in $(SUBDIRS); do $(MAKE) -C $$f $@; done


clean: 
	for f in $(SUBDIRS); do $(MAKE) -C $$f $@; done


FORCE:
