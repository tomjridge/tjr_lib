NAME:=tjr_lib

all: 
	$(MAKE) -C .. clean
	$(MAKE) local
	$(MAKE) from_github
	$(MAKE) using_opam

local:
	rm -rf tjr_lib
	mkdir tjr_lib
	rsync --exclude='.dockerfile/*' --exclude='.git/*' -vaz ../ ./tjr_lib
	docker build -t $(NAME)_local -f Dockerfile.local .

from_github:
	docker build -t $(NAME)_from_github -f Dockerfile.from_github .

using_opam:
	docker build -t $(NAME)_using_opam -f Dockerfile.using_opam .

#	docker run -ti $NAME bash
clean:
	-docker image rm $(NAME)_local $(NAME)_from_github $(NAME)_using_opam
	rm -rf tjr_lib
