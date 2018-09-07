NAME:=tjr_lib
all: 
	docker build -t $(NAME)_from_github -f Dockerfile .
	rm -rf src
	$(MAKE) -C ../src clea
	rsync -vaz ../src .
	docker build -t $(NAME)_local -f Dockerfile.local .

#	docker run -ti $NAME bash
