
all: msg_unix.ml


msg_unix.ml: msg_core.ml
	cat $< >sed -e "s/open M/open Net_unix/" >$@
