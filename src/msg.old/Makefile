
all: msg_unix.ml msg_lwt.ml


msg_unix.ml: msg_core.ml FORCE
	-chmod u+w $@
	echo "(* THIS IS A GENERATED FILE; DO NOT EDIT *)" >$@
	cat $< |sed -e "s/open M /open Net_unix /" >>$@
	chmod u-w $@


msg_lwt.ml: msg_core.ml FORCE
	-chmod u+w $@
	echo "(* THIS IS A GENERATED FILE; DO NOT EDIT *)" >$@
	cat $< |sed -e "s/open M /open Net_lwt /" >>$@
	chmod u-w $@


FORCE:
