DESTDIR=/usr/local

blight: blight.rkt
	raco exe --vv --gui blight.rkt

install: blight
	mkdir -pv $(DESTDIR)/bin
	mkdir -pv $(DESTDIR)/share/blight/sounds
	install -m 0755 blight $(DESTDIR)/bin
	install -m 0644 sounds/* $(DESTDIR)/share/blight/sounds
