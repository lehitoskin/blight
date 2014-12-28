DESTDIR=/usr/local

blight: blight.rkt
	raco exe --vv --gui blight.rkt

blight-repl: repl-client.rkt
	raco exe --vv --gui -o blight-repl repl-client.rkt

install-blight: blight
	mkdir -pv $(DESTDIR)/bin
	mkdir -pv $(DESTDIR)/share/blight/sounds
	mkdir -pv $(DESTDIR)/share/blight/icons
	mkdir -pv $(DESTDIR)/share/icons/hicolor/{16x16,32x32,48x48,64x64,128x128,256x256,512x512,scalable}/apps
	install -m 0755 blight $(DESTDIR)/bin
	install -m 0644 sounds/* $(DESTDIR)/share/blight/sounds
	install -m 0644 img/blight-logo-16px.png $(DESTDIR)/share/icons/16x16/apps
	install -m 0644 img/blight-logo-32px.png $(DESTDIR)/share/icons/32x32/apps
	install -m 0644 img/blight-logo-48px.png $(DESTDIR)/share/icons/48x48/apps
	install -m 0644 img/blight-logo-64px.png $(DESTDIR)/share/icons/64x64/apps
	install -m 0644 img/blight-logo-128px.png $(DESTDIR)/share/icons/128x128/apps
	install -m 0644 img/blight-logo-256px.png $(DESTDIR)/share/icons/256x256/apps
	install -m 0644 img/blight-logo-512px.png $(DESTDIR)/share/icons/512x512/apps
	install -m 0644 img/blight-logo.svg $(DESTDIR)/share/icons/scalable/apps
	install -m 0644 icons/* $(DESTDIR)/share/blight/icons

install-repl: blight-repl
	install -m 0755 blight-repl $(DESTDIR)/bin
