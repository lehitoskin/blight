Blight
======
[![Build Status](http://jenkins.libtoxcore.so/job/Blight-Debian/badge/icon)](http://jenkins.libtoxcore.so/job/Blight-Debian/)

Blight is a cross-platform graphical user interface for [Tox](https://github.com/irungentoo/toxcore) written in [Racket](http://racket-lang.org/) that utilizes [libtoxcore-racket](https://github.com/lehitoskin/libtoxcore-racket) and [libopenal-racket](https://github.com/lehitoskin/libopenal-racket).
In very early pre-alpha stage. All help and testing appreciated.

![Blight Screenshot](https://raw.github.com/lehitoskin/blight/master/screenshot-2015-01-01.png "Blight Screenshot")

## Prerequisites

- Racket (version 6.0.1 or higher)

- Toxcore

- OpenAL

- libtoxcore-racket

- libopenal-racket

## Installation

- Precompiled binaries are automatically compiled after a Github commit. Navigate to the blight/bin directory and run blight. Please note that since these builds are taken from the latest git the program's behavior may have unintended side effects.

- [Latest amd64 GNU/Linux](https://jenkins.libtoxcore.so/job/Blight-Debian/lastSuccessfulBuild/artifact/blight-latest-linux-amd64.tar.xz)

- [Latest amd64 OSX](https://jenkins.libtoxcore.so/job/Blight-OS_X/lastSuccessfulBuild/artifact/blight-latest-darwin-x86_64.tar.gz)

- [INSTALL.md](INSTALL.md) details installation from source.

## Keyboard shortcuts

- Blight implements several cutsom keyboard shortcuts for commonly used unicode characters.

- λ (U+03BB) is mapped to ctrl+\

- © (U+00A9) is mapped to ctrl+1

- ® (U+00AE) is mapped to ctrl+2

- ™ (U+2122) is mapped to ctrl+3

## Read Eval Print Loop (REPL) Client

Blight now contains a REPL server built in, which you can access through the `repl-client.rkt` program.
This REPL client will allow you to access the internals of Blight while it is running and you can modify the program on the fly.
While this does give you a great amount of power, it also requires a great amount of knowledge of the program's internals
and good amount responsibility in how you use that knowledge. It is always a danger that you could corrupt the data file,
so be sure to backup your files frequently!

## Contributing

- Check the [issue tracker](https://github.com/lehitoskin/Blight/issues?direction=desc&sort=created&state=open) for bugs to be fixed and features to be implemented.

- Come up with your own idea, [create a discussion issue](https://github.com/lehitoskin/blight/issues/new) for it and get feedback.

- [Report bugs](https://github.com/lehitoskin/blight/issues/new).

## Contact

- Visit the Tox IRC channel `#tox-dev` on [freenode](http://freenode.net/).

- Email me: [lehi@tosk.in](mailto:lehi@tosk.in)

- Tox me: `802D30E27746AE299FC2796D014C24700140574BFBFBB9397114D7CB82DC25728BA74CC378EF`

## License

Blight is licensed under [GPLv3+](LICENSE).
