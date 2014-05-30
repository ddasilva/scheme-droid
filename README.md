# Scheme Droid

Scheme Droid is a REPL (Read Eval Print Loop) Scheme interpreter for Android. [Scheme](http://en.wikipedia.org/wiki/Scheme_%28programming_language%29) is a minimalist, multi-paradigm dialect of Lisp. Scheme Droid is powered by an enhanced version of the [JScheme](http://jscheme.sourceforge.net/jscheme/main.html) Library.

Scheme Droid is [available on the Google Play Store](https://market.android.com/details?id=net.meltingwax.schemedroid&hl=en).

# Building

To build and install onto your device, first set the ``$ANDROID_HOME`` environmental variable and then execute the following commands. Building on Windows is not currently supported.

    $ make
    $ make install

**Eclipse and ADT users** must execute ``make`` first, and then may use the integrated compilation and deployment tools.

# License

Scheme Droid is, and always will be, free software. The code of Scheme Droid is licensed under the [GNU GPL v2](http://www.gnu.org/licenses/gpl-2.0.html) (see ``LICENSE``), and the bundled code of JScheme is licensed under the [zlib/libpng open source license](http://www.libpng.org/pub/png/src/libpng-LICENSE.txt) (see ``LICENSE.jscheme``).