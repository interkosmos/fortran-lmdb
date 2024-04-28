# fortran-lmdb

A collection of Fortran 2018 interface bindings to the OpenLDAP
[Lightning Memory-Mapped Database](http://www.lmdb.tech/doc/) (LMDB), a
B-tree-based database management library modeled loosely on the BerkeleyDB API.

## Build Instructions

The LMDB package has to be installed with development headers. On FreeBSD, run:

```
# pkg install databases/lmdb
```

On Linux, instead:

```
# apt-get install liblmdb-dev
```

### Make

If GNU Fortran is used to build the interface library, select the build target
depending on your operating system. On FreeBSD:

```
$ make freebsd
```

On Linux, instead:

```
$ make linux
```

On Windows:

```
$ make windows
```

Or, overwrite the argument `PPFLAGS`. Install the library and the module files
system-wide, for example:

```
$ make install PREFIX=/opt
```

Link your programs against `/opt/lib/libfortran-lmdb.a -llmdb`. Alternatively,
overwrite the default compiler and the compiler/preprocessor flags:

```
$ make FC=ifx FFLAGS="-O3" PPFLAGS="-D__linux__"
```

Build and run the test program:

```
$ make test
$ ./test_lmdb
```

### Fortran Package Manager

Pass the operating system as an additional flag to the
[Fortran Package Manager](https://github.com/fortran-lang/fpm). On FreeBSD:

```
$ fpm build --profile release --flag "-D__FreeBSD__"
```

On Linux:

```
$ fpm build --profile release --flag "-D__linux__"
```

On Windows:

```
$ fpm build --profile release --flag "-D_MSC_VER -D_WIN32"
```

You can add *fortran-lmdb* as an FPM dependency:

```toml
[dependencies]
fortran-lmdb = { git = "https://github.com/interkosmos/fortran-lmdb.git" }
```

## Licence

ISC
