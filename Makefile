.POSIX:
.SUFFIXES:

FC      = gfortran
AR      = ar
MAKE    = make
PREFIX  = /usr/local

DEBUG   = -std=f2018 -g -O0 -Wall -fmax-errors=1
RELEASE = -std=f2018 -O2

PPFLAGS = -D__FreeBSD__
FFLAGS  = $(RELEASE)
LDFLAGS = -I$(PREFIX)/include -L$(PREFIX)/lib
LDLIBS  = -llmdb
ARFLAGS = rcs
INCDIR  = $(PREFIX)/include/libfortran-lmdb
LIBDIR  = $(PREFIX)/lib
SRC     = src/lmdb.F90
OBJ     = lmdb.o
MOD     = lmdb.mod
TARGET  = ./libfortran-lmdb.a

.PHONY: all clean debug freebsd install linux test windows

all: $(TARGET)

freebsd:
	$(MAKE) $(TARGET) PPFLAGS="-D__FreeBSD__"

linux:
	$(MAKE) $(TARGET) PPFLAGS="-D__linux__"

windows:
	$(MAKE) $(TARGET) PPFLAGS="-D_MSC_VER -D_WIN32"

debug:
	$(MAKE) $(TARGET) PPFLAGS="$(PPFLAGS)" FFLAGS="$(DEBUG)"
	$(MAKE) test FFLAGS="$(DEBUG)"

test: test_lmdb

$(TARGET): $(SRC)
	$(FC) $(PPFLAGS) $(FFLAGS) -c $(SRC)
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)

test_lmdb: $(TARGET) test/test_lmdb.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o test_lmdb test/test_lmdb.f90 $(TARGET) $(LDLIBS)

install: $(TARGET)
	@echo "--- Installing library to $(LIBDIR)/ ..."
	install -d $(LIBDIR)
	install -m 644 $(TARGET) $(LIBDIR)/
	if [ -e $(SHARED) ]; then install -m 644 $(SHARED) $(LIBDIR)/; fi
	@echo "--- Installing module to $(INCDIR)/ ..."
	install -d $(INCDIR)
	install -m 644 $(MOD) $(INCDIR)/

clean:
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then rm *.mod; fi
	if [ `ls -1 *.o 2>/dev/null | wc -l` -gt 0 ]; then rm *.o; fi
	if [ -e $(TARGET) ]; then rm $(TARGET); fi
	if [ -e test_lmdb ]; then rm test_lmdb; fi
