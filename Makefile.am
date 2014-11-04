include build-aux/guile.am

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/ccache

SOURCES = \
	jpeg/array.scm \
	jpeg/bit-ports.scm \
	jpeg/exif.scm \
	jpeg/huffman.scm \
	jpeg/jfif.scm \
	jpeg/pixbufs.scm \
	jpeg.scm

TESTS = 					\
	tests/basic.scm
TESTS_ENVIRONMENT = $(abs_top_builddir)/env $(GUILE) --no-auto-compile

EXTRA_DIST += \
	env.in \
	HACKING
