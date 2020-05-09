# rules for directories with ly2MusicXML tests.

CHECK = $(src-dir)/check.sh

MUSICXML_DIR ?= $(outdir)/musicxml
MUSICXML_REPO ?= https://github.com/w3c/musicxml.git
MUSICXML_GIT_TAG ?= v3.0

MUSIC2LY = $(src-dir)/music2ly.sh
GIT ?= /usr/bin/git
GUILE ?= /usr/local/bin/guile
GUILE_FLAGS = -L $(configure-builddir)/out/share/lilypond/current/

LILYPOND ?= $(src-dir)/lilypond.sh $(LILYPOND_BINARY)


TEST-TARGETS = precond1.checked \
	xml01.checked xml02.checked xml03.checked xml04.checked \
	xml05.checked xml06.checked xml07.checked xml08.checked \
	xml09.checked xml10.checked xml11.checked xml12.checked \
	xml13.checked xml14.checked xml15.checked \
	lily01.checked lily02.checked lily03.checked lily04.checked \
	lily05.checked lily06.checked lily07.checked lily08.checked \
	lily09.checked lily10.checked lily11.checked