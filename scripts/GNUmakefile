depth = ..

SUBDIRS=build

SEXECUTABLES=convert-ly lilypond-book abc2ly etf2ly midi2ly lilypond-invoke-editor musicxml2ly lilysong lilymidi

STEPMAKE_TEMPLATES=script help2man po
LOCALSTEPMAKE_TEMPLATES = lilypond
HELP2MAN_EXECS = $(SEXECUTABLES)

AT_FILES=relocate-preamble
at-ext=.py
at-dir=$(outdir)

include $(depth)/make/stepmake.make

$(PYTHON_SCRIPTS): $(outdir)/relocate-preamble.py

local-install:
	-$(INSTALLPY) -d $(DESTDIR)$(bindir)
	$(INSTALLPY) -c -m 755 $(addprefix $(outdir)/, $(SEXECUTABLES)) $(DESTDIR)$(bindir)

local-uninstall:
	$(foreach a, $(SEXECUTABLES), rm -f $(DESTDIR)$(bindir)/$(a) &&) true
