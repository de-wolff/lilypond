depth = ../../..
TEXI2HTML_FLAGS += --nomenu

configure-srcdir ?= .
stepmake ?= $(depth)/stepmake

include $(depth)/make/stepmake.make

.SUFFIXES:
.SUFFIXES: .scm .xml .xpath .checked .ly-music

src-dir ?= $(configure-srcdir)/$(depth)/input/regression/ly2musicxml/

LILYPOND ?= $(src-dir)/lilypond.sh $(configure-builddir)/out/bin/lilypond
CHECK = $(src-dir)/check.sh
MUSICXML_DIR ?= $(outdir)/musicxml
MUSICXML_REPO ?= https://github.com/w3c/musicxml.git
MUSICXML_GIT_TAG ?= v3.0
MUSIC2LY = $(src-dir)/music2ly.sh
GIT ?= /usr/bin/git
GUILE ?= /usr/local/bin/guile
GUILE_FLAGS = -L $(configure-builddir)/out/share/lilypond/current/
TEST-TARGETS = precond1.checked \
	xml01.checked xml02.checked xml03.checked xml04.checked \
	xml05.checked xml06.checked xml07.checked xml08.checked \
	xml09.checked xml10.checked xml11.checked xml12.checked \
	xml13.checked xml14.checked xml15.checked \
	lily01.checked lily02.checked lily03.checked lily04.checked \
	lily05.checked lily06.checked lily07.checked lily08.checked \
	lily09.checked lily10.checked lily11.checked

# remove .SECONDARY when stable.
# for now prevent some intermediate files from being removed
.SECONDARY:
#	%.xml %.ly

.INTERMEDIATE:
#	%.xpath %.regex %.info

$(outdir)/%.ly : $(src-dir)/%.ly-music
	@$(MUSIC2LY) $< $@

%.xml %.info %.xpath : $(outdir)/%.ly
	@$(LILYPOND) $< 1> $(basename $(notdir $@)).log

$(outdir)/%.checked : $(outdir)/%.xml $(outdir)/%.xpath $(outdir)/%.info $(outdir)/catalog.xml
	@$(CHECK) $< $(src-dir) $(outdir)
	touch $@

$(outdir)/%.xml $(outdir)/%.info $(outdir)/%.xpath: $(src-dir)/%.scm
	@$(GUILE) $(GUILE_FLAGS) $< $@

$(outdir)/%.xml : %.xml
	mv $< $@

$(outdir)/%.xpath : %.xpath
	mv $< $@

$(outdir)/%.info : %.info
	mv $< $@

all:

check :
test :
local-test : $(addprefix $(outdir)/,$(TEST-TARGETS)) | $(MUSICXML_DIR)

$(outdir)/catalog.xml: | $(MUSICXML_DIR)
	$(src-dir)/create-catalog.sh $@ $(src-dir)

$(MUSICXML_DIR):
	$(GIT) clone -q --depth 1 -b $(MUSICXML_GIT_TAG) $(MUSICXML_REPO) $(MUSICXML_DIR)

clean :
local-clean :
	@rm -f $(outdir)/*.checked
	@rm -f $(outdir)/precond*.xml
	@rm -f $(outdir)/precond*.xpath
	@rm -f $(outdir)/precond*.regex
	@rm -f $(outdir)/precond*.info
	@rm -f $(outdir)/xml*.xml
	@rm -f $(outdir)/xml*.xpath
	@rm -f $(outdir)/xml*.regex
	@rm -f $(outdir)/xml*.info
	@rm -f $(outdir)/lily*.xpath
	@rm -f $(outdir)/lily*.info
	@rm -f $(outdir)/lily*.xml
	@rm -f $(outdir)/lily*.ly
	@rm -f *.log
	@rm -f $(outdir)/catalog.xml
	@rm -rf $(MUSICXML_DIR)

