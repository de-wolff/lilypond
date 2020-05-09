.SUFFIXES: .scm .xml .xpath .checked .ly-music


$(outdir)/%.ly : $(src-dir)/%.ly-music
	$(call ly_progress,Making,$@,< ly-music)
	@$(MUSIC2LY) $< $@

%.xml %.info %.xpath : $(outdir)/%.ly
	$(call ly_progress,Making,$@,< ly)
	@$(LILYPOND) $< 1> $(basename $(notdir $@)).log

$(outdir)/%.checked : $(outdir)/%.xml $(outdir)/%.xpath $(outdir)/%.info $(outdir)/catalog.xml
	@$(CHECK) $< $(src-dir) $(outdir)
	touch $@

$(outdir)/%.xml $(outdir)/%.info $(outdir)/%.xpath: $(src-dir)/%.scm
	$(call ly_progress,Making,$@,< scm)
	@$(GUILE) $(GUILE_FLAGS) $< $@

$(outdir)/%.xml : %.xml
	mv $< $@

$(outdir)/%.xpath : %.xpath
	mv $< $@

$(outdir)/%.info : %.info
	mv $< $@
