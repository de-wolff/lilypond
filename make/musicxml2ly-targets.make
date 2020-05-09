default:

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

