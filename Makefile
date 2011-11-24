
TOP := $(shell pwd)
LISPEXT := lisp
SOURCES := clocc
include $(TOP)/clocc.mk

TOP_DEP = clocc.$(FASLEXT) src/defsystem-3.x/defsystem.$(FASLEXT)

ifneq ($(DO_DUMP),)
ifneq ($(DUMPEXT),)
clocc-top: clocc-top$(DUMPEXT)
endif

clocc-top$(DUMPEXT): $(TOP_DEP)
	$(RUNLISP) $(patsubst %,-i %,$^) -d clocc-top

else

clocc-top: clocc-top.$(FASLEXT)

clocc-top.$(FASLEXT): $(TOP_DEP)
	$(RUNLISP) -cat $^ > $@

endif

# should be exuberant ctags
CTAGS = ctags
TAGS : force
	$(CTAGS) -e -R .

recursive-clean: force
	for x in `find . -name .hg -prune -o -type d`; do \
		if [ -r $${x}/Makefile ]; then $(MAKE) -C $${x} clean; \
		else TOP=$(TOP) $(MAKE) -C $${x} -f $(TOP)/clocc.mk clean; \
		fi ; \
	done

hg.log: force
	hg log > $@ 2>/dev/null

clocc.diff: force
	(clocc diff > $@ && $(RM) $@) || true

clocc.diff.gz: clocc.diff
	gzip -9vf $^

hg-stat: force
	@hg log --template '{author}\n' | sort | uniq -c | sort -n
	@hg log --template '\n' | wc -l

fix-perms: force
	find . -name \*.lisp -a -perm /+x -print0 | xargs -0 chmod -v a-x

tarname=clocc
TARFILES=INSTALL Makefile README bin clocc.lisp clocc.mk etc src
$(tarname).tgz: force
	$(RM) $(tarname); $(LN) -s . $(tarname)
	tar -zvhcf $@ $(addprefix $(tarname)/,$(TARFILES)) \
		$(addprefix --exclude=,$(FASLFILES) $(JUNK) .hg .hgignore)
	$(RM) $(tarname)
