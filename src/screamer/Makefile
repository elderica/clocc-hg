## makefile for screamer
# $Id$
# $Source$

TOP := $(shell cd ../..; pwd)
LISPEXT := lisp
SOURCES := equations iterate primordial screamer screams
include $(TOP)/clocc.mk

screamer.$(FASLEXT): screamer.$(LISPEXT)

iterate.$(FASLEXT): iterate.$(LISPEXT)

primordial.$(FASLEXT): primordial.$(LISPEXT) \
	iterate.$(FASLEXT) screamer.$(FASLEXT)

screams.$(FASLEXT): screams.$(LISPEXT)  \
	iterate.$(FASLEXT) screamer.$(FASLEXT)

equations.$(FASLEXT): equations.$(LISPEXT) \
	iterate.$(FASLEXT) screamer.$(FASLEXT)

check: screamer.$(FASLEXT) iterate.$(FASLEXT) primordial.$(FASLEXT)
	$(TOP)/bin/run-lisp $(patsubst %,-i %,$^) \
		-x '(print (funcall (intern "PRIME-ORDEAL" "PRIMORDIAL")))'
