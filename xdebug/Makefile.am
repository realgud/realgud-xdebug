lisp_files = $(wildcard *.el)
EXTRA_DIST=$(lisp_files)
lisp_LISP = $(lisp_files)
include ../common.mk
lispdir = @lispdir@/ipdb

ipdb.elc: core.elc track-mode.elc
track-mode.elc: core.elc init.elc
