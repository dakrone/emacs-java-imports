CASK ?= cask
EMACS ?= emacs

all: test

test: unit

unit:
	${CASK} exec ert-runner

ecukes:
	${CASK} exec ecukes

install:
	${CASK} install
