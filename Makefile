CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = matrices

all: compile

include $(ROOT)/make/kz.mk
