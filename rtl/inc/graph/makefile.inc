GRAPHINCNAMES=graph.inc fontdata.inc clip.inc fills.inc gtext.inc palette.inc modes.inc

ifeq ($(OS_TARGET),go32v2)
GRAPHINCNAMES+=vesa.inc vesah.inc dpmi.inc
endif
