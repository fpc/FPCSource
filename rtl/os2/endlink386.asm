; end.asm (emx+gcc) -- Copyright (c) 1995 by Eberhard Mattes

                .386

                PUBLIC  WEAK$ZERO
WEAK$ZERO       =       0

                PUBLIC  _end
                PUBLIC  _edata
                PUBLIC  _etext
                PUBLIC  __end
                PUBLIC  __edata
                PUBLIC  __etext

TEXT32          SEGMENT PUBLIC PARA USE32 'CODE'
TEXT32          ENDS

________TEXT    SEGMENT PUBLIC PARA USE32 'CODE'
__etext         LABEL BYTE
_etext          LABEL BYTE
________TEXT    ENDS

CGROUP          GROUP TEXT32, ________TEXT


DATA32          SEGMENT PUBLIC PARA USE32 'DATA'
DATA32          ENDS

________DATA    SEGMENT PUBLIC PARA USE32 'DATA'
__edata         LABEL BYTE
_edata          LABEL BYTE
________DATA    ENDS


c_common        SEGMENT PUBLIC PARA USE32 'BSS'
c_common        ENDS

________BSS     SEGMENT PUBLIC PARA USE32 'BSS'
__end           LABEL BYTE
_end            LABEL BYTE
________BSS     ENDS

DGROUP          GROUP   DATA32, ________DATA, c_common, ________BSS

                END
