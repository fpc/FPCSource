{****************************************************************************

                   Copyright (c) 1999-2000 by Florian Kl„mpfl
                  
 ****************************************************************************}
unit bsememf;

  interface

    const
       PAG_READ = $00000001;
       PAG_WRITE = $00000002;
       PAG_EXECUTE = $00000004;
       PAG_GUARD = $00000008;
       PAG_DEFAULT = $00000400;
       PAG_COMMIT = $00000010;
       PAG_DECOMMIT = $00000020;
       OBJ_TILE = $00000040;
       OBJ_PROTECTED = $00000080;
       OBJ_GETTABLE = $00000100;
       OBJ_GIVEABLE = $00000200;
       fPERM = (PAG_EXECUTE or PAG_READ or PAG_WRITE);
       fSHARE = (OBJ_GETTABLE or OBJ_GIVEABLE);
       fALLOC = (OBJ_TILE or PAG_COMMIT or fPERM);
       fALLOCSHR = (OBJ_TILE or PAG_COMMIT or fSHARE or fPERM);
       fGETNMSHR = (fPERM);
       fGETSHR = (fPERM);
       fGIVESHR = (fPERM);
       fSET = (PAG_COMMIT+PAG_DECOMMIT+PAG_DEFAULT+fPERM);
       DOSSUB_INIT = $01;
       DOSSUB_GROW = $02;
       DOSSUB_SPARSE_OBJ = $04;
       DOSSUB_SERIALIZE = $08;
       PAG_SHARED = $00002000;
       PAG_FREE = $00004000;
       PAG_BASE = $00010000;

  implementation

end.
