unit nwpre;

interface

// AD 02.09.2000: Dont know why its not working with kNLMInfo...
//                It always abends in TerminateNLM, so i am using the old style
{$DEFINE OldPrelude}

FUNCTION _Prelude (NLMHandle               : LONGINT;
                   initErrorScreenID       : LONGINT;
                   cmdLineP                : PCHAR;
                   loadDirectoryPath       : PCHAR;
                   uninitializedDataLength : LONGINT;
                   NLMFileHandle           : LONGINT;
                   readRoutineP            : POINTER;
                   customDataOffset        : LONGINT;
                   customDataSize          : LONGINT) : LONGINT; CDECL;


implementation


FUNCTION _TerminateNLM (NLMInformation : POINTER; threadID, status : LONGINT) : LONGINT; CDECL; EXTERNAL;
FUNCTION _SetupArgV_411 (MainProc : POINTER) : LONGINT; CDECL; EXTERNAL;
FUNCTION _StartNLM (NLMHandle               : LONGINT;
                    initErrorScreenID       : LONGINT;
                    cmdLineP                : PCHAR;
                    loadDirectoryPath       : PCHAR;
                    uninitializedDataLength : LONGINT;
                    NLMFileHandle           : LONGINT;
                    readRoutineP            : POINTER;
                    customDataOffset        : LONGINT;
                    customDataSize          : LONGINT;
                    NLMInformation          : POINTER;
                    userStartFunc           : POINTER) : LONGINT; CDECL; EXTERNAL;
//PROCEDURE _exit (x : LONGINT); CDECL; EXTERNAL;		    


(*****************************************************************************)

CONST TRADINIONAL_NLM_INFO_SIGNATURE = 0;
      TRADINIONAL_FLAVOR             = 0;
      TRADINIONAL_VERSION            = 0;
      LIBERTY_VERSION                = 1;
      TERMINATE_BY_EXTERNAL_THREAD   = 0;
      TERMINATE_BY_UNLOAD            = 5;


{$IFDEF OldPrelude}
CONST NLMID : LONGINT = 0;
{$ELSE}
TYPE 
  kNLMInfoT =
  PACKED RECORD
    Signature      : ARRAY [0..3] OF CHAR;  // LONG
    Flavor         : LONGINT;
    Version        : LONGINT;
    LongDoubleSize : LONGINT;
    wchar_tSize    : LONGINT;
  END;

CONST NLM_INFO_SIGNATURE             = 'NLMI';  // $494d3c3e;  // NLMI

      kNLMInfo : kNLMInfoT =
       (Signature      : NLM_INFO_SIGNATURE;
        Flavor         : TRADINIONAL_FLAVOR;    // 0
        Version        : LIBERTY_VERSION;       // 1
        LongDoubleSize : 8; 
        wchar_tSize    : 2);
{$ENDIF}

(*****************************************************************************)

FUNCTION _nlm_main (Argc : LONGINT; ArgV : ARRAY OF PCHAR) : LONGINT; CDECL;
EXTERNAL;


FUNCTION _Stop : LONGINT; CDECL;
BEGIN
  {$IFDEF OldPrelude}
  _Stop := _TerminateNLM (POINTER(NLMID),0,TERMINATE_BY_UNLOAD);
  {$ELSE}
  _Stop := _TerminateNLM (@kNLMInfo,0,TERMINATE_BY_UNLOAD);
  {$ENDIF}
END;


FUNCTION _cstart_ : LONGINT; CDECL;
BEGIN
  _cstart_ := _SetupArgV_411 (@_nlm_main);
END;


FUNCTION _Prelude (NLMHandle               : LONGINT;
                   initErrorScreenID       : LONGINT;
                   cmdLineP                : PCHAR;
                   loadDirectoryPath       : PCHAR;
                   uninitializedDataLength : LONGINT;
                   NLMFileHandle           : LONGINT;
                   readRoutineP            : POINTER;
                   customDataOffset        : LONGINT;
                   customDataSize          : LONGINT) : LONGINT; CDECL;
BEGIN
  _Prelude := _StartNLM
            (NLMHandle,
             initErrorScreenID,
             cmdLineP,
             loadDirectoryPath,
             uninitializedDataLength,
             NLMFileHandle,
             readRoutineP,
             customDataOffset,
             customDataSize,
	     {$IFDEF OldPrelude}
	     @NLMID,
	     {$ELSE}
             @kNLMInfo,
	     {$ENDIF}
             @_cstart_);
END;




end.
{
  $Log$
  Revision 1.2  2001-04-11 14:17:00  florian
    * added logs, fixed email address of Armin, it is
      diehl@nordrhein.de

}
