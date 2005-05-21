{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team
    Copyright (c) 2001 Armin Diehl

    This unit implements the startup code for a netware nlm. It must be the first object file
    linked. Currently the 'old-style', similar to novell's prelude.obj is used. With the newer
    way (novells nwpre.obj) i only got abends. Dont know what's different in novells nwpre.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

unit nwpre;

interface

{ 2000/08/29 armin: first version, untested
  2000/09/02 armin: Dont know why its not working with kNLMInfo...
                    It always abends in TerminateNLM, so i am using the old style
  2001/04/15 armin: Added comments, S-
                    Removed dead code }

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

{$S-}

FUNCTION _TerminateNLM  (NLMInformation          : POINTER;
                         threadID, status        : LONGINT) : LONGINT; CDECL; EXTERNAL;

FUNCTION _SetupArgV_411 (MainProc                : POINTER) : LONGINT; CDECL; EXTERNAL;

FUNCTION _StartNLM      (NLMHandle               : LONGINT;
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


{**************************************************************************************************}

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

CONST NLM_INFO_SIGNATURE             = 'NLMI';  // 0x494d3c3e;

      kNLMInfo : kNLMInfoT =
       (Signature      : NLM_INFO_SIGNATURE;
        Flavor         : TRADINIONAL_FLAVOR;    // 0
        Version        : LIBERTY_VERSION;       // 1
        LongDoubleSize : 8;
        wchar_tSize    : 2);
{$ENDIF}

{**************************************************************************************************}

{ _nlm_main is defined in system.pp. It sets command line parameters and calls PASCALMAIN }
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
