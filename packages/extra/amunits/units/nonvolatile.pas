{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
    History:
    
    Added the defines use_amiga_smartlink and
    use_auto_openlib. Implemented autoopening
    of the library.
    14 Jan 2003.
    
    nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}

{$I useamigasmartlink.inc}
{$ifdef use_amiga_smartlink}
   {$smartlink on}
{$endif use_amiga_smartlink}

UNIT nonvolatile;

INTERFACE
USES exec;


Type
 pNVInfo = ^tNVInfo;
 tNVInfo = record
    nvi_MaxStorage,
    nvi_FreeStorage : ULONG;
 end;

{***************************************************************************}


 pNVEntry = ^tNVEntry;
 tNVEntry = record
    nve_Node        : tMinNode;
    nve_Name        : STRPTR;
    nve_Size,
    nve_Protection  : ULONG;
 end;

const
{ bit definitions for mask in SetNVProtection().  Also used for
 * NVEntry.nve_Protection.
 }
 NVEB_DELETE  = 0 ;
 NVEB_APPNAME = 31;

 NVEF_DELETE  = 1;
 NVEF_APPNAME = -2147483648;


{***************************************************************************}


{ errors from StoreNV() }
 NVERR_BADNAME   = 1;
 NVERR_WRITEPROT = 2;
 NVERR_FAIL      = 3;
 NVERR_FATAL     = 4;



{ --- functions in V40 or higher (Release 3.1) --- }

VAR NVBase : pLibrary;

const
    NONVOLATILENAME : PChar = 'nonvolatile.library';

FUNCTION DeleteNV(appName : pCHAR; itemName : pCHAR; killRequesters : LONGINT) : BOOLEAN;
PROCEDURE FreeNVData(data : POINTER);
FUNCTION GetCopyNV(appName : pCHAR; itemName : pCHAR; killRequesters : LONGINT) : POINTER;
FUNCTION GetNVInfo(killRequesters : LONGINT) : pNVInfo;
FUNCTION GetNVList(appName : pCHAR; killRequesters : LONGINT) : pMinList;
FUNCTION SetNVProtection(appName : pCHAR; itemName : pCHAR; mask : LONGINT; killRequesters : LONGINT) : BOOLEAN;
FUNCTION StoreNV(appName : pCHAR; itemName : pCHAR; data : POINTER; length : ULONG; killRequesters : LONGINT) : WORD;

IMPLEMENTATION

uses msgbox;

FUNCTION DeleteNV(appName : pCHAR; itemName : pCHAR; killRequesters : LONGINT) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L appName,A0
    MOVEA.L itemName,A1
    MOVE.L  killRequesters,D1
    MOVEA.L NVBase,A6
    JSR -048(A6)
    MOVEA.L (A7)+,A6
    TST.W   D0
    BEQ.B   @end
    MOVEQ   #1,D0
  @end: MOVE.B  D0,@RESULT
  END;
END;

PROCEDURE FreeNVData(data : POINTER);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L data,A0
    MOVEA.L NVBase,A6
    JSR -036(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION GetCopyNV(appName : pCHAR; itemName : pCHAR; killRequesters : LONGINT) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L appName,A0
    MOVEA.L itemName,A1
    MOVE.L  killRequesters,D1
    MOVEA.L NVBase,A6
    JSR -030(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION GetNVInfo(killRequesters : LONGINT) : pNVInfo;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  killRequesters,D1
    MOVEA.L NVBase,A6
    JSR -054(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION GetNVList(appName : pCHAR; killRequesters : LONGINT) : pMinList;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L appName,A0
    MOVE.L  killRequesters,D1
    MOVEA.L NVBase,A6
    JSR -060(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION SetNVProtection(appName : pCHAR; itemName : pCHAR; mask : LONGINT; killRequesters : LONGINT) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L appName,A0
    MOVEA.L itemName,A1
    MOVE.L  mask,D2
    MOVE.L  killRequesters,D1
    MOVEA.L NVBase,A6
    JSR -066(A6)
    MOVEA.L (A7)+,A6
    TST.W   D0
    BEQ.B   @end
    MOVEQ   #1,D0
  @end: MOVE.B  D0,@RESULT
  END;
END;

FUNCTION StoreNV(appName : pCHAR; itemName : pCHAR; data : POINTER; length : ULONG; killRequesters : LONGINT) : WORD;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L appName,A0
    MOVEA.L itemName,A1
    MOVEA.L data,A2
    MOVE.L  length,D0
    MOVE.L  killRequesters,D1
    MOVEA.L NVBase,A6
    JSR -042(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

{$I useautoopenlib.inc}
{$ifdef use_auto_openlib}
  {$Info Compiling autoopening of nonvolatile.library}

var
    nonvolatile_exit : Pointer;

procedure ClosenonvolatileLibrary;
begin
    ExitProc := nonvolatile_exit;
    if NVBase <> nil then begin
        CloseLibrary(NVBase);
        NVBase := nil;
    end;
end;

const
    { Change VERSION and LIBVERSION to proper values }

    VERSION : string[2] = '0';
    LIBVERSION : Cardinal = 0;

begin
    NVBase := nil;
    NVBase := OpenLibrary(NONVOLATILENAME,LIBVERSION);
    if NVBase <> nil then begin
        nonvolatile_exit := ExitProc;
        ExitProc := @ClosenonvolatileLibrary
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open nonvolatile.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;

{$else}
   {$Warning No autoopening of nonvolatile.library compiled}
   {$Info Make sure you open nonvolatile.library yourself}
{$endif use_auto_openlib}


END. (* UNIT NONVOLATILE *)
