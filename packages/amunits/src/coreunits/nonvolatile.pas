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

    Update for Amigaos 3.9
    Changed startcode for unit.
    09 Feb 2003.

    nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}
{$PACKRECORDS 2}

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

FUNCTION DeleteNV(const appName : pCHAR location 'a0'; const itemName : pCHAR location 'a1'; killRequesters : LONGINT location 'd1') : BOOLEAN; syscall NVBase 048;
PROCEDURE FreeNVData(data : POINTER location 'a0'); syscall NVBase 036;
FUNCTION GetCopyNV(const appName : pCHAR location 'a0'; const itemName : pCHAR location 'a1'; killRequesters : LONGINT location 'd1') : POINTER; syscall NVBase 030;
FUNCTION GetNVInfo(killRequesters : LONGINT location 'd1') : pNVInfo; syscall NVBase 054;
FUNCTION GetNVList(const appName : pCHAR location 'a0'; killRequesters : LONGINT location 'd1') : pMinList; syscall NVBase 060;
FUNCTION SetNVProtection(const appName : pCHAR location 'a0'; const itemName : pCHAR location 'a1'; mask : LONGINT location 'd2'; killRequesters : LONGINT location 'd1') : BOOLEAN; syscall NVBase 066;
FUNCTION StoreNV(const appName : pCHAR location 'a0'; const itemName : pCHAR location 'a1'; const data : POINTER location 'a2'; length : ULONG location 'd0'; killRequesters : LONGINT location 'd1') : WORD; syscall NVBase 042;

{Here we read how to compile this unit}
{You can remove this include and use a define instead}
{$I useautoopenlib.inc}
{$ifdef use_init_openlib}
procedure InitNONVOLATILELibrary;
{$endif use_init_openlib}

{This is a variable that knows how the unit is compiled}
var
    NONVOLATILEIsCompiledHow : longint;

IMPLEMENTATION

{$ifndef dont_use_openlib}
uses amsgbox;
{$endif dont_use_openlib}

const
    { Change VERSION and LIBVERSION to proper values }

    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

{$ifdef use_init_openlib}
  {$Info Compiling initopening of nonvolatile.library}
  {$Info don't forget to use InitNONVOLATILELibrary in the beginning of your program}

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

procedure InitNONVOLATILELibrary;
begin
    NVBase := nil;
    NVBase := OpenLibrary(NONVOLATILENAME,LIBVERSION);
    if NVBase <> nil then begin
        nonvolatile_exit := ExitProc;
        ExitProc := @ClosenonvolatileLibrary;
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open nonvolatile.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;
end;

begin
    NONVOLATILEIsCompiledHow := 2;
{$endif use_init_openlib}

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

begin
    NVBase := nil;
    NVBase := OpenLibrary(NONVOLATILENAME,LIBVERSION);
    if NVBase <> nil then begin
        nonvolatile_exit := ExitProc;
        ExitProc := @ClosenonvolatileLibrary;
        NONVOLATILEIsCompiledHow := 1;
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open nonvolatile.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;

{$endif use_auto_openlib}

{$ifdef dont_use_openlib}
begin
    NONVOLATILEIsCompiledHow := 3;
   {$Warning No autoopening of nonvolatile.library compiled}
   {$Warning Make sure you open nonvolatile.library yourself}
{$endif dont_use_openlib}



END. (* UNIT NONVOLATILE *)
