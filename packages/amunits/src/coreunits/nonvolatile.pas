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

VAR NVBase : pLibrary = nil;

const
    NONVOLATILENAME : PChar = 'nonvolatile.library';

FUNCTION DeleteNV(const appName : pCHAR location 'a0'; const itemName : pCHAR location 'a1'; killRequesters : LONGINT location 'd1') : LongBool; syscall NVBase 048;
PROCEDURE FreeNVData(data : POINTER location 'a0'); syscall NVBase 036;
FUNCTION GetCopyNV(const appName : pCHAR location 'a0'; const itemName : pCHAR location 'a1'; killRequesters : LONGINT location 'd1') : POINTER; syscall NVBase 030;
FUNCTION GetNVInfo(killRequesters : LONGINT location 'd1') : pNVInfo; syscall NVBase 054;
FUNCTION GetNVList(const appName : pCHAR location 'a0'; killRequesters : LONGINT location 'd1') : pMinList; syscall NVBase 060;
FUNCTION SetNVProtection(const appName : pCHAR location 'a0'; const itemName : pCHAR location 'a1'; mask : LONGINT location 'd2'; killRequesters : LONGINT location 'd1') : LongBool; syscall NVBase 066;
FUNCTION StoreNV(const appName : pCHAR location 'a0'; const itemName : pCHAR location 'a1'; const data : POINTER location 'a2'; length : ULONG location 'd0'; killRequesters : LONGINT location 'd1') : WORD; syscall NVBase 042;

IMPLEMENTATION

const
    { Change VERSION and LIBVERSION to proper values }
    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

initialization
  NVBase := OpenLibrary(NONVOLATILENAME,LIBVERSION);
finalization
  if Assigned(NVBase) then
    CloseLibrary(NVBase);
END. (* UNIT NONVOLATILE *)
