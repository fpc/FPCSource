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
    use_auto_openlib. Implemented autoopening of
    the library.
    14 Jan 2003.

    Update for AmigaOS 3.9.
    Changed start code for unit.
    01 Feb 2003.

    Changed cardinal > longword.
    09 Feb 2003.

    nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}

{$I useamigasmartlink.inc}
{$ifdef use_amiga_smartlink}
   {$smartlink on}
{$endif use_amiga_smartlink}

UNIT expansion;

INTERFACE
USES exec, configvars, amigados;

Const
    EXPANSIONNAME       : PChar = 'expansion.library';

{ flags for the AddDosNode() call }
    ADNB_STARTPROC      = 0;

    ADNF_STARTPROC      = 1;

VAR ExpansionBase : pLibrary;


FUNCTION AddBootNode(bootPri : LONGINT location 'd0'; flags : ULONG location 'd1'; deviceNode : pDeviceNode location 'a0'; configDev : pConfigDev location 'a1') : wordbool; syscall ExpansionBase 036;
PROCEDURE AddConfigDev(configDev : pConfigDev location 'a0'); syscall ExpansionBase 030;
FUNCTION AddDosNode(bootPri : LONGINT location 'd0'; flags : ULONG location 'd1'; deviceNode : pDeviceNode location 'a0') : wordbool; syscall ExpansionBase 150;
PROCEDURE AllocBoardMem(slotSpec : ULONG location 'd0'); syscall ExpansionBase 042;
FUNCTION AllocConfigDev : pConfigDev; syscall ExpansionBase 048;
FUNCTION AllocExpansionMem(numSlots : ULONG location 'd0'; slotAlign : ULONG location 'd1') : POINTER; syscall ExpansionBase 054;
PROCEDURE ConfigBoard(board : POINTER location 'a0'; configDev : pConfigDev location 'a1'); syscall ExpansionBase 060;
PROCEDURE ConfigChain(baseAddr : POINTER location 'a0'); syscall ExpansionBase 066;
FUNCTION FindConfigDev(const oldConfigDev : pConfigDev location 'a0'; manufacturer : LONGINT location 'd0'; product : LONGINT location 'd1') : pConfigDev; syscall ExpansionBase 072;
PROCEDURE FreeBoardMem(startSlot : ULONG location 'd0'; slotSpec : ULONG location 'd1'); syscall ExpansionBase 078;
PROCEDURE FreeConfigDev(configDev : pConfigDev location 'a0'); syscall ExpansionBase 084;
PROCEDURE FreeExpansionMem(startSlot : ULONG location 'd0'; numSlots : ULONG location 'd1'); syscall ExpansionBase 090;
FUNCTION GetCurrentBinding(const currentBinding : pCurrentBinding location 'a0'; bindingSize : ULONG location 'd0') : ULONG; syscall ExpansionBase 138;
FUNCTION MakeDosNode(const parmPacket : POINTER location 'a0') : pDeviceNode; syscall ExpansionBase 144;
PROCEDURE ObtainConfigBinding; syscall ExpansionBase 120;
FUNCTION ReadExpansionByte(const board : POINTER location 'a0'; offset : ULONG location 'd0') : BYTE; syscall ExpansionBase 096;
PROCEDURE ReadExpansionRom(const board : POINTER location 'a0'; configDev : pConfigDev location 'a1'); syscall ExpansionBase 102;
PROCEDURE ReleaseConfigBinding; syscall ExpansionBase 126;
PROCEDURE RemConfigDev(configDev : pConfigDev location 'a0'); syscall ExpansionBase 108;
PROCEDURE SetCurrentBinding(currentBinding : pCurrentBinding location 'a0'; bindingSize : ULONG location 'd0'); syscall ExpansionBase 132;
PROCEDURE WriteExpansionByte(board : POINTER location 'a0'; offset : ULONG location 'd0'; byte : ULONG location 'd1'); syscall ExpansionBase 114;

{Here we read how to compile this unit}
{You can remove this include and use a define instead}
{$I useautoopenlib.inc}
{$ifdef use_init_openlib}
procedure InitEXPANSIONLibrary;
{$endif use_init_openlib}

{This is a variable that knows how the unit is compiled}
var
    EXPANSIONIsCompiledHow : longint;

IMPLEMENTATION

uses
{$ifndef dont_use_openlib}
amsgbox;
{$endif dont_use_openlib}

const
    { Change VERSION and LIBVERSION to proper values }

    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

{$ifdef use_init_openlib}
  {$Info Compiling initopening of expansion.library}
  {$Info don't forget to use InitEXPANSIONLibrary in the beginning of your program}

var
    expansion_exit : Pointer;

procedure CloseexpansionLibrary;
begin
    ExitProc := expansion_exit;
    if ExpansionBase <> nil then begin
        CloseLibrary(ExpansionBase);
        ExpansionBase := nil;
    end;
end;

procedure InitEXPANSIONLibrary;
begin
    ExpansionBase := nil;
    ExpansionBase := OpenLibrary(EXPANSIONNAME,LIBVERSION);
    if ExpansionBase <> nil then begin
        expansion_exit := ExitProc;
        ExitProc := @CloseexpansionLibrary;
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open expansion.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;
end;

begin
    EXPANSIONIsCompiledHow := 2;
{$endif use_init_openlib}

{$ifdef use_auto_openlib}
  {$Info Compiling autoopening of expansion.library}

var
    expansion_exit : Pointer;

procedure CloseexpansionLibrary;
begin
    ExitProc := expansion_exit;
    if ExpansionBase <> nil then begin
        CloseLibrary(ExpansionBase);
        ExpansionBase := nil;
    end;
end;

begin
    ExpansionBase := nil;
    ExpansionBase := OpenLibrary(EXPANSIONNAME,LIBVERSION);
    if ExpansionBase <> nil then begin
        expansion_exit := ExitProc;
        ExitProc := @CloseexpansionLibrary;
        EXPANSIONIsCompiledHow := 1;
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open expansion.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;

{$endif use_auto_openlib}

{$ifdef dont_use_openlib}
begin
    EXPANSIONIsCompiledHow := 3;
   {$Warning No autoopening of expansion.library compiled}
   {$Warning Make sure you open expansion.library yourself}
{$endif dont_use_openlib}


END. (* UNIT EXPANSION *)



