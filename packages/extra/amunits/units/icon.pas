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
    Added overlay functions for Pchar->Strings, functions
    and procedures.
    14 Jul 2000.

    Removed amigaoverlays, use smartlink instead.
    05 Nov 2002.

    Added the defines use_amiga_smartlink and
    use_auto_openlib. Implemented autoopening of
    the library.
    14 Jan 2003.

    nils.sjoholm@mailbox.swipnet.se
}

{$I useamigasmartlink.inc}
{$ifdef use_amiga_smartlink}
   {$smartlink on}
{$endif use_amiga_smartlink}

unit icon;

INTERFACE


uses exec, workbench;


Const

    ICONNAME    : PChar = 'icon.library';

VAR IconBase : pLibrary;

FUNCTION AddFreeList(freelist : pFreeList; mem : POINTER; size : ULONG) : BOOLEAN;
FUNCTION BumpRevision(newname : pCHAR; oldname : pCHAR) : pCHAR;
FUNCTION DeleteDiskObject(name : pCHAR) : BOOLEAN;
FUNCTION FindToolType(toolTypeArray : POINTER; typeName : pCHAR) : pCHAR;
PROCEDURE FreeDiskObject(diskobj : pDiskObject);
PROCEDURE FreeFreeList(freelist : pFreeList);
FUNCTION GetDefDiskObject(typ : LONGINT) : pDiskObject;
FUNCTION GetDiskObject(name : pCHAR) : pDiskObject;
FUNCTION GetDiskObjectNew(name : pCHAR) : pDiskObject;
FUNCTION MatchToolValue(typeString : pCHAR; value : pCHAR) : BOOLEAN;
FUNCTION PutDefDiskObject(diskObject : pDiskObject) : BOOLEAN;
FUNCTION PutDiskObject(name : pCHAR; diskobj : pDiskObject) : BOOLEAN;


FUNCTION BumpRevision(newname : string; oldname : pCHAR) : pCHAR;
FUNCTION BumpRevision(newname : pCHar; oldname : string) : pCHAR;
FUNCTION BumpRevision(newname : string; oldname : string) : pCHAR;
FUNCTION DeleteDiskObject(name : string) : BOOLEAN;
FUNCTION FindToolType(toolTypeArray : POINTER; typeName : string) : pCHAR;
FUNCTION GetDiskObject(name : string) : pDiskObject;
FUNCTION GetDiskObjectNew(name : string) : pDiskObject;
FUNCTION MatchToolValue(typeString : string; value : pCHAR) : BOOLEAN;
FUNCTION MatchToolValue(typeString : pCHAR; value : string) : BOOLEAN;
FUNCTION MatchToolValue(typeString : string; value : string) : BOOLEAN;
FUNCTION PutDiskObject(name : string; diskobj : pDiskObject) : BOOLEAN;


IMPLEMENTATION


uses pastoc,msgbox;

FUNCTION AddFreeList(freelist : pFreeList; mem : POINTER; size : ULONG) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L freelist,A0
    MOVEA.L mem,A1
    MOVEA.L size,A2
    MOVEA.L IconBase,A6
    JSR -072(A6)
    MOVEA.L (A7)+,A6
    TST.W   D0
    BEQ.B   @end
    MOVEQ   #1,D0
  @end: MOVE.B  D0,@RESULT
  END;
END;

FUNCTION BumpRevision(newname : pCHAR; oldname : pCHAR) : pCHAR;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L newname,A0
    MOVEA.L oldname,A1
    MOVEA.L IconBase,A6
    JSR -108(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION DeleteDiskObject(name : pCHAR) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L name,A0
    MOVEA.L IconBase,A6
    JSR -138(A6)
    MOVEA.L (A7)+,A6
    TST.W   D0
    BEQ.B   @end
    MOVEQ   #1,D0
  @end: MOVE.B  D0,@RESULT
  END;
END;

FUNCTION FindToolType(toolTypeArray : POINTER; typeName : pCHAR) : pCHAR;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L toolTypeArray,A0
    MOVEA.L typeName,A1
    MOVEA.L IconBase,A6
    JSR -096(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE FreeDiskObject(diskobj : pDiskObject);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L diskobj,A0
    MOVEA.L IconBase,A6
    JSR -090(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE FreeFreeList(freelist : pFreeList);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L freelist,A0
    MOVEA.L IconBase,A6
    JSR -054(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION GetDefDiskObject(typ : LONGINT) : pDiskObject;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  typ,D0
    MOVEA.L IconBase,A6
    JSR -120(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION GetDiskObject(name : pCHAR) : pDiskObject;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L name,A0
    MOVEA.L IconBase,A6
    JSR -078(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION GetDiskObjectNew(name : pCHAR) : pDiskObject;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L name,A0
    MOVEA.L IconBase,A6
    JSR -132(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION MatchToolValue(typeString : pCHAR; value : pCHAR) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L typeString,A0
    MOVEA.L value,A1
    MOVEA.L IconBase,A6
    JSR -102(A6)
    MOVEA.L (A7)+,A6
    TST.W   D0
    BEQ.B   @end
    MOVEQ   #1,D0
  @end: MOVE.B  D0,@RESULT
  END;
END;

FUNCTION PutDefDiskObject(diskObject : pDiskObject) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L diskObject,A0
    MOVEA.L IconBase,A6
    JSR -126(A6)
    MOVEA.L (A7)+,A6
    TST.W   D0
    BEQ.B   @end
    MOVEQ   #1,D0
  @end: MOVE.B  D0,@RESULT
  END;
END;

FUNCTION PutDiskObject(name : pCHAR; diskobj : pDiskObject) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L name,A0
    MOVEA.L diskobj,A1
    MOVEA.L IconBase,A6
    JSR -084(A6)
    MOVEA.L (A7)+,A6
    TST.W   D0
    BEQ.B   @end
    MOVEQ   #1,D0
  @end: MOVE.B  D0,@RESULT
  END;
END;


FUNCTION BumpRevision(newname : string; oldname : pCHAR) : pCHAR;
begin
      BumpRevision := BumpRevision(pas2c(newname),oldname);
end;

FUNCTION BumpRevision(newname : pCHar; oldname : string) : pCHAR;
begin
      BumpRevision := BumpRevision(newname,pas2c(oldname));
end;

FUNCTION BumpRevision(newname : string; oldname : string) : pCHAR;
begin
      BumpRevision := BumpRevision(pas2c(newname),pas2c(oldname));
end;

FUNCTION DeleteDiskObject(name : string) : BOOLEAN;
begin
      DeleteDiskObject := DeleteDiskObject(pas2c(name));
end;

FUNCTION FindToolType(toolTypeArray : POINTER; typeName : string) : pCHAR;
begin
      FindToolType := FindToolType(toolTypeArray,pas2c(typeName));
end;

FUNCTION GetDiskObject(name : string) : pDiskObject;
begin
      GetDiskObject := GetDiskObject(pas2c(name));
end;

FUNCTION GetDiskObjectNew(name : string) : pDiskObject;
begin
      GetDiskObjectNew := GetDiskObjectNew(pas2c(name)); 
end;

FUNCTION MatchToolValue(typeString : string; value : pCHAR) : BOOLEAN;
begin
       MatchToolValue := MatchToolValue(pas2c(typeString),value);
end;

FUNCTION MatchToolValue(typeString : pCHAR; value : string) : BOOLEAN;
begin
       MatchToolValue := MatchToolValue(typeString,pas2c(value));
end;

FUNCTION MatchToolValue(typeString : string; value : string) : BOOLEAN;
begin
       MatchToolValue := MatchToolValue(pas2c(typeString),pas2c(value));
end;

FUNCTION PutDiskObject(name : string; diskobj : pDiskObject) : BOOLEAN;
begin
       PutDiskObject := PutDiskObject(pas2c(name),diskobj);
end;

{$I useautoopenlib.inc}
{$ifdef use_auto_openlib}
  {$Info Compiling autoopening of icon.library}

var
    icon_exit : Pointer;

procedure CloseiconLibrary;
begin
    ExitProc := icon_exit;
    if IconBase <> nil then begin
        CloseLibrary(IconBase);
        IconBase := nil;
    end;
end;

const
    { Change VERSION and LIBVERSION to proper values }

    VERSION : string[2] = '0';
    LIBVERSION : Cardinal = 0;

begin
    IconBase := nil;
    IconBase := OpenLibrary(ICONNAME,LIBVERSION);
    if IconBase <> nil then begin
        icon_exit := ExitProc;
        ExitProc := @CloseiconLibrary
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open icon.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;

{$else}
   {$Warning No autoopening of icon.library compiled}
   {$Info Make sure you open icon.library yourself}
{$endif use_auto_openlib}


END. (* UNIT ICON *)

{
  $Log$
  Revision 1.3  2003-01-14 18:46:04  nils
  * added defines use_amia_smartlink and use_auto_openlib

  * implemented autoopening of library

  Revision 1.2  2002/11/18 20:54:32  nils
    * update check internal log

}

  

