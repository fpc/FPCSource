{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

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

IMPLEMENTATION

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

END. (* UNIT ICON *)



