{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    File utility calls

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


const
 ofRead        = $0000;     {Open for reading}
 ofWrite       = $0001;     {Open for writing}
 ofReadWrite   = $0002;     {Open for reading/writing}
 faCreateNew   = $00010000; {Create if file does not exist}
 faOpenReplace = $00040000; {Truncate if file exists}
 faCreate      = $00050000; {Create if file does not exist, truncate otherwise}

{$ASMMODE INTEL}
function FileOpen (const FileName: string; Mode: integer): longint;
{$IFOPT H+}
                                                                    assembler;
{$ELSE}
var FN: string;
begin
    FN := FileName + #0;
{$ENDIF}
    asm
         mov eax, 7F2Bh
         mov ecx, Mode
{$IFOPT H+}
         mov edx, FileName
{$ELSE}
         lea edx, FN
         inc edx
{$ENDIF}
         call syscall
{$IFOPT H-}
    end;
{$ELSE}
end;


function FileCreate (const FileName: string): longint;
var FN: string;
begin
    FN := FileName + #0;
    asm
         mov eax, 7F2Bh
         mov ecx, ofReadWrite or faCreate
         lea edx, FN
         inc edx
         call syscall
    end;
end;


function FileRead (Handle: longint; var Buffer; Count: longint): longint;
                                                                     assembler;
asm
    mov eax, 3F00h
    mov ebx, Handle
    mov ecx, Count
    mov edx, Buffer
    call syscall
    jnc @FReadEnd
    mov eax, -1
@FReadEnd:
end;


function FileWrite (Handle: longint; const Buffer; Count: longint): longint;
                                                                     assembler;
asm
    mov eax, 4000h
    mov ebx, Handle
    mov ecx, Count
    mov edx, Buffer
    call syscall
    jnc @FWriteEnd
    mov eax, -1
@FWriteEnd:
end;


function FileSeek (Handle, Offset, Origin: longint): longint; assembler;
asm
    mov eax, Origin
    mov ah, 42h
    mov ebx, Handle
    mov edx, Offset
    call syscall
    jnc @FSeekEnd
    mov eax, -1
@FSeekEnd:
end;


procedure FileClose (Handle: longint); assembler;
asm
    mov eax, 3E00h
    mov ebx, Handle
    call syscall
end;


function FileTruncate (Handle, Size: longint): boolean; assembler;
asm
    mov eax, 7F25h
    mov ebx, Handle
    mov edx, Size
    call syscall
    mov eax, 0
    jc @FTruncEnd
    inc eax
@FTruncEnd:
end;


function FileAge (const FileName: string): longint;
var Handle: longint;
begin
    Handle := FileOpen (FileName, 0);
    if Handle <> -1 then
        begin
            Result := FileGetDate (Handle);
            FileClose (Handle);
        end
    else
        Result := -1;
end;


function FileExists (const FileName: string): boolean;
{$IFOPT H+}
                                                       assembler;
{$ELSE}
var FN: string;
begin
    FN := FileName + #0;
{$ENDIF}
asm
    mov ax, 4300h
{$IFOPT H+}
    mov edx, FileName
{$ELSE}
    lea edx, FN
    inc edx
{$ENDIF}
    call syscall
    mov eax, 0
    jc @FExistsEnd
    test cx, 18h
    jnz @FExistsEnd
    inc eax
@FExistsEnd:
{$IFOPT H-}
end;
{$ENDIF}
end;


Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;

begin
  //!! Needs implementing
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;

begin
  //!! Needs implementing
end;


Procedure FindClose (Var F : TSearchrec);

begin
  //!! Needs implementing
end;


Function FileGetDate (Handle : Longint) : Longint;

begin
  //!! Needs implementing
end;


Function FileSetDate (Handle,Age : Longint) : Longint;

begin
  //!! Needs implementing
end;


function FileGetAttr (const FileName: string): longint;
{$IFOPT H+}
                                                        assembler;
{$ELSE}
var FN: string;
begin
    FN := FileName + #0;
{$ENDIF}
asm
    mov ax, 4300h
{$IFOPT H+}
    mov edx, FileName
{$ELSE}
    lea edx, FN
    inc edx
{$ENDIF}
    call syscall
    jnc @FGetAttrEnd
    mov eax, -1
@FGetAttrEnd:
{$IFOPT H-}
end;
{$ENDIF}
end;


function FileSetAttr (const Filename: string; Attr: longint): longint;
{$IFOPT H+}
                                                                     assembler;
{$ELSE}
var FN: string;
begin
    FN := FileName + #0;
{$ENDIF}
asm
    mov ax, 4301h
    mov ecx, Attr
{$IFOPT H+}
    mov edx, FileName
{$ELSE}
    lea edx, FN
    inc edx
{$ENDIF}
    call syscall
    mov eax, 0
    jnc @FSetAttrEnd
    mov eax, -1
@FSetAttrEnd:
{$IFOPT H-}
end;
{$ENDIF}
end;


function DeleteFile (const FileName: string): boolean;
{$IFOPT H+}
                                                       assembler;
{$ELSE}
var FN: string;
begin
    FN := FileName + #0;
{$ENDIF}
asm
    mov ax, 4100h
{$IFOPT H+}
    mov edx, FileName
{$ELSE}
    lea edx, FN
    inc edx
{$ENDIF}
    call syscall
    mov eax, 0
    jc @FDeleteEnd
    inc eax
@FExistsEnd:
{$IFOPT H-}
end;
{$ENDIF}
end;


function RenameFile (const OldName, NewName: string): boolean;
{$IFOPT H+}
                                                       assembler;
{$ELSE}
var FN1, FN2: string;
begin
    FN1 := OldName + #0;
    FN2 := NewName + #0;
{$ENDIF}
asm
    mov ax, 5600h
{$IFOPT H+}
    mov edx, OldName
    mov edi, NewName
{$ELSE}
    lea edx, FN1
    inc edx
    lea edi, FN2
    inc edi
{$ENDIF}
    call syscall
    mov eax, 0
    jc @FRenameEnd
    inc eax
@FRenameEnd:
{$IFOPT H-}
end;
{$ENDIF}
end;


function FileSearch (const Name, DirList: string): string;
begin
    Result := Dos.FSearch (Name, DirList);
end;


Procedure GetLocalTime(var SystemTime: TSystemTime);

begin
  //!! Needs implementing
end ;

Procedure InitAnsi;
(* __nls_ctype ??? *)
begin
  //!! Needs implementing
end;

Procedure InitInternational;
begin
  InitAnsi;
end;


{
  $Log$
  Revision 1.7  2000-05-28 18:22:58  hajny
    + implementation started

  Revision 1.6  2000/02/17 22:16:05  sg
  * Changed the second argument of FileWrite from "var buffer" to
    "const buffer", like in Delphi.

  Revision 1.5  2000/02/09 16:59:33  peter
    * truncated log

  Revision 1.4  2000/01/07 16:41:47  daniel
    * copyright 2000

  Revision 1.3  1999/11/08 22:45:55  peter
    * updated

}
