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


{This is the correct way to call external assembler procedures.}
procedure syscall;external name '___SYSCALL';


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
(* DenyAll if sharing not specified. *)
    if Mode and 112 = 0 then
        Mode := Mode or 16;
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
        mov [ebp - 4], eax
    end;
{$ENDIF}
end;


function FileCreate (const FileName: string): longint;
{$IFOPT H+}
                                                                    assembler;
{$ELSE}
var FN: string;
begin
    FN := FileName + #0;
(* DenyAll if sharing not specified. *)
    if Mode and 112 = 0 then
        Mode := Mode or 16;
{$ENDIF}
    asm
        mov eax, 7F2Bh
        mov ecx, ofReadWrite or faCreate
{$IFOPT H+}
        mov edx, FileName
{$ELSE}
        lea edx, FN
        inc edx
{$ENDIF}
        call syscall
{$IFOPT H-}
        mov [ebp - 4], eax
    end;
{$ENDIF}
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


function FileSeek (Handle, FOffset, Origin: longint): longint; assembler;
asm
    mov eax, Origin
    mov ah, 42h
    mov ebx, Handle
    mov edx, FOffset
    call syscall
    jnc @FSeekEnd
    mov eax, -1
@FSeekEnd:
end;


procedure FileClose (Handle: longint);
begin
    if (Handle <= 4) or (os_mode = osOS2) and (Handle <= 2) then
        asm
            mov eax, 3E00h
            mov ebx, Handle
            call syscall
        end;
end;


function FileTruncate (Handle, Size: longint): boolean; assembler;
asm
    mov eax, 7F25h
    mov ebx, Handle
    mov edx, Size
    call syscall
    jc @FTruncEnd
    mov eax, 4202h
    mov ebx, Handle
    mov edx, 0
    call syscall
    mov eax, 0
    jnc @FTruncEnd
    dec eax
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


type    TRec = record
            T, D: word;
        end;
        PSearchRec = ^SearchRec;

function FindFirst (const Path: string; Attr: longint; var Rslt: TSearchRec): longint;

var SR: PSearchRec;
    FStat: PFileFindBuf3;
    Count: longint;
    Err: longint;

begin
    if os_mode = osOS2 then
        begin
            New (FStat);
            Rslt.FindHandle := $FFFFFFFF;
            Count := 1;
            Err := DosFindFirst (Path, Rslt.FindHandle, Attr, FStat,
                                           SizeOf (FStat^), Count, ilStandard);
            if (Err = 0) and (Count = 0) then Err := 18;
            FindFirst := -Err;
            if Err = 0 then
                begin
                    Rslt.Name := FStat^.Name;
                    Rslt.Size := FStat^.FileSize;
                    Rslt.Attr := FStat^.AttrFile;
                    Rslt.ExcludeAttr := 0;
                    TRec (Rslt.Time).T := FStat^.TimeLastWrite;
                    TRec (Rslt.Time).D := FStat^.DateLastWrite;
                end;
            Dispose (FStat);
        end
    else
        begin
            GetMem (SR, SizeOf (SearchRec));
            Rslt.FindHandle := longint(SR);
            DOS.FindFirst (Path, Attr, SR^);
            FindFirst := -DosError;
            if DosError = 0 then
                begin
                    Rslt.Time := SR^.Time;
                    Rslt.Size := SR^.Size;
                    Rslt.Attr := SR^.Attr;
                    Rslt.ExcludeAttr := 0;
                    Rslt.Name := SR^.Name;
                end;
        end;
end;


function FindNext (var Rslt: TSearchRec): longint;

var SR: PSearchRec;
    FStat: PFileFindBuf3;
    Count: longint;
    Err: longint;

begin
    if os_mode = osOS2 then
        begin
            New (FStat);
            Count := 1;
            Err := DosFindNext (Rslt.FindHandle, FStat, SizeOf (FStat), Count);
            if (Err = 0) and (Count = 0) then Err := 18;
            FindNext := -Err;
            if Err = 0 then
                begin
                    Rslt.Name := FStat^.Name;
                    Rslt.Size := FStat^.FileSize;
                    Rslt.Attr := FStat^.AttrFile;
                    Rslt.ExcludeAttr := 0;
                    TRec (Rslt.Time).T := FStat^.TimeLastWrite;
                    TRec (Rslt.Time).D := FStat^.DateLastWrite;
                end;
            Dispose (FStat);
        end
    else
        begin
            SR := PSearchRec (Rslt.FindHandle);
            if SR <> nil then
                begin
                    DOS.FindNext (SR^);
                    FindNext := -DosError;
                    if DosError = 0 then
                        begin
                            Rslt.Time := SR^.Time;
                            Rslt.Size := SR^.Size;
                            Rslt.Attr := SR^.Attr;
                            Rslt.ExcludeAttr := 0;
                            Rslt.Name := SR^.Name;
                        end;
                end;
        end;
end;


procedure FindClose (var F: TSearchrec);

var SR: PSearchRec;

begin
    if os_mode = osOS2 then
        begin
            DosFindClose (F.FindHandle);
        end
    else
        begin
            DOS.FindClose (SR^);
            FreeMem (SR, SizeOf (SearchRec));
        end;
    F.FindHandle := 0;
end;


function FileGetDate (Handle: longint): longint; assembler;
asm
    mov ax, 5700h
    mov ebx, Handle
    call syscall
    mov eax, -1
    jc @FGetDateEnd
    mov ax, dx
    shld eax, ecx, 16
@FGetDateEnd:
end;


function FileSetDate (Handle, Age: longint): longint;
var FStat: PFileStatus0;
    RC: longint;
begin
    if os_mode = osOS2 then
        begin
            New (FStat);
            RC := DosQueryFileInfo (Handle, ilStandard, FStat,
                                                              SizeOf (FStat^));
            if RC <> 0 then
                FileSetDate := -1
            else
                begin
                    FStat^.DateLastAccess := Hi (Age);
                    FStat^.DateLastWrite := Hi (Age);
                    FStat^.TimeLastAccess := Lo (Age);
                    FStat^.TimeLastWrite := Lo (Age);
                    RC := DosSetFileInfo (Handle, ilStandard, FStat,
                                                              SizeOf (FStat^));
                    if RC <> 0 then
                        FileSetDate := -1
                    else
                        FileSetDate := 0;
                end;
            Dispose (FStat);
        end
    else
        asm
            mov ax, 5701h
            mov ebx, Handle
            mov cx, word ptr [Age]
            mov dx, word ptr [Age + 2]
            call syscall
            jnc @FSetDateEnd
            mov eax, -1
@FSetDateEnd:
            mov [ebp - 4], eax
        end;
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
    mov [ebp - 4], eax
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
    mov [ebp - 4], eax
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
@FDeleteEnd:
{$IFOPT H-}
    mov [ebp - 4], eax
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
    mov [ebp - 4], eax
end;
{$ENDIF}
end;


function FileSearch (const Name, DirList: string): string;
begin
    Result := Dos.FSearch (Name, DirList);
end;


procedure GetLocalTime (var SystemTime: TSystemTime); assembler;
asm
(* Expects the default record alignment (DWord)!!! *)
    mov ah, 2Ah
    call syscall
    mov edi, SystemTime
    xor eax, eax
    mov ax, cx
    stosd
    xor eax, eax
    mov al, dh
    stosd
    mov al, dl
    stosd
    push edi
    mov ah, 2Ch
    call syscall
    pop edi
    xor eax, eax
    mov al, ch
    stosd
    mov al, cl
    stosd
    mov al, dh
    stosd
    mov al, dl
    stosd
end;

procedure InitAnsi;
var I: byte;
    Country: TCountryCode;
begin
    for I := 0 to 255 do
        UpperCaseTable [I] := Chr (I);
    Move (UpperCaseTable, LowerCaseTable, SizeOf (UpperCaseTable));
    if os_mode = osOS2 then
        begin
            FillChar (Country, SizeOf (Country), 0);
            DosMapCase (SizeOf (UpperCaseTable), Country, @UpperCaseTable);
        end
    else
        begin
(* !!! TODO: DOS/DPMI mode support!!! *)
        end;
    for I := 0 to 255 do
        if UpperCaseTable [I] <> Chr (I) then
            LowerCaseTable [Ord (UpperCaseTable [I])] := Chr (I);
end;

procedure InitInternational;
var Country: TCountryCode;
    CtryInfo: TCountryInfo;
    Size: cardinal;
    RC: longint;
begin
    Size := 0;
    FillChar (Country, SizeOf (Country), 0);
    FillChar (CtryInfo, SizeOf (CtryInfo), 0);
    RC := DosQueryCtryInfo (SizeOf (CtryInfo), Country, CtryInfo, Size);
    if RC = 0 then
        begin
            DateSeparator := CtryInfo.DateSeparator;
            case CtryInfo.DateFormat of
             1: begin
                    ShortDateFormat := 'd/m/y';
                    LongDateFormat := 'dd" "mmmm" "yyyy';
                end;
             2: begin
                    ShortDateFormat := 'y/m/d';
                    LongDateFormat := 'yyyy" "mmmm" "dd';
                end;
             3: begin
                    ShortDateFormat := 'm/d/y';
                    LongDateFormat := 'mmmm" "dd" "yyyy';
                end;
            end;
            TimeSeparator := CtryInfo.TimeSeparator;
            DecimalSeparator := CtryInfo.DecimalSeparator;
            ThousandSeparator := CtryInfo.ThousandSeparator;
            CurrencyFormat := CtryInfo.CurrencyFormat;
            CurrencyString := PChar (CtryInfo.CurrencyUnit);
        end;
    InitAnsi;
end;


{
  $Log$
  Revision 1.1  2000-07-13 06:31:05  michael
  + Initial import

  Revision 1.13  2000/07/06 19:03:40  hajny
    * filutil.inc implementation (almost) finished

  Revision 1.12  2000/06/05 18:57:38  hajny
    * handle number check added to FileClose

  Revision 1.11  2000/06/04 15:04:22  hajny
    * another bunch of corrections

  Revision 1.10  2000/06/04 14:22:02  hajny
    * minor corrections

  Revision 1.9  2000/06/01 18:36:50  hajny
    * FileGetDate added

  Revision 1.8  2000/05/29 17:59:58  hajny
    * FindClose implemented

  Revision 1.7  2000/05/28 18:22:58  hajny
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
