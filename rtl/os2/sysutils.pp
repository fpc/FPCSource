{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Sysutils unit for OS/2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sysutils;
interface

{$MODE objfpc}
{ force ansistrings }
{$H+}

uses
  doscalls,dos;

{ Include platform independent interface part }
{$i sysutilh.inc}


implementation

{ Include platform independent implementation part }
{$i sysutils.inc}


{****************************************************************************
                              File Functions
****************************************************************************}

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


{****************************************************************************
                              Disk Functions
****************************************************************************}

{$ASMMODE ATT}

function DiskFree (Drive: byte): int64;

var FI: TFSinfo;
    RC: longint;

begin
    if (os_mode = osDOS) or (os_mode = osDPMI) then
    {Function 36 is not supported in OS/2.}
        asm
            movb 8(%ebp),%dl
            movb $0x36,%ah
            call syscall
            cmpw $-1,%ax
            je .LDISKFREE1
            mulw %cx
            mulw %bx
            shll $16,%edx
            movw %ax,%dx
            xchgl %edx,%eax
            leave
            ret
         .LDISKFREE1:
            cltd
            leave
            ret
        end
    else
        {In OS/2, we use the filesystem information.}
        begin
            RC := DosQueryFSInfo (Drive, 1, FI, SizeOf (FI));
            if RC = 0 then
                DiskFree := int64 (FI.Free_Clusters) *
                   int64 (FI.Sectors_Per_Cluster) * int64 (FI.Bytes_Per_Sector)
            else
                DiskFree := -1;
        end;
end;

function DiskSize (Drive: byte): int64;

var FI: TFSinfo;
    RC: longint;

begin
    if (os_mode = osDOS) or (os_mode = osDPMI) then
        {Function 36 is not supported in OS/2.}
        asm
            movb 8(%ebp),%dl
            movb $0x36,%ah
            call syscall
            movw %dx,%bx
            cmpw $-1,%ax
            je .LDISKSIZE1
            mulw %cx
            mulw %bx
            shll $16,%edx
            movw %ax,%dx
            xchgl %edx,%eax
            leave
            ret
        .LDISKSIZE1:
            cltd
            leave
            ret
        end
    else
        {In OS/2, we use the filesystem information.}
        begin
            RC := DosQueryFSinfo (Drive, 1, FI, SizeOf (FI));
            if RC = 0 then
                DiskSize := int64 (FI.Total_Clusters) *
                   int64 (FI.Sectors_Per_Cluster) * int64 (FI.Bytes_Per_Sector)
            else
                DiskSize := -1;
        end;
end;


function GetCurrentDir: string;
begin
 GetDir (0, Result);
end;


function SetCurrentDir (const NewDir: string): boolean;
begin
{$I-}
 ChDir (NewDir);
 Result := (IOResult = 0);
{$I+}
end;


function CreateDir (const NewDir: string): boolean;
begin
{$I-}
 MkDir (NewDir);
 Result := (IOResult = 0);
{$I+}
end;


function RemoveDir (const Dir: string): boolean;
begin
{$I-}
 RmDir (Dir);
 Result := (IOResult = 0);
 {$I+}
end;


{****************************************************************************
                              Time Functions
****************************************************************************}

{$asmmode intel}
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
{$asmmode default}


{****************************************************************************
                              Misc Functions
****************************************************************************}

procedure Beep;
begin
end;


{****************************************************************************
                              Locale Functions
****************************************************************************}

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


{****************************************************************************
                              Initialization code
****************************************************************************}

Initialization
  InitExceptions;       { Initialize exceptions. OS independent }
  InitInternational;    { Initialize internationalization settings }
Finalization
  OutOfMemory.Free;
  InValidPointer.Free;
end.
{
  $Log$
  Revision 1.2  2000-08-20 15:46:46  peter
    * sysutils.pp moved to target and merged with disk.inc, filutil.inc

}
