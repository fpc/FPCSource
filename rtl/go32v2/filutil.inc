{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by the Free Pascal development team

    File utility calls

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{******************************************************************************}
{ private functions                                                            }
{******************************************************************************}

{ some internal constants }

const
   ofRead        = $0000;    { Open for reading }
   ofWrite       = $0001;    { Open for writing }
   ofReadWrite   = $0002;    { Open for reading/writing }
   faFail        = $0000;    { Fail if file does not exist }
   faCreate      = $0010;    { Create if file does not exist }
   faOpen        = $0001;    { Open if file exists }
   faOpenReplace = $0002;    { Clear if file exists }


{  converts S to a pchar and copies it to the transfer-buffer.   }

procedure StringToTB(const S: string);
var P: pchar; Len: integer;
begin
Len := Length(S) + 1;
P := StrPCopy(StrAlloc(Len), S);
SysCopyToDos(longint(P), Len);
StrDispose(P);
end ;

{  Native OpenFile function.
   if return value <> 0 call failed.  }

function OpenFile(const FileName: string; var Handle: longint; Mode, Action: word): longint;
var
   Regs: registers;
begin
result := 0;
Handle := 0;
StringToTB(FileName);
if LFNSupport then Regs.Eax:=$716c
else Regs.Eax:=$6c00;
Regs.Edx := Action;                   { Action if file exists/not exists }
Regs.Ds := tb_segment;
Regs.Esi := tb_offset;
Regs.Ebx := $2000 + (Mode and $ff);   { file open mode }
Regs.Ecx := $20;                      { Attributes }
RealIntr($21, Regs);
if Regs.Flags and CarryFlag <> 0 then result := Regs.Ax
else Handle := Regs.Eax;
end ;

{******************************************************************************}
{ Public functions                                                             }
{******************************************************************************}


Function FileOpen (Const FileName : string; Mode : Integer) : Longint;
var e: integer;
Begin
e := OpenFile(FileName, result, Mode, faOpen);
if e <> 0 then result := -1;
end ;


Function FileCreate (Const FileName : String) : Longint;
var e: integer;
begin
e := OpenFile(FileName, result, ofReadWrite, faCreate or faOpenReplace);
if e <> 0 then result := -1;
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;
begin
result := Do_Read(Handle, longint(@Buffer), Count);
end;


Function FileWrite (Handle : Longint; Var Buffer; Count : Longint) : Longint;
begin
result := Do_Write(Handle, longint(@Buffer), Count);
end;


Function FileSeek (Handle, Offset, Origin : Longint) : Longint;
var Regs: registers;
begin
Regs.Eax := $4200;
Regs.Al := Origin;
Regs.Edx := Lo(Offset);
Regs.Ecx := Hi(Offset);
Regs.Ebx := Handle;
RealIntr($21, Regs);
if Regs.Flags and CarryFlag <> 0 then
   result := -1
else begin
   LongRec(result).Lo := Regs.Eax;
   LongRec(result).Hi := Regs.Edx;
   end ;
end;


Procedure FileClose (Handle : Longint);
var Regs: registers;
begin
Regs.Eax := $3e00;
Regs.Ebx := Handle;
RealIntr($21, Regs);
end;


Function FileAge (Const FileName : String): Longint;
var Handle: longint;
begin
Handle := FileOpen(FileName, 0);
if Handle <> -1 then begin
   result := FileGetDate(Handle);
   FileClose(Handle);
   end
else result := -1;
end;


Function FileExists (Const FileName : String) : Boolean;
var Handle: longint;
begin
  //!!   This can be done quicker, need to find out how
Result := (OpenFile(FileName, Handle, ofRead, faOpen) = 0);
if Handle <> 0 then
   FileClose(Handle);
end;

Type PSearchrec = ^Searchrec;


Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;

Var Sr : PSearchrec;

begin
//!! Sr := New(PSearchRec);
getmem(sr,sizeof(searchrec));
Rslt.FindHandle := longint(Sr);
DOS.FindFirst(Path, Attr, Sr^);
result := -DosError;
if result = 0 then begin
   Rslt.Time := Sr^.Time;
   Rslt.Size := Sr^.Size;
   Rslt.Attr := Sr^.Attr;
   Rslt.ExcludeAttr := 0;
   Rslt.Name := Sr^.Name;
   end ;
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;

var Sr: PSearchRec;

begin
Sr := PSearchRec(Rslt.FindHandle);
if Sr <> nil then begin
   DOS.FindNext(Sr^);
   result := -DosError;
   if result = 0 then begin
      Rslt.Time := Sr^.Time;
      Rslt.Size := Sr^.Size;
      Rslt.Attr := Sr^.Attr;
      Rslt.ExcludeAttr := 0;
      Rslt.Name := Sr^.Name;
      end ;
   end ;
end;


Procedure FindClose (Var F : TSearchrec);

var Sr: PSearchRec;

begin
Sr := PSearchRec(F.FindHandle);
if Sr <> nil then
  //!! Dispose(Sr);
  freemem(sr,sizeof(tsearchrec));
F.FindHandle := 0;
end;


Function FileGetDate (Handle : Longint) : Longint;
var Regs: registers;
begin
  //!! for win95 an alternative function is available.
Regs.Ebx := Handle;
Regs.Eax := $5700;
RealIntr($21, Regs);
if Regs.Flags and CarryFlag <> 0 then result := -Regs.Ax
else begin
   LongRec(result).Lo := Regs.Edx;
   LongRec(result).Hi := Regs.Eax;
   end ;
end;


Function FileSetDate (Handle, Age : Longint) : Longint;
var Regs: registers;
begin
Regs.Ebx := Handle;
Regs.Eax := $5701;
Regs.Ecx := Lo(Age);
Regs.Edx := Hi(Age);
RealIntr($21, Regs);
if Regs.Flags and CarryFlag <> 0 then result := -Regs.Ax
else result := 0;
end;


Function FileGetAttr (Const FileName : String) : Longint;

var Regs: registers;

begin
  StringToTB(FileName);
  Regs.Edx := tb_offset;
  Regs.Ds := tb_segment;
  if LFNSupport then
    begin
    Regs.Ax := $7143;
    Regs.Bx := 0;
    end
  else
    Regs.Ax := $4300;
  RealIntr($21, Regs);
  if Regs.Flags and CarryFlag <> 0 then
    result := -1
  else
    result := Regs.Cx;
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;

var Regs: registers;

begin
  StringToTB(FileName);
  Regs.Edx := tb_offset;
  Regs.Ds := tb_segment;
  if LFNSupport then
    begin
    Regs.Ax := $7143;
    Regs.Bx := 1;
    end
  else
    Regs.Ax := $4301;
  Regs.Cx := Attr;
  RealIntr($21, Regs);
  if Regs.Flags and CarryFlag <> 0 then result := -Regs.Ax
  else result := 0;
end;


Function DeleteFile (Const FileName : String) : Boolean;

var Regs: registers;

begin
  StringToTB(FileName);
  Regs.Edx := tb_offset;
  Regs.Ds := tb_offset;
  if LFNSupport then
    Regs.Eax := $7141
  else
    Regs.Eax := $4100;
  Regs.Esi := 0;
  Regs.Ecx := 0;
  RealIntr($21, Regs);
  result := (Regs.Flags and CarryFlag = 0);
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;

var Regs: registers;

begin
  StringToTB(OldName + #0 + NewName);
  Regs.Edx := tb_offset;
  Regs.Ds := tb_segment;
  Regs.Edi := tb_offset + Length(OldName) + 1;
  Regs.Es := tb_segment;
  if LFNSupport then
    Regs.Eax := $7156
  else
    Regs.Eax := $5600;
  Regs.Ecx := $ff;
  RealIntr($21, Regs);
  result := (Regs.Flags and CarryFlag = 0);
end;


Function FileSearch (Const Name, DirList : String) : String;

begin
  result := DOS.FSearch(Name, DirList);
end;


{
  $Log$
  Revision 1.1  1998-12-21 13:07:02  peter
    * use -FE

  Revision 1.4  1998/10/29 13:16:19  michael
  * Fix for fileseek by gertjan schouten

  Revision 1.3  1998/10/15 09:39:13  michael
  Changes from Gretjan Schouten

  Revision 1.2  1998/10/12 08:02:16  michael
  wrong file committed

  Revision 1.1  1998/10/11 12:21:01  michael
  Added file calls. Implemented for linux only

}
