{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Sysutils unit for Go32v2

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
  go32,dos;

{ Include platform independent interface part }
{$i sysutilh.inc}


implementation

{ Include platform independent implementation part }
{$i sysutils.inc}


{****************************************************************************
                              File Functions
****************************************************************************}

{ some internal constants }

const
   ofRead        = $0000;    { Open for reading }
   ofWrite       = $0001;    { Open for writing }
   ofReadWrite   = $0002;    { Open for reading/writing }
   faFail        = $0000;    { Fail if file does not exist }
   faCreate      = $0010;    { Create if file does not exist }
   faOpen        = $0001;    { Open if file exists }
   faOpenReplace = $0002;    { Clear if file exists }

Type
  PSearchrec = ^Searchrec;

{  converts S to a pchar and copies it to the transfer-buffer.   }

procedure StringToTB(const S: string);
var
  P: pchar;
  Len: integer;
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
  else Handle := Regs.Ax;
end ;


Function FileOpen (Const FileName : string; Mode : Integer) : Longint;
var
  e: integer;
Begin
  e := OpenFile(FileName, result, Mode, faOpen);
  if e <> 0 then
    result := -1;
end;


Function FileCreate (Const FileName : String) : Longint;
var
  e: integer;
begin
  e := OpenFile(FileName, result, ofReadWrite, faCreate or faOpenReplace);
  if e <> 0 then
    result := -1;
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;
var
  regs     : registers;
  size,
  readsize : longint;
begin
  readsize:=0;
  while Count > 0 do
   begin
     if Count>tb_size then
      size:=tb_size
     else
      size:=Count;
     regs.realecx:=size;
     regs.realedx:=tb_offset;
     regs.realds:=tb_segment;
     regs.realebx:=Handle;
     regs.realeax:=$3f00;
     RealIntr($21,regs);
     if (regs.realflags and carryflag) <> 0 then
      begin
        Result:=-1;
        exit;
      end;
     syscopyfromdos(Longint(@Buffer)+readsize,lo(regs.realeax));
     inc(readsize,lo(regs.realeax));
     dec(Count,lo(regs.realeax));
     { stop when not the specified size is read }
     if lo(regs.realeax)<size then
      break;
   end;
  Result:=readsize;
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;
var
  regs      : registers;
  size,
  writesize : longint;
begin
  writesize:=0;
  while Count > 0 do
   begin
     if Count>tb_size then
      size:=tb_size
     else
      size:=Count;
     syscopytodos(Longint(@Buffer)+writesize,size);
     regs.realecx:=size;
     regs.realedx:=tb_offset;
     regs.realds:=tb_segment;
     regs.realebx:=Handle;
     regs.realeax:=$4000;
     RealIntr($21,regs);
     if (regs.realflags and carryflag) <> 0 then
      begin
        Result:=-1;
        exit;
      end;
     inc(writesize,lo(regs.realeax));
     dec(Count,lo(regs.realeax));
     { stop when not the specified size is written }
     if lo(regs.realeax)<size then
      break;
   end;
  Result:=WriteSize;
end;


Function FileSeek (Handle, FOffset, Origin : Longint) : Longint;
var
  Regs: registers;
begin
  Regs.Eax := $4200;
  Regs.Al := Origin;
  Regs.Edx := Lo(FOffset);
  Regs.Ecx := Hi(FOffset);
  Regs.Ebx := Handle;
  RealIntr($21, Regs);
  if Regs.Flags and CarryFlag <> 0 then
     result := -1
  else begin
     LongRec(result).Lo := Regs.Ax;
     LongRec(result).Hi := Regs.Dx;
     end ;
end;


Function FileSeek (Handle : Longint; FOffset,Origin : Int64) : Int64;
begin
  {$warning need to add 64bit call }
  FileSeek:=FileSeek(Handle,Longint(FOffset),Longint(Origin));
end;


Procedure FileClose (Handle : Longint);
var
  Regs: registers;
begin
  if Handle<=4 then
   exit;
  Regs.Eax := $3e00;
  Regs.Ebx := Handle;
  RealIntr($21, Regs);
end;


Function FileTruncate (Handle,Size: Longint) : boolean;
var
  regs : trealregs;
begin
  FileSeek(Handle,Size,0);
  Regs.realecx := 0;
  Regs.realedx := tb_offset;
  Regs.ds := tb_segment;
  Regs.ebx := Handle;
  Regs.eax:=$4000;
  RealIntr($21, Regs);
  FileTruncate:=(regs.realflags and carryflag)=0;
end;


Function FileAge (Const FileName : String): Longint;
var Handle: longint;
begin
  Handle := FileOpen(FileName, 0);
  if Handle <> -1 then
   begin
     result := FileGetDate(Handle);
     FileClose(Handle);
   end
  else
   result := -1;
end;


Function FileExists (Const FileName : String) : Boolean;
var Handle: longint;
begin
  //!!   This can be done quicker, need to find out how
  Result := (OpenFile(FileName, Handle, ofRead, faOpen) = 0);
  if Handle <> 0 then
    FileClose(Handle);
end;


Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;

Var Sr : PSearchrec;

begin
  //!! Sr := New(PSearchRec);
  getmem(sr,sizeof(searchrec));
  Rslt.FindHandle := longint(Sr);
  DOS.FindFirst(Path, Attr, Sr^);
  result := -DosError;
  if result = 0 then
   begin
     Rslt.Time := Sr^.Time;
     Rslt.Size := Sr^.Size;
     Rslt.Attr := Sr^.Attr;
     Rslt.ExcludeAttr := 0;
     Rslt.Name := Sr^.Name;
   end ;
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;
var
  Sr: PSearchRec;
begin
  Sr := PSearchRec(Rslt.FindHandle);
  if Sr <> nil then
   begin
     DOS.FindNext(Sr^);
     result := -DosError;
     if result = 0 then
      begin
        Rslt.Time := Sr^.Time;
        Rslt.Size := Sr^.Size;
        Rslt.Attr := Sr^.Attr;
        Rslt.ExcludeAttr := 0;
        Rslt.Name := Sr^.Name;
      end;
   end;
end;


Procedure FindClose (Var F : TSearchrec);
var
  Sr: PSearchRec;
begin
  Sr := PSearchRec(F.FindHandle);
  if Sr <> nil then
    begin
      //!! Dispose(Sr);
      // This call is non dummy if LFNSupport is true PM
      DOS.FindClose(SR^);
      freemem(sr,sizeof(searchrec));
    end;
  F.FindHandle := 0;
end;


Function FileGetDate (Handle : Longint) : Longint;
var
  Regs: registers;
begin
  //!! for win95 an alternative function is available.
  Regs.Ebx := Handle;
  Regs.Eax := $5700;
  RealIntr($21, Regs);
  if Regs.Flags and CarryFlag <> 0 then
   result := -1
  else
   begin
     LongRec(result).Lo := Regs.cx;
     LongRec(result).Hi := Regs.dx;
   end ;
end;


Function FileSetDate (Handle, Age : Longint) : Longint;
var
  Regs: registers;
begin
  Regs.Ebx := Handle;
  Regs.Eax := $5701;
  Regs.Ecx := Lo(Age);
  Regs.Edx := Hi(Age);
  RealIntr($21, Regs);
  if Regs.Flags and CarryFlag <> 0 then
   result := -Regs.Ax
  else
   result := 0;
end;


Function FileGetAttr (Const FileName : String) : Longint;
var
  Regs: registers;
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
var
  Regs: registers;
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
  if Regs.Flags and CarryFlag <> 0 then
   result := -Regs.Ax
  else
   result := 0;
end;


Function DeleteFile (Const FileName : String) : Boolean;
var
  Regs: registers;
begin
  StringToTB(FileName);
  Regs.Edx := tb_offset;
  Regs.Ds := tb_segment;
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
var
  Regs: registers;
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


{****************************************************************************
                              Disk Functions
****************************************************************************}

TYPE  ExtendedFat32FreeSpaceRec=packed Record
         RetSize           : WORD; { (ret) size of returned structure}
         Strucversion      : WORD; {(call) structure version (0000h)
                                    (ret) actual structure version (0000h)}
         SecPerClus,               {number of sectors per cluster}
         BytePerSec,               {number of bytes per sector}
         AvailClusters,            {number of available clusters}
         TotalClusters,            {total number of clusters on the drive}
         AvailPhysSect,            {physical sectors available on the drive}
         TotalPhysSect,            {total physical sectors on the drive}
         AvailAllocUnits,          {Available allocation units}
         TotalAllocUnits : DWORD;  {Total allocation units}
         Dummy,Dummy2    : DWORD;  {8 bytes reserved}
         END;

function do_diskdata(drive : byte; Free : BOOLEAN) : Int64;
VAR S    : String;
    Rec  : ExtendedFat32FreeSpaceRec;
    regs : registers;
BEGIN
 if (swap(dosversion)>=$070A) AND LFNSupport then
  begin
   DosError:=0;
   S:='C:\'#0;
   if Drive=0 then
    begin
     GetDir(Drive,S);
     Setlength(S,4);
     S[4]:=#0;
    end
   else
    S[1]:=chr(Drive+64);
   Rec.Strucversion:=0;
   dosmemput(tb_segment,tb_offset,Rec,SIZEOF(ExtendedFat32FreeSpaceRec));
   dosmemput(tb_segment,tb_offset+Sizeof(ExtendedFat32FreeSpaceRec)+1,S[1],4);
   regs.dx:=tb_offset+Sizeof(ExtendedFat32FreeSpaceRec)+1;
   regs.ds:=tb_segment;
   regs.di:=tb_offset;
   regs.es:=tb_segment;
   regs.cx:=Sizeof(ExtendedFat32FreeSpaceRec);
   regs.ax:=$7303;
   msdos(regs);
   if regs.ax<>$ffff then
    begin
      copyfromdos(rec,Sizeof(ExtendedFat32FreeSpaceRec));
      if Free then
       Do_DiskData:=int64(rec.AvailAllocUnits)*rec.SecPerClus*rec.BytePerSec
      else
       Do_DiskData:=int64(rec.TotalAllocUnits)*rec.SecPerClus*rec.BytePerSec;
    end
   else
    Do_DiskData:=-1;
  end
 else
  begin
   DosError:=0;
   regs.dl:=drive;
   regs.ah:=$36;
   msdos(regs);
   if regs.ax<>$FFFF then
    begin
     if Free then
      Do_DiskData:=int64(regs.ax)*regs.bx*regs.cx
     else
      Do_DiskData:=int64(regs.ax)*regs.cx*regs.dx;
    end
   else
    do_diskdata:=-1;
  end;
end;


function diskfree(drive : byte) : int64;
begin
   diskfree:=Do_DiskData(drive,TRUE);
end;


function disksize(drive : byte) : int64;
begin
  disksize:=Do_DiskData(drive,false);
end;


Function GetCurrentDir : String;
begin
  GetDir(0, result);
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;
begin
  {$I-}
   ChDir(NewDir);
  {$I+}
  result := (IOResult = 0);
end;


Function CreateDir (Const NewDir : String) : Boolean;
begin
  {$I-}
   MkDir(NewDir);
  {$I+}
  result := (IOResult = 0);
end;


Function RemoveDir (Const Dir : String) : Boolean;
begin
  {$I-}
   RmDir(Dir);
  {$I+}
  result := (IOResult = 0);
end;


{****************************************************************************
                              Time Functions
****************************************************************************}

Procedure GetLocalTime(var SystemTime: TSystemTime);
var
  Regs: Registers;
begin
  Regs.ah := $2C;
  RealIntr($21, Regs);
  SystemTime.Hour := Regs.Ch;
  SystemTime.Minute := Regs.Cl;
  SystemTime.Second := Regs.Dh;
  SystemTime.MilliSecond := Regs.Dl;
  Regs.ah := $2A;
  RealIntr($21, Regs);
  SystemTime.Year := Regs.Cx;
  SystemTime.Month := Regs.Dh;
  SystemTime.Day := Regs.Dl;
end ;


{****************************************************************************
                              Misc Functions
****************************************************************************}

procedure Beep;
begin
end;


{****************************************************************************
                              Locale Functions
****************************************************************************}

{  Codepage constants  }
const
   CP_US = 437;
   CP_MultiLingual = 850;
   CP_SlavicLatin2 = 852;
   CP_Turkish = 857;
   CP_Portugal = 860;
   CP_IceLand = 861;
   CP_Canada = 863;
   CP_NorwayDenmark = 865;

{  CountryInfo   }
type
   TCountryInfo = packed record
      InfoId: byte;
      case integer of
         1: ( Size: word;
              CountryId: word;
              CodePage: word;
              CountryInfo: array[0..33] of byte );
         2: ( UpperCaseTable: longint );
         4: ( FilenameUpperCaseTable: longint );
         5: ( FilecharacterTable: longint );
         6: ( CollatingTable: longint );
         7: ( DBCSLeadByteTable: longint );
   end ;


procedure GetExtendedCountryInfo(InfoId: integer; CodePage, CountryId: word; var CountryInfo: TCountryInfo);

Var Regs: Registers;

begin
  Regs.AH := $65;
  Regs.AL := InfoId;
  Regs.BX := CodePage;
  Regs.DX := CountryId;
  Regs.ES := transfer_buffer div 16;
  Regs.DI := transfer_buffer and 15;
  Regs.CX := SizeOf(TCountryInfo);
  RealIntr($21, Regs);
  DosMemGet(transfer_buffer div 16,
            transfer_buffer and 15,
            CountryInfo, Regs.CX );
end;


procedure InitAnsi;
var
  CountryInfo: TCountryInfo; i: integer;
begin
  {  Fill table entries 0 to 127  }
  for i := 0 to 96 do
    UpperCaseTable[i] := chr(i);
  for i := 97 to 122 do
    UpperCaseTable[i] := chr(i - 32);
  for i := 123 to 127 do
    UpperCaseTable[i] := chr(i);
  for i := 0 to 64 do
    LowerCaseTable[i] := chr(i);
  for i := 65 to 90 do
    LowerCaseTable[i] := chr(i + 32);
  for i := 91 to 255 do
    LowerCaseTable[i] := chr(i);

  {  Get country and codepage info  }
  GetExtendedCountryInfo(1, $FFFF, $FFFF, CountryInfo);
  if CountryInfo.CodePage = 850 then
    begin
    { Special, known case }
    Move(CP850UCT, UpperCaseTable[128], 128);
    Move(CP850LCT, LowerCaseTable[128], 128);
    end
  else
    begin
    { this needs to be checked !!
    this is correct only if UpperCaseTable is
    and Offset:Segment word record (PM) }
    {  get the uppercase table from dosmemory  }
    GetExtendedCountryInfo(2, $FFFF, $FFFF, CountryInfo);
    DosMemGet(CountryInfo.UpperCaseTable shr 16, 2 + CountryInfo.UpperCaseTable and 65535, UpperCaseTable[128], 128);
    for i := 128 to 255 do
       begin
       if UpperCaseTable[i] <> chr(i) then
          LowerCaseTable[ord(UpperCaseTable[i])] := chr(i);
       end;
    end;
end;


Procedure InitInternational;
begin
  InitAnsi;
end;

function SysErrorMessage(ErrorCode: Integer): String;

begin
  Result:=Format(SUnknownErrorCode,[ErrorCode]);
end;

{****************************************************************************
                              Os utils
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : String) : String;

begin
  Result:=getenv(EnvVar);
end;

{****************************************************************************
                              Initialization code
****************************************************************************}

Initialization
  InitExceptions;       { Initialize exceptions. OS independent }
  InitInternational;    { Initialize internationalization settings }
Finalization
  DoneExceptions;
end.
{
  $Log$
  Revision 1.8  2002-01-25 16:23:03  peter
    * merged filesearch() fix

  Revision 1.7  2002/01/19 11:57:55  peter
    * merged fixes

  Revision 1.6  2001/10/25 21:23:49  peter
    * added 64bit fileseek

  Revision 1.5  2001/06/03 15:18:01  peter
    * eoutofmemory and einvalidpointer fix

  Revision 1.4  2001/02/20 22:14:19  peter
    * merged getenvironmentvariable

  Revision 1.3  2000/08/30 06:29:19  michael
  + Merged syserrormsg fix


    Revision 1.2  2000/08/20 15:46:46  peter
    * sysutils.pp moved to target and merged with disk.inc, filutil.inc

    Revision 1.1.2.2  2000/08/22 19:21:48  michael
    + Implemented syserrormessage. Made dummies for go32v2 and OS/2
    * Changed linux/errors.pp so it uses pchars for storage.

    Revision 1.1.2.1  2000/08/20 15:08:32  peter
      * forgot the add command :(

}
