{
   $Id$

   Unit to access the file system
   All file operations except those on open files (see FileCtrl for that)

   Copyright by Marco Schmidt <marco@pool.informatik.rwth-aachen.de>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.


   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}
unit FileSys;

interface

{$I platform.inc} { Conditional directives :
                    compiler, operating system }

uses
  ApiComm          { Error handling }

{$IFDEF PPC_FPC}
  , Strings
{$ENDIF}

{$IFDEF OS_DOS}
  , DOS           { GetFAttr, GetFTime, FindFirst, FindNext, ... }
{$else not OS_DOS}
 {$ifdef PPC_FPC}
  {$ifdef OS_WINDOWS}
  {$define OS_DOS}
  , DOS
  {$endif OS_WIN32}
 {$endif PPC_FPC}
{$ENDIF}

{$IFDEF OS_Unix}
  , linux
{$ENDIF}
  ;


const

  { Maximum length of a file name (must be <= 255, for we use
    standard Pascal strings) }
  MaxNameLength = {$IFDEF PPC_BP}
                  79;
                  {$ELSE}
                  255;
                  {$ENDIF}

  { Character to separate directories in a path }
  PathSeparator = {$IFDEF OS_Unix}
                  '/';
                  {$ELSE}
                  '\';
                  {$ENDIF}

  { Defines if a character is inserted into a number string every three
    digits;
    true :  returns "3,555,234"
    false : returns "3555234" }
  SeparateThousands : Boolean = true;

  { Character to be used to separate three digits in FileIntToString }
  ThousandsSeparator : Char = ',';

  { "CheckName" function return values }
  cnUnknown     = 0;
  cnFile        = 1;
  cnDirectory   = 2;

  { File attribute bit masks }

  faReadOnly    = $0001;
  faSystem      = $0002;
  faHidden      = $0004;
  faVolumeID    = $0008;
  faDirectory   = $0010;
  faArchive     = $0020;
  faAnyFile     = faReadOnly or
                  faSystem or
                  faHidden or
                  faVolumeID or
                  faDirectory or
                  faArchive;      { = $003f }

  { Wildcard characters for use with "ContainsWildcards" }

  NumWildcardChars = 2;
  WildcardChars : Array[0..NumWildcardChars-1] of Char =
      ('*', '?');

type
  { file attribute type }
  TFileAttr = {$IFDEF PPC_BP}
              Word;               { DOS: RSHVAD }
              {$ELSE}
              Longint;            { Any other OS }
              {$ENDIF}

  { Stores date and time in a system-independent way }
  TDateTime = packed record
    DOW    : Byte; { 0=Sunday, 1=Monday, ... }
    Day    : Byte; { 1..31 }
    Month  : Byte; { 1..12 }
    Year   : Word; { 1601..3999 }
    IsLeap : Boolean; { is "Year" a leap year ? }
    Hour   : Byte; { 0..23 }
    Minute : Byte; { 0..59 }
    Second : Byte; { 0..59 }
    Valid  : Boolean; { set by "CheckDateTime" }
  end;

  { Stores file size & offset values;
    may have to be changed for other environments }
  TFileInt  = Longint; { 32 bit signed, as we have no unsigned 32 bit type }

  { directory / file name }
  TFileName = String[MaxNameLength];

  { record to describe a file or directory entry;
    used in combination with a file search }
  TFileDescriptor = packed record
    { fields available for all platforms }
    Attr             : TFileAttr;
    IsDirectory      : Boolean;
    LastModification : TDateTime;
    Name             : TFileName;
    Size             : TFileInt;
    { platform-specific fields }
    {$IFDEF OS_Unix}
    Created          : TDateTime;
    LastAccessed     : TDateTime;
    {$ENDIF OS_Unix}
  end;

  { Search record declaration for FPC for DOS (we're not using the DOS unit
    that provides SearchRec) }

  {$IFDEF PPC_FPC}
    {$IFDEF OS_DOS}

    type
      TDOSSearchRec = packed record
        Fill:     Array[1..21] of Byte;
        Attr:     Byte;
        Time:     Longint;
        Reserved: Word; { requires the DOS extender (DJ GNU-C) }
        Size:     Longint;
        Name:     String[15]; { the same size as declared by (DJ GNU C) }
      end;

    {$ENDIF OS_DOS}
  {$ENDIF PPC_FPC}

  { File search record to be used with
    StartSearch, ContinueSearch and TerminateSearch }

  TFileSearch = packed record
    { Input fields for all platforms }
    Specs   : TFileName;
    { OS-specific input fields }
    {$IFDEF OS_DOS}
    Attr    : TFileAttr;
    {$ENDIF}

    { Output fields for all platforms }
    FD      : TFileDescriptor;
    Success : Boolean;

    { OS-specific output fields }

    {$IFDEF OS_Unix}
    GL : PGlob;
    {$ELSE OS_Unix}
    SR      : DOS.SearchRec;
    {$ENDIF OS_Unix}
  end;

procedure CheckDateTime(var DT: TDateTime);
function  CheckName(AName: TFileName): Byte;
function  ContainsWildcards(AName: TFileName): Boolean;
procedure ContinueSearch(var FS: TFileSearch);
procedure CreateDir(AName: TFileName);
function  DateToString(const DT: TDateTime): String;
procedure DeleteDir(AName: TFileName);
procedure DeleteFile(AName: TFileName);
function  EqualNames(Name1, Name2: TFileName): Boolean;
function  Exists(AName: TFileName): Boolean;
function  ExpandName(AName: TFileName): TFileName;
function  FileAttrToString(AFileAttr: TFileAttr): String;
function  FileIntToString(FI: TFileInt): String;
function  GetCurrentDir: TFileName;
procedure GetFAttr(AName: TFileName; var Attr: TFileAttr);
procedure GetFTime(AName: TFileName; var DT: TDateTime);
function  IsValidName(AName: TFileName) : Boolean;
procedure RenameDir(OldName, NewName: TFileName);
procedure RenameFile(OldName, NewName: TFileName);
procedure SetCurrentDir(AName: TFileName);
procedure SetFAttr(AName: TFileName; AFileAttr: TFileAttr);
procedure SetFTime(AName: TFileName; DT: TDateTime);
procedure SplitName(AName: TFileName; var Path, RawName, Extension: TFileName);
procedure StartSearch(var FS: TFileSearch);
procedure TerminateSearch(var FS: TFileSearch);
function  TimeToString(DT: TDateTime): String;

implementation

{ Structure of the implementation section
  ---------------------------------------
  - proc. & functions that do not appear in the interface section and
    are the same for all platforms
  - proc. & functions that do appear in the interface section and
    are the same for all platforms
  - proc. & functions that do not appear in the interface section and
    are DOS-specific
  - proc. & functions that do appear in the interface section and
    are not the same for all platforms
}

{ procedures and functions that do not appear in the interface section and
  are the same for all platforms }

function weekday(y,m,d : longint) : longint;

{ Calculates th day of the week. Florian provided this.
  returns -1 on error }

    var
       century_offset : integer;
       temp : longint;
       _is_leap_year : boolean;

    const
       month_table : array[1..12] of longint = (1,4,4,0,2,5,0,3,6,1,4,6);

  function is_leap_year(y : longint) : boolean;

    begin
       if (y mod 100)=0 then
         is_leap_year:=((y mod 400)=0)
       else
         is_leap_year:=(y mod 4)=0;
    end;

  { Beginning of weekday }
  begin
       if (m<1) or (m>12) then
         begin
            weekday:=-1;
            exit;
         end;
       case y of
          1700..1799 : century_offset:=4;
          1800..1899 : century_offset:=2;
          1900..1999 : century_offset:=0;
          2000..2099 : century_offset:=-1;
          else
            begin
               if (y>=2100) then
                 begin
                 end;
               weekday:=-1;
               exit;
            end;
       end;
       _is_leap_year:=is_leap_year(y);
       y:=y mod 100;
       temp:=(y div 12)+(y mod 12)+((y mod 12) div 4);
       temp:=temp mod 7;
       temp:=(temp+month_table[m]+d) mod 7;
       { do some corrections for special years }
       { other century ? }
       inc(temp,century_offset);
       { leap year correction }
       if _is_leap_year and (m<3) then
         dec(temp);
       { now is sonday 1, but should be for example 0 }
       dec(temp);
       { the result could be less than zero }
       while temp<0 do
         inc(temp,7);
       weekday:=temp mod 7;
    end;


{ Returns Longint value as String }
function LongToStr(L: Longint): String;
var
  S: String[20];
begin
  System.Str(L, S);
  LongToStr := S;
end;

{ Returns Longint value as String, adding a leading '0' character if value
  is >= 0 and <= 9 (LZ = leading zero) }
function LongToStrLZ(L: Longint): String;
var
  Z: String[1];
begin
  if (L >= 0) and (L <= 9)
    then Z := '0'
    else Z := '';
  LongToStrLZ := Z + LongToStr(L);
end;

{ Procedures and functions that do appear in the interface section and are
  the same for all platforms }

{ Checks if date and time in "dt" is valid; also determines the day of the
  week }
procedure CheckDateTime(var DT: TDateTime);
const
  MonthLength : array[1..12] of Byte =
    (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  DT.Valid := false;
  { check data that is within a fixed range }
  with DT do
    if (Hour < 0) or (Hour > 23) or
       (Minute < 0) or (Minute > 59) or
       (Second < 0) or (Second > 59) or
       (Month < 1) or (Month > 12) or
       (Day < 1) or
       (Year < 1600) or (Year > 3999)
      then  exit;
  { determine if year is leap year }
  DT.IsLeap := ((dt.Year mod 4) = 0) and
                 (not (((dt.Year mod 100)  = 0) and
                      ((dt.Year mod 400) <> 0)));
  { check if day is within limits }
  if ( DT.IsLeap      and (dt.Month = 2) and (dt.Day > 29)) or
     ((not dt.IsLeap) and (dt.Day > MonthLength[dt.Month]))
    then exit;
  { date seems to be alright, compute day of the week
    (formula taken from DDJ 06/95 [#231], p.11) }
  if weekday (dt.year,dt.month,dt.day)<0 then
     dt.dow:=0
  else
     dt.dow:=weekday(dt.year,dt.month,dt.day);
{  Removed - caused segfault in linux. Michael.

  dt.DOW := (((( 3 * (dt.Year) - ( 7 * ((dt.Year) +
            ((dt.Month)+9) div 12)) div 4 +
            (23 * (dt.Month)) div 9 + (dt.Day) + 2 +
            (((dt.Year) - Ord ((dt.Month) < 3)) div 100 + 1)
             * 3 div 4 - 16 ) + 1 ) mod 7));
}
  dt.Valid := true;
end;

{ Returns if AName contains at least one of the characters from global
  constant WildcardChars }
function ContainsWildcards(AName: TFileName): Boolean;
var
  I, J: Longint;
begin
  ContainsWildcards := false;
  if (Length(AName) = 0)
    then exit;
  { compare each character in AName with each character in WildCards }
  for I := 1 to Length (AName) do
    for J := 0 to NumWildcardChars-1 do
      if (AName[I] = WildcardChars[J])
        then begin
               ContainsWildcards := true;
               exit;
             end;
end;

{ Returns date part of TDateTime as String : "Tue 29 Jul 1997" }
function DateToString(const DT: TDateTime): String;
const
  DOWNames : array[0..6] of String[3] =
    ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  MonthNames : array[1..12] of String[3] =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
begin
  if DT.Valid
    then DateToString := DOWNames [dt.DOW] + ' ' +
                         LongToStrLZ (dt.Day) + ' ' +
                         MonthNames [dt.Month] + ' ' +
                         LongToStr   (dt.Year)
    else DateToString := '';
end;

{ Returns if two names are considered equal for the file system }
function  EqualNames(Name1, Name2: TFileName): Boolean;
{$IFDEF OS_DOS}
var
  I: Byte;
begin
  { case-insensitive comparision of strings }
  EqualNames := false;
  if (Length(Name1) <> Length(Name2)) or (Length(Name1) = 0)
    then exit;
  for I := 1 to Length(Name1) do
    if (Upcase(Name1[I]) <> Upcase(Name2[I]))
      then exit;
  EqualNames := true;
end;
{$ELSE}
begin
  { case-sensitive comparision of strings }
  EqualNames := (Name1 = Name2);
end;
{$ENDIF}

{ Returns if name "AName" is in use (as file or directory) }
function Exists(AName: TFileName): Boolean;
begin
  Exists := (CheckName (AName) <> cnUnknown);
end;

{ Splits AName into its path, raw name and extension; example:
  "c:\pp\fv\archive.zip" will be split into path "c:\pp\fv\",
  raw name "archive" and extension "zip" }
procedure SplitName(AName: TFileName; var Path, RawName, Extension: TFileName);
var
  HasDot, HasSeparator: Boolean;
  I, NameLength, DotOffset, SeparatorOffset: Longint;
begin
  NameLength := Length(AName);
  Path := '';
  RawName := '';
  Extension := '';
  { search for last separator in name }
  SeparatorOffset := -1;
  HasSeparator := false;
  I := NameLength;
  while (I > 0) and (not HasSeparator) do begin
    if (AName[i] = PathSeparator)
      then begin
             HasSeparator := true;
             SeparatorOffset := I;
           end;
    Dec(I);
  end;
  if HasSeparator
    then begin
           Path := System.Copy(AName, 1, SeparatorOffset);
           SeparatorOffset := SeparatorOffset + 1;
         end
    else SeparatorOffset := 1;
  I := SeparatorOffset;
  { search for last dot in name (not in path /
    think of 'dir/files.old/filename') }
  HasDot := false;
  while (I <= NameLength) do begin
    if (AName[I] = '.')
      then begin
             HasDot := true;
             DotOffset := I;
           end;
    Inc(I);
  end;
  if HasDot
    then begin
           RawName := System.Copy (AName,
                                   SeparatorOffset,
                                   DotOffset-SeparatorOffset);
           Extension := System.Copy (AName,
                                     DotOffset + 1,
                                     NameLength - DotOffset);
         end
    else begin
           { no extension }
           RawName := System.Copy (AName,
                                   SeparatorOffset,
                                   NameLength - SeparatorOffset);
         end;
end;

{ Returns time part of "DT" as "23:04:38" }
function TimeToString(DT: TDateTime): String;
begin
  if DT.Valid
    then TimeToString := LongToStrLZ(DT.Hour) + ':' +
                         LongToStrLZ(DT.Minute) + ':' +
                         LongToStrLZ(DT.Second)
    else TimeToString := '';
end;

{$IFDEF OS_DOS} { procedures & functions for the DOS platform }

{ Functions and procedures not declared in the interface section }

{ Returns date part of dt in DOS format, as unsigned 16 bit integer }
procedure GetDOSDate(DT: TDateTime; var W: Word);
begin
  W := (DT.Day and $1f) or
       ((DT.Month and $f) shl 5) or
       (((DT.Year - 1980) and $7f) shl 9);
end;

{ Returns time part of DT in DOS format, as unsigned 16 bit integer }
procedure GetDOSTime(DT: TDateTime; var W: Word);
begin
  W := ((DT.Second shr 1) and $1f) or
       ((DT.Minute and $3f) shl 5) or
       ((DT.Hour and $1f) shl 11);
end;

{ Returns date and time as 32 bit integer value (DOS time format) }
procedure GetDOSDateTime(DT : TDateTime; var L: Longint);
var
  W: Word;
begin
  GetDOSTime(DT, W);
  L := W;
  GetDOSDate(DT, W);
  L := L + (W * 65536); { shifting by 16 doesn't work everywhere ... }
end;

{ Sets date part of DT to W }
procedure SetDOSDate(W: Word; var DT: TDateTime);
begin
  DT.Day := W and $1f;
  DT.Month := (W shr 5) and $f;
  DT.Year := 1980 + (W shr 9) and $7f;
end;

{ Sets time part of DT to W }
procedure SetDOSTime(W: Word; var DT: TDateTime);
begin
  DT.Second := (W and $1f) shl 1;
  DT.Minute := (W shr 5) and $3f;
  DT.Hour := (W shr 11) and $1f;
end;

{ Sets DT to data from L }
procedure SetDOSDateTime(L: Longint; var DT: TDateTime);
begin
  SetDOSTime(L mod 65536, DT);
  SetDOSDate(L div 65536, DT);
end;

{ Converts DOS.SearchRec to TFileDesciptor }
procedure SearchRecToFileDescriptor (    SR: DOS.SearchRec;
                                     var FD: TFileDescriptor);
begin
  FD.Name := SR.Name;
  FD.Attr := SR.Attr;
  FD.Size := SR.Size;
  FD.IsDirectory := ((SR.Attr and faDirectory) <> 0);
  SetDOSDateTime(SR.Time, FD.LastModification);
  CheckDateTime(FD.LastModification);
end;

{$ENDIF} { OS_DOS }

{$IFDEF OS_UNIX}
{ Functions and procedures not decalred in interface section,
  Unix operating systems }

Procedure EpochToDateTime (Epoch : Longint; var DT : TDateTime);
{ Returns a Checked datetime, starting from a Unix epoch-style time }

var y,m,d,h,mi,s : Word; { needed because of call by var }

begin
  Linux.EpochToLocal(Epoch,Y,M,D,h,mi,s);
  DT.Year   :=y;
  DT.Month  :=m;
  DT.Day    :=d;
  DT.Hour   :=h;
  DT.Minute :=mi;
  DT.Second :=s;
  CheckDateTime (DT);
end;

Procedure StatToFileDescriptor (Info : Stat; Var Fd : TFileDescriptor);
{Starting from a stat record, returns a TFileDescriptor record.
 Name is not filled in !}
begin
  Fd.Attr:=Info.Mode;
  Fd.IsDirectory:=S_ISDIR(Info.mode);
  EpochToDateTime(Info.Mtime,Fd.LastModification);
  EpochToDateTime(Info.Atime,Fd.LastAccessed);
  EpochToDateTime(Info.Ctime,Fd.Created);
  Fd.Size:=Info.size;
end;
{$ENDIF} {OS_Unix}

{ Functions and procedures declared in the interface section }

{ Returns type of name as cnXXXX constant (unknown, file, directory) }
function CheckName(AName: TFileName): Byte;
var
  FS: TFileSearch;
begin
  FS.Specs := AName;
  {$IFDEF OS_DOS}
  FS.Attr := faAnyFile;
  {$ENDIF}
  StartSearch(fs);
  if FS.Success
    then begin
           if FS.FD.IsDirectory
             then CheckName := cnDirectory
             else CheckName := cnFile;
         end
    else CheckName := cnUnknown;
  TerminateSearch(FS);
end;

{ Continues a file search started by StartSearch }
procedure ContinueSearch(var FS: TFileSearch);
{$IFDEF OS_Unix}
Var g : PGLob;
    info : stat;

begin
  if Not FS.Success then exit;
  FS.Success:=False;
  if FS.GL=nil then exit; { Paranoia setting }
  g:=FS.GL;
  FS.GL:=FS.GL^.NEXT;
  strdispose(g^.name);
  dispose (g);
  If FS.GL=Nil then exit;
  linux.fstat(strpas(FS.GL^.Name),info);
  if linuxerror<>0 then
    begin
    StatToFileDescriptor (info,FS.FD);
    FS.FD.Name:=strpas(FS.GL^.Name);
    FS.Success:=True;
    end;
end;
{$ELSE OS_Unix}
begin
  if fs.Success
    then begin
           DOS.FindNext(FS.SR);
           FS.Success := (DOS.DOSError = 0);
           if FS.Success
             then SearchRecToFileDescriptor(fs.sr, fs.fd);
         end;
end;
{$ENDIF OS_Unix}

{ Create a new subdirectory AName }
procedure CreateDir(AName : TFileName);
begin
  {$I-}
  System.MkDir(AName);
  {$I+}
  ErrorCode := System.IOResult;
end;

{ Deletes the directory AName }
procedure DeleteDir(AName : TFileName);
begin
  {$I-}
  System.RmDir(AName);
  {$I+}
  ErrorCode := System.IOResult;
end;

{ Deletes the file AName }
procedure DeleteFile(AName: TFileName);
var
  F: file;
begin
  Assign(F, AName);
  {$I-}
  System.Erase(F);
  {$I+}
  ErrorCode := System.IOResult;
end;

{ Returns the full version of AName }
function ExpandName(AName : TFileName): TFileName;
begin
{$IFDEF OS_Unix}
  ExpandName := Linux.FExpand(AName);
{$ELSE}
  ExpandName := DOS.FExpand(AName);
{$ENDIF}
end;

{ Returns a string version of AFileAttr; OS-dependent }
function FileAttrToString(AFileAttr: TFileAttr): String;
{$IFDEF OS_DOS}
{ Volume Label and Directory are not regarded }
const
  NumChars = 4;
  AttrChars: String[NumChars] = 'RSHA';
  AttrMasks: Array[0..NumChars-1] of Word = (1, 2, 4, 32);
var
  I: Word;
  S: String[NumChars];
begin
  s[0] := Chr(NumChars);
  for I := 1 to NumChars do begin
    if ((AFileAttr and AttrMasks[i-1]) = 0)
      then S[I] := '.'
      else S[I] := AttrChars[i];
  end;
  FileAttrToString := S;
end;
{$ELSE OS_DOS}
{$IFDEF OS_Unix}
var temp : string[9];
    i : longint;

const
    full = 'rwxrwxrwx';

begin
  temp:='---------';
  for i:=0 to 8 do
    if (AFileAttr and (1 shl i))=(1 shl I) then temp[9-i]:=full[9-i];
  FileAttrToString := Temp;
end;
{$ELSE OS_Unix}
begin
  FileAttrToString:='';
end;
{$ENDIF OS_Unix}
{$ENDIF OS_DOS}

{ Returns a string version of the file integer value fi }
function FileIntToString(fi: TFileInt): String;
var
  S: String[14]; { maximum is "-2,147,483,648" }
  I: Integer;    { must be signed ! }
begin
  Str(fi, S);
  if SeparateThousands
    then begin
           I := System.Length(S) - 2;
           while (I > 1) and (not (I = 2) and (s[1] = '-')) do begin
             System.Insert (ThousandsSeparator, S, I);
             Dec(I, 3);
           end;
         end;
  FileIntToString := S;
end;

{ Returns the currently set directory }
function GetCurrentDir: TFileName;
{$IFDEF PPC_BP}
var
  I: Byte;
  R: DOS.Registers;
  S: TFileName;
begin
  { to get a full name, we have to get the drive letter ourselves }

  { get current drive letter first }
  R.AH := $19;
  DOS.MsDos(R);

  S[1] := Chr(Ord('A') + R.AL);
  S[2] := ':';
  S[3] := '\';

  { get current directory }
  R.AH := $47;
  R.DL := $00;
  R.DS := Seg(S[4]);
  R.SI := Ofs(S[4]);
  DOS.MsDos (r);
  if ((R.Flags and FCarry) <> 0)
    then begin
           { error }
         end;

  { determine length of current directory }
  I := 4;
  while (S[I] <> #0) and (I < MaxNameLength) do
    Inc(I);
  S[0] := Chr(I - 1);

  GetCurrentDir := S;
end;
{$ELSE}
var
  S: TFileName;
begin
  System.GetDir(0, S);
  GetCurrentDir := S;
end;
{$ENDIF}

{ Gets attribute of AName }
procedure GetFAttr(AName: TFileName; var Attr: TFileAttr);
{$IFDEF OS_DOS}
var
  F: file;
  W: word;
begin
  Assign(F, AName);
  {$I-}
  DOS.GetFAttr(F, W);
  Attr:=W;
  {$I+}
  ErrorCode := DOS.DOSError;
end;
{$ELSE}
{$IFDEF OS_Unix}
var
  info : stat;
begin
  Linux.FStat (AName,Info);
  ErrorCode:=LinuxError;
  if ErrorCode<>0 then exit;
  Attr:=Info.Mode;
end;
{$ELSE}
begin
end;
{$ENDIF}
{$ENDIF}

{ Gets date and time of last modification of AName }
procedure GetFTime(AName: TFileName; var DT: TDateTime);
{$IFDEF OS_DOS}
var
  F: file;
  L: Longint;
begin
  DT.Valid := false;
  { open file }
  Assign(F, AName);
  {$I-}
  Reset(F);
  {$I+}
  ErrorCode := System.IOResult;
  if (ErrorCode <> errOK)
    then exit;
  { get date/time of last modification in DOS format }
  {$I-}
  DOS.GetFTime(F, L);
  {$I+}
  ErrorCode := DOS.DOSError;
  if (ErrorCode <> errOK)
    then exit;
  { close file }
  {$I-}
  Close(F);
  {$I+}
  ErrorCode := System.IOResult;
  { convert date/time L to TDateTime format }
  GetDOSDateTime(DT, L);
  CheckDateTime(DT);
end;
{$ELSE}
{$IFDEF OS_Unix}
var info : Stat;

begin
  Linux.FStat (AName,Info);
  ErrorCode:=LinuxError;
  if ErrorCode<>0 then exit;
  EpochToDateTime (info.mtime,DT);
end;
{$ELSE}
begin
end;
{$ENDIF}
{$ENDIF}

{ Returns if AName is a valid file name (not if it actually exists) }
function IsValidName(AName: TFileName): Boolean;
{$IFDEF OS_DOS}
  { isn't ready yet }

  { Returns if a name (without a path) is valid }
  function ValidName(S: TFileName): Boolean;
  var
    I: Byte;
  begin
    ValidName := false;
    if (Length(S) > 12)
      then exit;
    I := Pos('.', S);

    ValidName := true;
  end;

const
  InvalidChars: String[2] = '*?';

var
  I, J: Longint;
  P, R, E: TFileName;
begin
  IsValidName := false;
  { check for invalid characters }
  for I := 1 to Length(AName) do
    for J := 1 to Length(InvalidChars) do
      if (AName[I] = InvalidChars[J])
        then exit;
  SplitName(AName, P, R, E);
  if (Length(R) > 0) or (Length(E) > 0)
    then begin
           if (not ValidName(R + E))
             then exit;
         end;

  IsValidName := true;
end;
{$ELSE}
{$IFDEF OS_Unix}
begin
  IsVAlidName:=((pos('?',AName)=0) and (pos('*',AName)=0))
end;
{$ELSE}
begin
  IsValidName:=True;
end;
{$ENDIF}
{$ENDIF}

{ Renames directory from OldName to NewName }
procedure RenameDir(OldName, NewName : TFileName);
begin
  { for DOS, renaming files and directories should be the same ... }
  RenameFile(OldName, NewName);
end;

{ Renames file from OldName to NewName }
procedure RenameFile(OldName, NewName : TFileName);
var
  F: file;
begin
  Assign(F, OldName);
  {$I-}
  System.Rename(F, NewName);
  {$I+}
  ErrorCode := IOResult;
end;

{ Sets current directory to AName }
procedure SetCurrentDir(AName : TFileName);
begin
  {$I-}
  System.ChDir(AName);
  {$I+}
  ErrorCode := IOResult;
end;

{ Sets attribute of file AName to AFileAttr }
procedure SetFAttr(AName: TFileName; AFileAttr: TFileAttr);
{$IFDEF OS_DOS}
var
  F: file;
begin
  Assign(F, AName);
  {$I-}
  DOS.SetFAttr(F, AFileAttr);
  {$I+}
  ErrorCode := DOS.DOSError;
end;
{$ELSE}
{$IFDEF OS_Unix}
begin
  Linux.Chmod (Aname,AFileAttr);
  ErrorCode:=LinuxError;
end;
{$ELSE}
begin
end;
{$ENDIF}
{$ENDIF}

{ Sets date and time of last modification of file AName to dt }
procedure SetFTime(AName: TFileName; DT: TDateTime);
{$IFDEF OS_DOS}
var
  F: file;
  L: Longint;
begin
  GetDOSDateTime(DT, L);
  Assign(f, AName);
  {$I-}
  DOS.SetFTime(F, L);
  {$I+}
  ErrorCode := DOS.DOSError;
end;
{$ELSE}
{$IFDEF OS_Unix}
var
  utim : utimebuf;
begin
  utim.actime:=LocalToEpoch(DT.Year,DT.Month,DT.Day,DT.Hour,DT.Minute,DT.second);
  utim.modtime:=utim.actime;
  utime (AName,utim);
  ErrorCode:=linuxerror
end;
{$ELSE}
begin
end;
{$ENDIF}
{$ENDIF}

{ Starts a file search, using input data from fs }
procedure StartSearch(var FS: TFileSearch);
{$IFDEF OS_Unix}
var
  info : stat;
begin
  FS.Success:=False;
  FS.GL:=Linux.Glob(FS.Specs);
  if FS.GL=nil then exit;
  linux.fstat(strpas(FS.GL^.Name),info);
  if linuxerror=0 then
    begin
    StatToFileDescriptor (info,FS.FD);
    FS.FD.Name:=strpas(FS.GL^.Name);
    FS.Success:=True;
    end;
end;
{$ELSE OS_Unix}
{ this version works for every platform/os/bits combination that has a
  working DOS unit : BP/FPC/Virtual Pascal }
begin
  DOS.FindFirst(fs.Specs, fs.Attr, fs.sr);
  fs.Success := (DOS.DOSError = 0);
  if fs.Success
    then SearchRecToFileDescriptor(FS.SR, FS.FD);
end;
{$ENDIF OS_Unix}

{ Terminates a file search }
procedure TerminateSearch (var FS: TFileSearch);
begin
{$IFDEF OS_Unix}
GlobFree (FS.GL);
{$ELSE}
  {$IFNDEF PPC_BP}
  DOS.FindClose(fs.sr);
  {$ENDIF}
{$ENDIF}
end;

{ Unit initialization }
begin
  { Empty, though we could retrieve the thousands separator and
    date/time formats here (in case the OS supports that) }
end.
{
  $Log$
  Revision 1.3  2000-11-23 10:17:48  sg
  * Linux.EpochToLocal has var arguments of type Word, not Integer - so
    some local variables had to be changed

  Revision 1.2  2000/11/13 14:35:57  marco
   * Unix Renamefest for defines.

  Revision 1.1  2000/07/13 06:29:38  michael
  + Initial import

  Revision 1.2  2000/02/29 11:43:16  pierre
    Common renamed APIComm to avoid problems with free vision

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/12/23 19:36:47  peter
    * place unitfiles in target dirs

  Revision 1.1  1999/11/24 23:36:37  peter
    * moved to packages dir

  Revision 1.4  1999/05/17 13:55:18  pierre
   * FPC win32 also need dos unit

  Revision 1.3  1999/04/13 09:25:47  daniel
  * Reverted a terrible mistake

  Revision 1.1  1998/12/04 12:48:24  peter
    * moved some dirs

  Revision 1.5  1998/10/26 11:22:50  peter
    * updates


  ?            0.1      marco   Initial implementation
  ?                             Several fixes ...
  08/29/1997   0.4      marco   Some platform adjustments
  09/16/1997   0.4.1    marco   Added "EqualNames"
  09/17/1997   0.5      michael Implemented linux part.
  09/20/1997   0.5.1    marco   Added LastAccessed/Created to Linux part of
                                file descriptor
  04/15/1998   0.5.2    michael Updated linux part.
}