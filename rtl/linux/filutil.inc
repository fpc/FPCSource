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


Function FileOpen (Const FileName : string; Mode : Integer) : Longint;

Var LinuxFlags : longint;

BEGIN
  LinuxFlags:=0;
  Case (Mode and 3) of
    0 : LinuxFlags:=LinuxFlags or Open_RdOnly;
    1 : LinuxFlags:=LinuxFlags or Open_WrOnly;
    2 : LinuxFlags:=LinuxFlags or Open_RdWr;
  end;
  FileOpen:=fdOpen (FileName,LinuxFlags);
  //!! We need to set locking based on Mode !!
end;


Function FileCreate (Const FileName : String) : Longint;

begin
  FileCreate:=fdOpen(FileName,Open_RdWr or Open_Creat or Open_Trunc);
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;

begin
  FileRead:=fdRead (Handle,Buffer,Count);
end;


Function FileWrite (Handle : Longint; Var Buffer; Count : Longint) : Longint;

begin
  FileWrite:=fdWrite (Handle,Buffer,Count);
end;


Function FileSeek (Handle,Offset,Origin : Longint) : Longint;

begin
  FileSeek:=fdSeek (Handle,Offset,Origin);
end;


Procedure FileClose (Handle : Longint);

begin
  fdclose(Handle);
end;

Function FileTruncate (Handle,Size: Longint) : boolean;

begin
  FileTruncate:=fdtruncate(Handle,Size);
end;

Function FileAge (Const FileName : String): Longint;

Var Info : Stat;
    Y,M,D,hh,mm,ss : word;

begin
  If not fstat (FileName,Info) then
    exit(-1)
  else
    begin
    EpochToLocal(info.mtime,y,m,d,hh,mm,ss);
    Result:=DateTimeToFileDate(EncodeDate(y,m,d)+EncodeTime(hh,mm,ss,0));
    end;
end;


Function FileExists (Const FileName : String) : Boolean;

Var Info : Stat;

begin
  FileExists:=fstat(filename,Info);
end;

Function LinuxToWinAttr (FN : Char; Const Info : Stat) : Longint;



begin
  Result:=faArchive;
  If FN='.' then
    Result:=Result or faHidden;
  If (Info.Mode and STAT_IFDIR)=STAT_IFDIR then
    Result:=Result or faDirectory;
  If (Info.Mode and STAT_IWUSR)=0 Then
     Result:=Result or faReadOnly;
  If (Info.Mode and 
      (STAT_IFSOCK or STAT_IFBLK or STAT_IFCHR or STAT_IFIFO))<>0 then
     Result:=Result or faSysFile;  
end;

{
 GlobToSearch takes a glob entry, stats the file.
 The glob entry is removed.
 If FileAttributes match, the entry is reused
}

Function GlobToTSearchRec (Var Info : TSearchRec) : Boolean;

Var SInfo : Stat;
    p     : Pglob;
    TAttr : Longint;

begin
  TAttr:=$ffffffff;
  P:=pglob(Info.FindHandle);
  Result:=P<>Nil;
  If Result then
    begin
    Info.FindHandle:=Longint(P^.Next);
    Result:=Fstat(p^.name,SInfo);
    If Result then
      begin
      Info.Attr:=LinuxToWinAttr(p^.name[0],SInfo);
      Result:=(Info.ExcludeAttr and TAttr)<>0;
      If Result Then
         With Info do
           begin
           Attr:=Info.Attr;
           If P^.Name<>Nil then
           Name:=strpas(p^.name);
           Time:=Sinfo.mtime;
           Size:=Sinfo.Size;
           end;
      end;
    P^.Next:=Nil;
    GlobFree(P);
    end;
end;

Function DoFind(Var Rslt : TSearchRec) : Longint;

begin
  Result:=-1;
  If Rslt.FindHandle<>0 then
    While (Rslt.FindHandle<>0) and not (Result=0) do
      If GlobToTSearchRec(Rslt) Then Result:=0;
end;



Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;

begin
  Rslt.ExcludeAttr:=Attr; //!! Not correct !!
  Rslt.FindHandle:=Longint(Glob(Path));
  Result:=DoFind (Rslt);
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;

begin
  Result:=DoFind (Rslt);
end;


Procedure FindClose (Var F : TSearchrec);

begin
  GlobFree (PGlob(F.FindHandle));
end;


Function FileGetDate (Handle : Longint) : Longint;

Var Info : Stat;

begin
  If Not(FStat(Handle,Info)) then
    Result:=-1
  else
    Result:=Info.Mtime;
end;


Function FileSetDate (Handle,Age : Longint) : Longint;

begin
  // Impossible under Linux from FileHandle !!
  FileSetDate:=-1;
end;


Function FileGetAttr (Const FileName : String) : Longint;

Var Info : Stat;

begin
  If Not FStat (FileName,Info) then
    Result:=-1
  Else
    Result:=LinuxToWinAttr(FileName[1],Info);
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;

begin
  Result:=-1;
end;


Function DeleteFile (Const FileName : String) : Boolean;

begin
  Result:=UnLink (FileName);
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;

begin
  RenameFile:=Linux.FRename(OldNAme,NewName);
end;


Function FileSearch (Const Name, DirList : String) : String;

begin
  FileSearch:=Linux.FSearch(Name,Dirlist);
end;

Procedure GetLocalTime(var SystemTime: TSystemTime);

begin
linux.GetTime(SystemTime.Hour, SystemTime.Minute, SystemTime.Second);
linux.GetDate(SystemTime.Year, SystemTime.Month, SystemTime.Day);
SystemTime.MilliSecond := 0;
end ;

Procedure InitAnsi;

Var i : longint;

begin
{  Fill table entries 0 to 127  }
for i := 0 to 96 do
  UpperCaseTable[i] := chr(i);
for i := 97 to 122 do
  UpperCaseTable[i] := chr(i - 32);
for i := 123 to 191 do
  UpperCaseTable[i] := chr(i);
Move (CPISO88591UCT,UpperCaseTable[192],SizeOf(CPISO88591UCT));

for i := 0 to 64 do
  LowerCaseTable[i] := chr(i);
for i := 65 to 90 do
  LowerCaseTable[i] := chr(i + 32);
for i := 91 to 191 do
  LowerCaseTable[i] := chr(i);
Move (CPISO88591LCT,UpperCaseTable[192],SizeOf(CPISO88591UCT));
end;

Procedure InitInternational;

begin
 InitAnsi;
end;

{
  $Log$
  Revision 1.12  2000-01-07 16:41:40  daniel
    * copyright 2000

  Revision 1.11  1999/05/14 22:19:34  michael
  * Some more fixes.

  Revision 1.10  1999/05/13 21:54:09  michael
  * Fixed FileAge and corrected FileGetAttr

  Revision 1.9  1999/04/08 11:31:01  peter
    * removed warnings

  Revision 1.8  1999/02/28 13:18:10  michael
  + Added internationalization support

  Revision 1.7  1999/02/24 15:57:29  michael
  + Moved getlocaltime to system-dependent files

  Revision 1.6  1999/02/04 21:43:08  michael
  FileCreate must truncate the file

  Revision 1.5  1999/02/02 21:20:34  michael
  + Added filetruncate, corrected FileCreate

  Revision 1.4  1998/12/15 22:43:07  peter
    * removed temp symbols

  Revision 1.3  1998/11/10 14:57:55  peter
    * renamed rename -> FRename

  Revision 1.2  1998/10/13 10:20:07  peter
    * fix for 0.99.8 which has no auto pchar-string ;)

  Revision 1.1  1998/10/11 12:21:01  michael
  Added file calls. Implemented for linux only

}
