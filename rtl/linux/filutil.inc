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
  FileCreate:=FileOpen (FileName,Open_RdWr or Open_Creat);
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


Function FileAge (Const FileName : String): Longint;

Var Info : Stat;

begin
  If not fstat (FileName,Info) then
    exit(-1)
  else
    Exit (Info.mtime);
end;


Function FileExists (Const FileName : String) : Boolean;

Var Info : Stat;

begin
  FileExists:=fstat(filename,Info);
end;

Function LinuxToWinAttr (FN : Char; Const Info : Stat) : Longint;

begin
  Result:=0;
  If FN='.' then
    Result:=Result or faHidden;
  If (Info.Mode and STAT_IFDIR)=STAT_IFDIR then
    Result:=Result or faDirectory;
  If (Info.Mode and STAT_IWUSR)=0 Then
     Result:=Result or faReadOnly;
end;

{
 GlobToSearch takes a glob entry, stats the file.
 The glob entry is removed.
 If FileAttributes match, the entry is reused
}

Function GlobToTSearchRec (Info : TSearchRec) : Boolean;

Var SInfo : Stat;
    p     : Pglob;
    TAttr : Longint;

begin
  P:=pglob(Info.FindHandle);
{$ifdef AUTOOBJPAS}
  Result:=Fstat(p^.name,SInfo);
{$else}
  Result:=Fstat(StrPas(p^.name),SInfo);
{$endif}
  Info.FindHandle:=Longint(P^.Next);
  P^.Next:=Nil;
  GlobFree(P);
  If Result then
    begin
    Info.Attr:=LinuxToWinAttr(p^.name[0],SInfo);
    Result:=(Info.ExcludeAttr and TAttr)<>0 ;
    If Result Then
       With Info do
         begin
         Attr:=Info.Attr;
         Name:=strpas(p^.name);
         Time:=Sinfo.mtime;
         Size:=Sinfo.Size;
         end;
    end;
end;

Function DoFind(Var Rslt : TSearchRec) : Longint;

begin
  If Rslt.FindHandle<>0 then
    While (Rslt.FindHandle<>0) and GlobToTSearchRec(Rslt) do;
  If Rslt.FindHandle=0 Then
    Result:=-1
  else
    Result:=0;
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
  //!! Still Needs doing
end;


Function DeleteFile (Const FileName : String) : Boolean;

begin
  Result:=UnLink (FileName);
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;

Var P1,P2 : String;

begin
  RenameFile:=Linux.FRename(OldNAme,NewName);
end;


Function FileSearch (Const Name, DirList : String) : String;

begin
  FileSearch:=Linux.FSearch(Name,Dirlist);
end;


{
  $Log$
  Revision 1.3  1998-11-10 14:57:55  peter
    * renamed rename -> FRename

  Revision 1.2  1998/10/13 10:20:07  peter
    * fix for 0.99.8 which has no auto pchar-string ;)

  Revision 1.1  1998/10/11 12:21:01  michael
  Added file calls. Implemented for linux only

}
