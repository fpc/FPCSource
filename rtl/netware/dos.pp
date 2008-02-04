{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Dos unit for BP7 compatible RTL (novell netware)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit dos;
interface

Type
  searchrec = packed record
     DirP  : POINTER;              { used for opendir }
     EntryP: POINTER;              { and readdir }
     Magic : WORD;
     fill  : array[1..11] of byte;
     attr  : byte;
     time  : longint;
     { reserved : word; not in DJGPP V2 }
     size  : longint;
     name  : string[255]; { NW uses only [12] but more can't hurt }
   end;

{$i dosh.inc}

implementation

uses
  strings, nwserv;

{$DEFINE HAS_GETMSCOUNT}
{$DEFINE HAS_GETCBREAK}
{$DEFINE HAS_SETCBREAK}
{$DEFINE HAS_KEEP}

{$define FPC_FEXPAND_DRIVES}
{$define FPC_FEXPAND_VOLUMES}
{$define FPC_FEXPAND_NO_DEFAULT_PATHS}

{$I dos.inc}


{$ASMMODE ATT}
{$I nwsys.inc }

{*****************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}
{$PACKRECORDS 4}


function dosversion : word;
VAR F : FILE_SERV_INFO;
begin
  IF GetServerInformation(SIZEOF(F),@F) = 0 THEN
    dosversion := WORD (F.netwareVersion) SHL 8 + F.netwareSubVersion;
end;


procedure getdate(var year,month,mday,wday : word);
VAR N : NWdateAndTime;
begin
  GetFileServerDateAndTime (N);
  wday:=N.DayOfWeek;
  year:=1900 + N.Year;
  month:=N.Month;
  mday:=N.Day;
end;


procedure setdate(year,month,day : word);
VAR N : NWdateAndTime;
begin
  GetFileServerDateAndTime (N);
  SetFileServerDateAndTime(year,month,day,N.Hour,N.Minute,N.Second);
end;


procedure gettime(var hour,minute,second,sec100 : word);
VAR N : NWdateAndTime;
begin
  GetFileServerDateAndTime (N);
  hour := N.Hour;
  Minute:= N.Minute;
  Second := N.Second;
  sec100 := 0;
end;


procedure settime(hour,minute,second,sec100 : word);
VAR N : NWdateAndTime;
begin
  GetFileServerDateAndTime (N);
  SetFileServerDateAndTime(N.year,N.month,N.day,hour,minute,second);
end;


function GetMsCount: int64;
begin
  GetMsCount := int64 (Nwserv.GetCurrentTicks) * 55;
end;


{******************************************************************************
                               --- Exec ---
******************************************************************************}

const maxargs=256;
procedure exec(const path : pathstr;const comline : comstr);
var c : comstr;
    i : integer;
    args : array[0..maxargs] of pchar;
    arg0 : pathstr;
    numargs : integer;
begin
  //writeln ('dos.exec (',path,',',comline,')');
  arg0 := fexpand (path)+#0;
  args[0] := @arg0[1];
  numargs := 0;
  c:=comline;
  i:=1;
  while i<=length(c) do
  begin
    if c[i]<>' ' then
    begin
      {Commandline argument found. append #0 and set pointer in args }
      inc(numargs);
      args[numargs]:=@c[i];
      while (i<=length(c)) and (c[i]<>' ') do
        inc(i);
      c[i] := #0;
    end;
    inc(i);
  end;
  args[numargs+1] := nil;
  i := spawnvp (P_WAIT,args[0],@args);
  if i >= 0 then
  begin
    doserror := 0;
    lastdosexitcode := i;
  end else
  begin
    doserror := 8;  // for now, what about errno ?
  end;
end;



procedure getcbreak(var breakvalue : boolean);
begin
  breakvalue := _SetCtrlCharCheckMode (false);  { get current setting }
  if breakvalue then
    _SetCtrlCharCheckMode (breakvalue);         { and restore old setting }
end;


procedure setcbreak(breakvalue : boolean);
begin
  _SetCtrlCharCheckMode (breakvalue);
end;


{******************************************************************************
                               --- Disk ---
******************************************************************************}

function getvolnum (drive : byte) : longint;
var dir : STRING[255];
    P,PS,
    V   : LONGINT;
begin
  if drive = 0 then
  begin  // get volume name from current directory (i.e. SERVER-NAME/VOL2:TEST)
    getdir (0,dir);
    p := pos (':', dir);
    if p = 0 then
    begin
      getvolnum := -1;
      exit;
    end;
    byte (dir[0]) := p-1;
    dir[p] := #0;
    PS := pos ('/', dir);
    INC (PS);
    if _GetVolumeNumber (@dir[PS], V) <> 0 then
      getvolnum := -1
    else
      getvolnum := V;
  end else
    getvolnum := drive-1;
end;


function diskfree(drive : byte) : int64;
VAR Buf                 : ARRAY [0..255] OF CHAR;
    TotalBlocks         : WORD;
    SectorsPerBlock     : WORD;
    availableBlocks     : WORD;
    totalDirectorySlots : WORD;
    availableDirSlots   : WORD;
    volumeisRemovable   : WORD;
    volumeNumber        : LONGINT;
begin
  volumeNumber := getvolnum (drive);
  if volumeNumber >= 0 then
  begin
    {i think thats not the right function but for others i need a connection handle}
    if _GetVolumeInfoWithNumber (byte(volumeNumber),@Buf,
                                 TotalBlocks,
                                 SectorsPerBlock,
                                 availableBlocks,
                                 totalDirectorySlots,
                                 availableDirSlots,
                                 volumeisRemovable) = 0 THEN
    begin
      diskfree := int64 (availableBlocks) * int64 (SectorsPerBlock) * 512;
    end else
      diskfree := 0;
  end else
    diskfree := 0;
end;


function disksize(drive : byte) : int64;
VAR Buf                 : ARRAY [0..255] OF CHAR;
    TotalBlocks         : WORD;
    SectorsPerBlock     : WORD;
    availableBlocks     : WORD;
    totalDirectorySlots : WORD;
    availableDirSlots   : WORD;
    volumeisRemovable   : WORD;
    volumeNumber        : LONGINT;
begin
  volumeNumber := getvolnum (drive);
  if volumeNumber >= 0 then
  begin
    {i think thats not the right function but for others i need a connection handle}
    if _GetVolumeInfoWithNumber (byte(volumeNumber),@Buf,
                                 TotalBlocks,
                                 SectorsPerBlock,
                                 availableBlocks,
                                 totalDirectorySlots,
                                 availableDirSlots,
                                 volumeisRemovable) = 0 THEN
    begin
      disksize := int64 (TotalBlocks) * int64 (SectorsPerBlock) * 512;
    end else
      disksize := 0;
  end else
    disksize := 0;
end;

{******************************************************************************
                     --- Findfirst FindNext ---
******************************************************************************}


PROCEDURE find_setfields (VAR f : searchRec);
BEGIN
  WITH F DO
  BEGIN
    IF Magic = $AD01 THEN
    BEGIN
      attr := WORD (PNWDirEnt(EntryP)^.d_attr);  // lowest 16 bit -> same as dos
      time := PNWDirEnt(EntryP)^.d_time + (LONGINT (PNWDirEnt(EntryP)^.d_date) SHL 16);
      size := PNWDirEnt(EntryP)^.d_size;
      name := strpas (PNWDirEnt(EntryP)^.d_name);
      if name = '' then
        name := strpas (PNWDirEnt(EntryP)^.d_nameDOS);
      doserror := 0;
    END ELSE
    BEGIN
      FillChar (f,SIZEOF(f),0);
      doserror := 18;
    END;
  END;
END;


procedure findfirst(const path : pathstr;attr : word;var f : searchRec);
var
  path0 : array[0..256] of char;
begin
  IF path = '' then
  begin
    doserror := 18;
    exit;
  end;
  strpcopy(path0,path);
  PNWDirEnt(f.DirP) := _opendir (path0);
  IF f.DirP = NIL THEN
    doserror := 18
  ELSE
  BEGIN
    IF attr <> anyfile THEN
      _SetReaddirAttribute (PNWDirEnt(f.DirP), attr);
    F.Magic := $AD01;
    PNWDirEnt(f.EntryP) := _readdir (PNWDirEnt(f.DirP));
    IF F.EntryP = NIL THEN
    BEGIN
      _closedir (PNWDirEnt(f.DirP));
      f.Magic := 0;
      doserror := 18;
    END ELSE
      find_setfields (f);
  END;
end;


procedure findnext(var f : searchRec);
begin
  IF F.Magic <> $AD01 THEN
  BEGIN
    doserror := 18;
    EXIT;
  END;
  doserror:=0;
  PNWDirEnt(f.EntryP) := _readdir (PNWDirEnt(f.DirP));
  IF F.EntryP = NIL THEN
    doserror := 18
  ELSE
    find_setfields (f);
end;


Procedure FindClose(Var f: SearchRec);
begin
  IF F.Magic <> $AD01 THEN
  BEGIN
    doserror := 18;
    EXIT;
  END;
  doserror:=0;
  _closedir (PNWDirEnt(f.DirP));
  f.Magic := 0;
  f.DirP := NIL;
  f.EntryP := NIL;
end;


{******************************************************************************
                               --- File ---
******************************************************************************}

Function FSearch(path: pathstr; dirlist: string): pathstr;
var
  i,p1   : longint;
  s      : searchrec;
  newdir : pathstr;
begin
  write ('FSearch ("',path,'","',dirlist,'"');
{ check if the file specified exists }
  findfirst(path,anyfile,s);
  if doserror=0 then
   begin
     findclose(s);
     fsearch:=path;
     exit;
   end;
{ No wildcards allowed in these things }
  if (pos('?',path)<>0) or (pos('*',path)<>0) then
    fsearch:=''
  else
    begin
       { allow backslash as slash }
       DoDirSeparators(dirlist);
       repeat
         p1:=pos(';',dirlist);
         if p1<>0 then
          begin
            newdir:=copy(dirlist,1,p1-1);
            delete(dirlist,1,p1);
          end
         else
          begin
            newdir:=dirlist;
            dirlist:='';
          end;
         if (newdir<>'') and (not (newdir[length(newdir)] in ['/',':'])) then
          newdir:=newdir+'/';
         findfirst(newdir+path,anyfile,s);
         if doserror=0 then
          newdir:=newdir+path
         else
          newdir:='';
       until (dirlist='') or (newdir<>'');
       fsearch:=newdir;
    end;
  findclose(s);
end;


{******************************************************************************
                       --- Get/Set File Time,Attr ---
******************************************************************************}

procedure getftime(var f;var time : longint);
VAR StatBuf : NWStatBufT;
    T       : DateTime;
    DosDate,
    DosTime : WORD;
begin
  IF _fstat (FileRec (f).Handle, StatBuf) = 0 THEN
  BEGIN
    _ConvertTimeToDos (StatBuf.st_mtime, DosDate, DosTime);
    time := DosTime + (LONGINT (DosDate) SHL 16);
  END ELSE
    time := 0;
end;


procedure setftime(var f;time : longint);
begin
  {is there a netware function to do that ?????}
  ConsolePrintf ('warning: fpc dos.setftime not implemented'#13#10);
end;


procedure getfattr(var f;var attr : word);
VAR StatBuf : NWStatBufT;
begin
  IF _fstat (FileRec (f).Handle, StatBuf) = 0 THEN
  BEGIN
    attr := word (StatBuf.st_attr);
  END ELSE
    attr := 0;
end;


procedure setfattr(var f;attr : word);
begin
  {is there a netware function to do that ?????}
  ConsolePrintf ('warning: fpc dos.setfattr not implemented'#13#10);
end;


{******************************************************************************
                             --- Environment ---
******************************************************************************}

function envcount : longint;
begin
  envcount := 0;  {is there a netware function to do that ?????}
  ConsolePrintf ('warning: fpc dos.envcount not implemented'#13#10);
end;


function envstr (index: longint) : string;
begin
  envstr := '';   {is there a netware function to do that ?????}
  ConsolePrintf ('warning: fpc dos.envstr not implemented'#13#10);
end;

{ works fine (at least with netware 6.5) }
Function  GetEnv(envvar: string): string;
var envvar0 : array[0..512] of char;
    p       : pchar;
    i,isDosPath,res : longint;
begin
  if upcase(envvar) = 'PATH' then
  begin  // netware does not have search paths in the environment var PATH
         // return it here (needed for the compiler)
    GetEnv := '';
    i := 1;
    res := _NWGetSearchPathElement (i, isdosPath, @envvar0[0]);
    while res = 0 do

    begin

      if GetEnv <> '' then GetEnv := GetEnv + ';';

      GetEnv := GetEnv + strpas(envvar0);

      inc (i);

      res := _NWGetSearchPathElement (i, isdosPath, @envvar0[0]);

    end;

    DoDirSeparators(getenv);

  end else
  begin
    strpcopy(envvar0,envvar);
    p := _getenv (envvar0);
    if p = NIL then
      GetEnv := ''
    else
      GetEnv := strpas (p);
  end;
end;


{******************************************************************************
                             --- Not Supported ---
******************************************************************************}

Procedure keep(exitcode : word);
Begin
 { simply wait until nlm will be unloaded }
 while true do _delay (60000);
End;


end.
