{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Dos unit for BP7 compatible RTL (novell netware)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ 2000/09/03 armin: first version
  2001/04/08 armin: implemented more functions
                      OK: Implemented and tested
                      NI: not implemented
  2001/04/15 armin: FindFirst bug corrected, FExpand and FSearch tested, GetCBreak, SetCBreak
                    implemented
}

unit dos;
interface

Const
  {Bitmasks for CPU Flags}
  fcarry     = $0001;
  fparity    = $0004;
  fauxiliary = $0010;
  fzero      = $0040;
  fsign      = $0080;
  foverflow  = $0800;

  {Bitmasks for file attribute}
  readonly  = $01;
  hidden    = $02;
  sysfile   = $04;
  volumeid  = $08;
  nwexeconly= $08;
  directory = $10;
  archive   = $20;
  sharable  = $80;
  anyfile   = $3F;

  {File Status}
  fmclosed = $D7B0;
  fminput  = $D7B1;
  fmoutput = $D7B2;
  fminout  = $D7B3;


Type
{ Needed for LFN Support }
  ComStr  = String[255];
  PathStr = String[255];
  DirStr  = String[255];
  NameStr = String[255];
  ExtStr  = String[255];

{
  filerec.inc contains the definition of the filerec.
  textrec.inc contains the definition of the textrec.
  It is in a separate file to make it available in other units without
  having to use the DOS unit for it.
}
{$i filerec.inc}
{$i textrec.inc}

  DateTime = packed record
    Year,
    Month,
    Day,
    Hour,
    Min,
    Sec   : word;
  End;

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

  registers = packed record
    case i : integer of
     0 : (ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,f8,flags,fs,gs : word);
     1 : (al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh : byte);
     2 : (eax,  ebx,  ecx,  edx,  ebp,  esi,  edi : longint);
    end;


Var
  DosError : integer;



{Info/Date/Time}
Function  DosVersion: Word;                                  {ok}
Procedure GetDate(var year, month, mday, wday: word);        {ok}
Procedure GetTime(var hour, minute, second, sec100: word);   {ok}
procedure SetDate(year,month,day: word);                     {ok}
Procedure SetTime(hour,minute,second,sec100: word);          {ok}
Procedure UnpackTime(p: longint; var t: datetime);           {ok}
Procedure PackTime(var t: datetime; var p: longint);         {ok}

{Exec}
Procedure Exec(const path: pathstr; const comline: comstr);  {ni}
Function  DosExitCode: word;                                 {ni}

{Disk}
{$ifdef Int64}
 Function  DiskFree(drive: byte) : int64;                    {ok}
 Function  DiskSize(drive: byte) : int64;                    {ok}
{$else}
 Function  DiskFree(drive: byte) : longint;                  {ok}
 Function  DiskSize(drive: byte) : longint;                  {ok}
{$endif}

{FincClose has to be called to avoid memory leaks}
Procedure FindFirst(const path: pathstr; attr: word;         {ok}
                    var f: searchRec);
Procedure FindNext(var f: searchRec);                        {ok}
Procedure FindClose(Var f: SearchRec);                       {ok}

{File}
Procedure GetFAttr(var f; var attr: word);                   {ok}
Procedure GetFTime(var f; var time: longint);                {ok}
Function  FSearch(path: pathstr; dirlist: string): pathstr;  {ok}
Function  FExpand(const path: pathstr): pathstr;             {ok}
Procedure FSplit(path: pathstr; var dir: dirstr; var name:   {untested}
                 namestr; var ext: extstr);

{Environment}
Function  EnvCount: longint;                                 {ni}
Function  EnvStr(index: integer): string;                    {ni}
Function  GetEnv(envvar: string): string;                    {ok}

{Misc}
Procedure SetFAttr(var f; attr: word);                       {ni}
Procedure SetFTime(var f; time: longint);                    {ni}
Procedure GetCBreak(var breakvalue: boolean);                {ni}
Procedure SetCBreak(breakvalue: boolean);                    {ni}
Procedure GetVerify(var verify: boolean);                    {ni}
Procedure SetVerify(verify: boolean);                        {ni}

{Do Nothing Functions}
Procedure SwapVectors;                                       {ni}
Procedure GetIntVec(intno: byte; var vector: pointer);       {ni}
Procedure SetIntVec(intno: byte; vector: pointer);           {ni}
Procedure Keep(exitcode: word);                              {ni}

Procedure Intr(intno: byte; var regs: registers);            {ni}
Procedure MSDos(var regs: registers);                        {ni}


implementation

uses
  strings;

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


Procedure packtime(var t : datetime;var p : longint);
Begin
  p:=(t.sec shr 1)+(t.min shl 5)+(t.hour shl 11)+(t.day shl 16)+(t.month shl 21)+((t.year-1980) shl 25);
End;


Procedure unpacktime(p : longint;var t : datetime);
Begin
  with t do
   begin
     sec:=(p and 31) shl 1;
     min:=(p shr 5) and 63;
     hour:=(p shr 11) and 31;
     day:=(p shr 16) and 31;
     month:=(p shr 21) and 15;
     year:=(p shr 25)+1980;
   end;
End;


{******************************************************************************
                               --- Exec ---
******************************************************************************}

var
  lastdosexitcode : word;

procedure exec(const path : pathstr;const comline : comstr);
begin
  ConsolePrintf ('warning: fpc dos.exec not implemented'#13#10,0);
end;


function dosexitcode : word;
begin
  dosexitcode:=lastdosexitcode;
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


procedure getverify(var verify : boolean);
begin
  verify := true;
end;


procedure setverify(verify : boolean);
begin
end;


{******************************************************************************
                               --- Disk ---
******************************************************************************}

function getvolnum (drive : byte) : longint;
var dir : STRING[255];
    P,PS: BYTE;
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

{$ifdef Int64}

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
    if _GetVolumeInfoWithNumber (volumeNumber,@Buf,
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
    if _GetVolumeInfoWithNumber (volumeNumber,@Buf,
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
{$else}

function diskfree(drive : byte) : longint;
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
    if _GetVolumeInfoWithNumber (volumeNumber,@Buf,
                                 TotalBlocks,
                                 SectorsPerBlock,
                                 availableBlocks,
                                 totalDirectorySlots,
                                 availableDirSlots,
                                 volumeisRemovable) = 0 THEN
    begin
      diskfree := availableBlocks * SectorsPerBlock * 512;
    end else
      diskfree := 0;
  end else
    diskfree := 0;
end;


function disksize(drive : byte) : longint;
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
    if _GetVolumeInfoWithNumber (volumeNumber,@Buf,
                                 TotalBlocks,
                                 SectorsPerBlock,
                                 availableBlocks,
                                 totalDirectorySlots,
                                 availableDirSlots,
                                 volumeisRemovable) = 0 THEN
    begin
      disksize := TotalBlocks * SectorsPerBlock * 512;
    end else
      disksize := 0;
  end else
    disksize := 0;
end;

{$endif}


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


procedure swapvectors;
begin
end;


{******************************************************************************
                               --- File ---
******************************************************************************}

procedure fsplit(path : pathstr;var dir : dirstr;var name : namestr;var ext : extstr);
var
   dotpos,p1,i : longint;
begin
  { allow slash as backslash }
  for i:=1 to length(path) do
   if path[i]='/' then path[i]:='\';
  { get drive name }
  p1:=pos(':',path);
  if p1>0 then
    begin
       dir:=path[1]+':';
       delete(path,1,p1);
    end
  else
    dir:='';
  { split the path and the name, there are no more path informtions }
  { if path contains no backslashes                                 }
  while true do
    begin
       p1:=pos('\',path);
       if p1=0 then
         break;
       dir:=dir+copy(path,1,p1);
       delete(path,1,p1);
    end;
  { try to find out a extension }
  if LFNSupport then
    begin
       Ext:='';
       i:=Length(Path);
       DotPos:=256;
       While (i>0) Do
         Begin
            If (Path[i]='.') Then
              begin
                 DotPos:=i;
                 break;
              end;
            Dec(i);
         end;
       Ext:=Copy(Path,DotPos,255);
       Name:=Copy(Path,1,DotPos - 1);
    end
  else
    begin
       p1:=pos('.',path);
       if p1>0 then
         begin
            ext:=copy(path,p1,4);
            delete(path,p1,length(path)-p1+1);
         end
       else
         ext:='';
       name:=path;
    end;
end;


function fexpand(const path : pathstr) : pathstr;
var
  s,pa : pathstr;
  i,j  : longint;
begin
  getdir(0,s);
  i:=ioresult;
  if LFNSupport then
   begin
     pa:=path;
   end
  else
   if FileNameCaseSensitive then
    pa:=path
   else
    pa:=upcase(path);

  { allow slash as backslash }
  for i:=1 to length(pa) do
   if pa[i]='/' then
    pa[i]:='\';

  if (length(pa)>1) and (pa[2]=':') and (pa[1] in ['A'..'Z','a'..'z']) then
    begin
       { Always uppercase driveletter }
       if (pa[1] in ['a'..'z']) then
        pa[1]:=Chr(Ord(Pa[1])-32);
       { we must get the right directory }
       getdir(ord(pa[1])-ord('A')+1,s);
       i:=ioresult;
       if (ord(pa[0])>2) and (pa[3]<>'\') then
         if pa[1]=s[1] then
           begin
             { remove ending slash if it already exists }
             if s[length(s)]='\' then
              dec(s[0]);
             pa:=s+'\'+copy (pa,3,length(pa));
           end
         else
           pa:=pa[1]+':\'+copy (pa,3,length(pa))
    end
  else
    if pa[1]='\' then
      begin
        { Do not touch Network drive names if LFNSupport is true }
        if not ((Length(pa)>1) and (pa[2]='\') and LFNSupport) then
          pa:=s[1]+':'+pa;
      end
    else if s[0]=#3 then
      pa:=s+pa
    else
      pa:=s+'\'+pa;

{ Turbo Pascal gives current dir on drive if only drive given as parameter! }
if length(pa) = 2 then
 begin
   getdir(byte(pa[1])-64,s);
   pa := s;
 end;

{First remove all references to '\.\'}
  while pos ('\.\',pa)<>0 do
   delete (pa,pos('\.\',pa),2);
{Now remove also all references to '\..\' + of course previous dirs..}
  repeat
    i:=pos('\..\',pa);
    if i<>0 then
     begin
       j:=i-1;
       while (j>1) and (pa[j]<>'\') do
        dec (j);
       if pa[j+1] = ':' then j := 3;
       delete (pa,j,i-j+3);
     end;
  until i=0;

  { Turbo Pascal gets rid of a \.. at the end of the path }
  { Now remove also any reference to '\..'  at end of line
    + of course previous dir.. }
  i:=pos('\..',pa);
  if i<>0 then
   begin
     if i = length(pa) - 2 then
      begin
        j:=i-1;
        while (j>1) and (pa[j]<>'\') do
         dec (j);
        delete (pa,j,i-j+3);
      end;
      pa := pa + '\';
    end;
  { Remove End . and \}
  if (length(pa)>0) and (pa[length(pa)]='.') then
   dec(byte(pa[0]));
  { if only the drive + a '\' is left then the '\' should be left to prevtn the program
    accessing the current directory on the drive rather than the root!}
  { if the last char of path = '\' then leave it in as this is what TP does! }
  if ((length(pa)>3) and (pa[length(pa)]='\')) and (path[length(path)] <> '\') then
   dec(byte(pa[0]));
  { if only a drive is given in path then there should be a '\' at the
    end of the string given back }
  if length(pa) = 2 then pa := pa + '\';
  fexpand:=pa;
end;


Function FSearch(path: pathstr; dirlist: string): pathstr;
var
  i,p1   : longint;
  s      : searchrec;
  newdir : pathstr;
begin
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
       { allow slash as backslash }
       for i:=1 to length(dirlist) do
         if dirlist[i]='/' then dirlist[i]:='\';
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
         if (newdir<>'') and (not (newdir[length(newdir)] in ['\',':'])) then
          newdir:=newdir+'\';
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
  ConsolePrintf ('warning: fpc dos.setftime not implemented'#13#10,0);
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
  ConsolePrintf ('warning: fpc dos.setfattr not implemented'#13#10,0);
end;


{******************************************************************************
                             --- Environment ---
******************************************************************************}

function envcount : longint;
begin
  envcount := 0;  {is there a netware function to do that ?????}
  ConsolePrintf ('warning: fpc dos.envcount not implemented'#13#10,0);
end;


function envstr(index : integer) : string;
begin
  envstr := '';   {is there a netware function to do that ?????}
  ConsolePrintf ('warning: fpc dos.envstr not implemented'#13#10,0);
end;

{ the function exists in clib but i dont know how to set environment vars.
  may be it's only a dummy in clib }
Function  GetEnv(envvar: string): string;
var
  envvar0 : array[0..256] of char;
  p       : pchar;
begin
  strpcopy(envvar0,envvar);
  p := _getenv (envvar0);
  if p = NIL then
    GetEnv := ''
  else
    GetEnv := strpas (p);
end;


{******************************************************************************
                             --- Not Supported ---
******************************************************************************}

Procedure keep(exitcode : word);
Begin
 { no netware equivalent }
End;

Procedure getintvec(intno : byte;var vector : pointer);
Begin
 { no netware equivalent }
End;

Procedure setintvec(intno : byte;vector : pointer);
Begin
 { no netware equivalent }
End;

procedure intr(intno : byte;var regs : registers);
begin
 { no netware equivalent }
end;

procedure msdos(var regs : registers);
begin
 { no netware equivalent }
end;


end.
{
  $Log$
  Revision 1.5  2002-09-07 16:01:20  peter
    * old logs removed and tabs fixed

}
