{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by the Free Pascal development team.

    Dos unit for BP7 compatible RTL (novell netware libc)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit dos;
interface

uses libc;

Type
  searchrec = packed record
     DirP  : POINTER;              { used for opendir }
     EntryP: POINTER;              { and readdir }
     Magic : WORD;
     fill  : array[1..11] of byte;
     attr  : byte;
     time  : longint;
     size  : longint;
     name  : string[255];
     { Internals used by netware port only: }
     _mask : string[255];
     _dir  : string[255];
     _attr : word;
   end;

{$i dosh.inc}
{Extra Utils}
function weekday(y,m,d : longint) : longint;


implementation

uses
  strings;

{$DEFINE HAS_GETMSCOUNT}
{$DEFINE HAS_KEEP}

{$DEFINE FPC_FEXPAND_DRIVES}
{$DEFINE FPC_FEXPAND_VOLUMES}
{$DEFINE FPC_FEXPAND_NO_DEFAULT_PATHS}

{$i dos.inc}


{$ASMMODE ATT}

{*****************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}
{$PACKRECORDS 4}


function dosversion : word;
var i : Tutsname;
begin
  if Fpuname (i) >= 0 then
    dosversion := WORD (i.netware_minor) SHL 8 + i.netware_major
  else dosversion := $0005;
end;

function WeekDay (y,m,d:longint):longint;
{
  Calculates th day of the week. returns -1 on error
}
var
  u,v : longint;
begin
  if (m<1) or (m>12) or (y<1600) or (y>4000) or
     (d<1) or (d>30+((m+ord(m>7)) and 1)-ord(m=2)) or
     ((m*d=58) and (((y mod 4>0) or (y mod 100=0)) and (y mod 400>0))) then
    WeekDay:=-1
  else
  begin
    u:=m;
    v:=y;
    if m<3 then
    begin
      inc(u,12);
      dec(v);
    end;
    WeekDay:=(d+2*u+((3*(u+1)) div 5)+v+(v div 4)-(v div 100)+(v div 400)+1) mod 7;
  end;
end;


procedure getdate(var year,month,mday,wday : word);
var
  t  : TTime_t;
  tm : Ttm;
begin
  time(t); localtime_r(t,tm);
  with tm do
  begin
    year := tm_year+1900;
    month := tm_mon+1;
    mday := tm_mday;
    wday := tm_wday;
  end;
end;


procedure setdate(year,month,day : word);
begin
end;


procedure gettime(var hour,minute,second,sec100 : word);
var
  t  : TTime_t;
  tm : Ttm;
begin
  time(t); localtime_r(t,tm);
  with tm do
  begin
    hour := tm_hour;
    minute := tm_min;
    second := tm_sec;
    sec100 := 0;
  end;
end;


procedure settime(hour,minute,second,sec100 : word);
begin
end;


function GetMsCount: int64;
var
  tv : TimeVal;
  tz : TimeZone;
begin
  FPGetTimeOfDay (tv, tz);
  GetMsCount := int64 (tv.tv_Sec) * 1000 + tv.tv_uSec div 1000;
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
    numargs,wstat : integer;
    Wiring : TWiring;
begin
  if pos ('.',path) = 0 then
   arg0 := fexpand(path+'.nlm'#0) else
    arg0 := fexpand (path)+#0;
  //writeln (stderr,'dos.exec (',path,',',comline,') arg0:"',copy(arg0,1,length(arg0)-1),'"');
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
  // i := spawnvp (P_WAIT,args[0],@args);
  Wiring.infd := StdInputHandle;  //textrec(Stdin).Handle;
  Wiring.outfd:= textrec(stdout).Handle;
  Wiring.errfd:= textrec(stderr).Handle;
  //writeln (stderr,'calling procve');
  i := procve(args[0],
              PROC_CURRENT_SPACE+PROC_INHERIT_CWD,
              envP,         // const char * env[] If passed as NULL, the child process inherits the parent.s environment at the time of the call.
              @Wiring,      // wiring_t *wiring, Pass NULL to inherit system defaults for wiring.
              nil,          // struct fd_set *fds, Not currently implemented. Pass in NULL.
              nil,          // void *appdata, Not currently implemented. Pass in NULL.
              0,            // size_t appdata_size, Not currently implemented. Pass in 0
              nil,          // void *reserved, Reserved. Pass NULL.
              @args);       // const char *argv[]
  //writeln (stderr,'Ok');
  if i <> -1 then
  begin
    Fpwaitpid(i,@wstat,0);
    doserror := 0;
    lastdosexitcode := wstat;
  end else
  begin
    doserror := 8;  // for now, what about errno ?
  end;
end;



{******************************************************************************
                               --- Disk ---
******************************************************************************}

function getvolnum (drive : byte) : longint;
{var dir : STRING[255];
    P,PS,
    V   : LONGINT;}
begin
  {if drive = 0 then
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
    getvolnum := drive-1;}
  getvolnum := -1;
end;


function diskfree(drive : byte) : int64;
{VAR Buf                 : ARRAY [0..255] OF CHAR;
    TotalBlocks         : WORD;
    SectorsPerBlock     : WORD;
    availableBlocks     : WORD;
    totalDirectorySlots : WORD;
    availableDirSlots   : WORD;
    volumeisRemovable   : WORD;
    volumeNumber        : LONGINT;}
begin
  // volumeNumber := getvolnum (drive);
  (*
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
  end else*)
    diskfree := 0;
end;


function disksize(drive : byte) : int64;
{VAR Buf                 : ARRAY [0..255] OF CHAR;
    TotalBlocks         : WORD;
    SectorsPerBlock     : WORD;
    availableBlocks     : WORD;
    totalDirectorySlots : WORD;
    availableDirSlots   : WORD;
    volumeisRemovable   : WORD;
    volumeNumber        : LONGINT;}
begin
  (*
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
  end else*)
    disksize := 0;
end;


{******************************************************************************
                     --- Utils ---
******************************************************************************}

procedure timet2dostime (timet:longint; var dostime : longint);
var tm : Ttm;
begin
  localtime_r(timet,tm);
  dostime:=(tm.tm_sec shr 1)+(tm.tm_min shl 5)+(tm.tm_hour shl 11)+(tm.tm_mday shl 16)+((tm.tm_mon+1) shl 21)+((tm.tm_year+1900-1980) shl 25);
end;

function nwattr2dosattr (nwattr : longint) : word;
begin
  nwattr2dosattr := 0;
  if nwattr and M_A_RDONLY > 0 then nwattr2dosattr := nwattr2dosattr + readonly;
  if nwattr and M_A_HIDDEN > 0 then nwattr2dosattr := nwattr2dosattr + hidden;
  if nwattr and M_A_SYSTEM > 0 then nwattr2dosattr := nwattr2dosattr + sysfile;
  if nwattr and M_A_SUBDIR > 0 then nwattr2dosattr := nwattr2dosattr + directory;
  if nwattr and M_A_ARCH   > 0 then nwattr2dosattr := nwattr2dosattr + archive;
end;


{******************************************************************************
                     --- Findfirst FindNext ---
******************************************************************************}

{returns true if attributes match}
function find_setfields (var f : searchRec) : boolean;
var
  StatBuf : TStat;
  fname   : string[255];
begin
  find_setfields := false;
  with F do
  begin
    if Magic = $AD01 then
    begin
      attr := nwattr2dosattr (Pdirent(EntryP)^.d_mode);
      size := Pdirent(EntryP)^.d_size;
      name := strpas (Pdirent(EntryP)^.d_name);
      doserror := 0;
      fname := f._dir + f.name;
      if length (fname) = 255 then dec (byte(fname[0]));
      fname := fname + #0;
      if Fpstat (@fname[1],StatBuf) = 0 then
        timet2dostime (StatBuf.st_mtim.tv_sec, time)
      else
        time := 0;
      if (f._attr and hidden) = 0 then
        if attr and hidden > 0 then exit;
      if (f._attr and Directory) = 0 then
        if attr and Directory > 0 then exit;
      if (f._attr and SysFile) = 0 then
        if attr and SysFile > 0 then exit;
      find_setfields := true;
    end else
    begin
      FillChar (f,sizeof(f),0);
      doserror := 18;
    end;
  end;
end;


procedure findfirst(const path : pathstr;attr : word;var f : searchRec);
var
  path0 : array[0..256] of char;
  p     : longint;
begin
  IF path = '' then
  begin
    doserror := 18;
    exit;
  end;
  f._attr := attr;
  p := length (path);
  while (p > 0) and (not (path[p] in AllowDirectorySeparators)) do
    dec (p);
  if p > 0 then
  begin
    f._mask := copy (path,p+1,255);
    f._dir := copy (path,1,p);
    strpcopy(path0,f._dir);
  end else
  begin
    f._mask := path;
    getdir (0,f._dir);
    if (f._dir[length(f._dir)] <> '/') and
       (f._dir[length(f._dir)] <> '\') then
      f._dir := f._dir + '/';
    strpcopy(path0,f._dir);
  end;
  if f._mask = '*' then f._mask := '';
  if f._mask = '*.*' then f._mask := '';
  //writeln (stderr,'mask: "',f._mask,'" dir:"',path0,'"');
  f._mask := f._mask + #0;
  Pdirent(f.DirP) := opendir (path0);
  if f.DirP = nil then
    doserror := 18
  else begin
    F.Magic := $AD01;
    findnext (f);
  end;
end;


procedure findnext(var f : searchRec);
begin
  if F.Magic <> $AD01 then
  begin
    doserror := 18;
    exit;
  end;
  doserror:=0;
  repeat
    Pdirent(f.EntryP) := readdir (Pdirent(f.DirP));
    if F.EntryP = nil then
      doserror := 18
    else
    if find_setfields (f) then
    begin
      if f._mask = #0 then exit;
      if fnmatch(@f._mask[1],Pdirent(f.EntryP)^.d_name,FNM_CASEFOLD) = 0 then
        exit;
    end;
  until doserror <> 0;
end;


Procedure FindClose(Var f: SearchRec);
begin
  if F.Magic <> $AD01 then
  begin
    doserror := 18;
    EXIT;
  end;
  doserror:=0;
  closedir (Pdirent(f.DirP));
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
var
  StatBuf : TStat;
begin
  doserror := 0;
  if Fpfstat (filerec (f).handle, StatBuf) = 0 then
    timet2dostime (StatBuf.st_mtim.tv_sec,time)
  else begin
    time := 0;
    doserror := ___errno^;
   end;
end;


procedure setftime(var f;time : longint);
Var
  utim: utimbuf;
  DT: DateTime;
  path: pathstr;
  tm : TTm;
Begin
  doserror:=0;
  with utim do
  begin
    actime:=libc.time(nil);  // getepochtime;
    UnPackTime(Time,DT);
    with tm do
    begin
      tm_sec   := DT.Sec;        // seconds after the minute [0..59]
      tm_min   := DT.Min;        // minutes after the hour [0..59]
      tm_hour  := DT.hour;       // hours since midnight [0..23]
      tm_mday  := DT.Day;        // days of the month [1..31]
      tm_mon   := DT.month-1;    // months since January [0..11]
      tm_year  := DT.year-1900;
      tm_wday  := -1;
      tm_yday  := -1;
      tm_isdst := -1;
    end;
    modtime:=mktime(tm);
  end;
  if utime(@filerec(f).name,utim)<0 then
  begin
    Time:=0;
    doserror:=3;
  end;
end;


procedure getfattr(var f;var attr : word);
var
  StatBuf : TStat;
{$ifndef FPC_ANSI_TEXTFILEREC}
  r: rawbytestring;
{$endif not FPC_ANSI_TEXTFILEREC}
  p: pchar;
begin
  doserror := 0;
{$ifdef FPC_ANSI_TEXTFILEREC}
  p := @filerec(f).name;
{$else FPC_ANSI_TEXTFILEREC}
  r := ToSingleByteFileSystemEncodedFileName(filerec(f).name);
  p := pchar(r);
{$endif FPC_ANSI_TEXTFILEREC}
  if Fpstat (p, StatBuf) = 0 then
    attr := nwattr2dosattr (StatBuf.st_mode)
  else
  begin
    attr := 0;
    doserror := ___errno^;
  end;
end;


procedure setfattr(var f;attr : word);
var
  StatBuf : TStat;
  newMode : longint;
{$ifndef FPC_ANSI_TEXTFILEREC}
  r: rawbytestring;
{$endif not FPC_ANSI_TEXTFILEREC}
  p: pchar;
begin
{$ifdef FPC_ANSI_TEXTFILEREC}
  p := @filerec(f).name;
{$else FPC_ANSI_TEXTFILEREC}
  r := ToSingleByteFileSystemEncodedFileName(filerec(f).name);
  p := pchar(r);
{$endif FPC_ANSI_TEXTFILEREC}
  if Fpstat (p,StatBuf) = 0 then
  begin
    newmode := StatBuf.st_mode and ($FFFF0000 - M_A_RDONLY-M_A_HIDDEN-M_A_SYSTEM-M_A_ARCH); {only this can be set by dos unit}
    newmode := newmode or M_A_BITS_SIGNIFICANT;  {set netware attributes}
    if attr and readonly > 0 then
      newmode := newmode or M_A_RDONLY;
    if attr and hidden > 0 then
      newmode := newmode or M_A_HIDDEN;
    if attr and sysfile > 0 then
      newmode := newmode or M_A_SYSTEM;
    if attr and archive > 0 then
      newmode := newmode or M_A_ARCH;
    if Fpchmod (@textrec(f).name,newMode) < 0 then
      doserror := ___errno^ else
      doserror := 0;
  end else
    doserror := ___errno^;
end;


{******************************************************************************
                             --- Environment ---
******************************************************************************}

Function EnvCount: Longint;
var
  envcnt : longint;
  p      : ppchar;
Begin
  envcnt:=0;
  p:=envp;      {defined in system}
  while (p^<>nil) do
  begin
    inc(envcnt);
    inc(p);
  end;
  EnvCount := envcnt
End;


Function EnvStr (Index: longint): String;
Var
  i : longint;
  p : ppchar;
Begin
  if Index <= 0 then
    envstr:=''
  else
  begin
    p:=envp;      {defined in system}
    i:=1;
    while (i<Index) and (p^<>nil) do
    begin
      inc(i);
      inc(p);
    end;
    if p=nil then
      envstr:=''
    else
      envstr:=strpas(p^)
  end;
end;


{ works fine (at least with netware 6.5) }
Function  GetEnv(envvar: string): string;
var envvar0 : array[0..512] of char;
    p       : pchar;
    SearchElement : string[255];
    i,isDosPath,res : longint;
begin
  if upcase(envvar) = 'PATH' then
  begin  // netware does not have search paths in the environment var PATH
         // return it here (needed for the compiler)
    GetEnv := '';
    i := 1;
    res := GetSearchPathElement (i, isdosPath, @SearchElement[0]);
    while res = 0 do
    begin
      if isDosPath = 0 then
      begin
        if GetEnv <> '' then GetEnv := GetEnv + ';';
        GetEnv := GetEnv + SearchElement;
      end;
      inc (i);
      res := GetSearchPathElement (i, isdosPath, @SearchElement[0]);
    end;
    DoDirSeparators(getenv);
  end else
  begin
    strpcopy(envvar0,envvar);
    p := libc.getenv (envvar0);
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
 while true do delay (60000);
End;


end.
