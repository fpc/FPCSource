{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Dos unit for BP7 compatible RTL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit dos;
interface

uses beos;

const
  FileNameLen=255;

type
  ComStr  = String[FileNameLen];
  PathStr = String[FileNameLen];
  DirStr  = String[FileNameLen];
  NameStr = String[FileNameLen];
  ExtStr  = String[FileNameLen];


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
  directory = $10;
  archive   = $20;
  anyfile   = $3F;

  {File Status}
  fmclosed = $D7B0;
  fminput  = $D7B1;
  fmoutput = $D7B2;
  fminout  = $D7B3;


  S_IFMT  = $F000; { type of file }
  S_IFLNK = $A000; { symbolic link }
  S_IFREG =     $8000; { regular }
  S_IFBLK =     $6000; { block special }
  S_IFDIR =     $4000; { directory }
  S_IFCHR =     $2000; { character special }
  S_IFIFO =     $1000; { fifo }

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

  searchrec = record
     fd       : longint;
     path     : string;
     fname    : string;
     attr     : byte;
     time     : longint;
     size     : longint;
     name     : string[255];
  end;


Var
  DosError : integer;

{Info/Date/Time}
Procedure GetDate(var year, month, mday, wday: word);
procedure GetTime(var hour,min,sec,msec,usec:word);
procedure GetTime(var hour,min,sec,sec100:word);
procedure GetTime(Var Hour,Min,Sec:Word);

Procedure UnpackTime(p: longint; var t: datetime);
Procedure PackTime(var t: datetime; var p: longint);

{Exec}
Procedure Exec(const path: pathstr; const comline: comstr);
Function  DosExitCode: word;


{Disk}
Procedure FindFirst(const path: pathstr; attr: word; var f: searchRec);
Procedure FindNext(var f: searchRec);
Procedure FindClose(var f: searchRec);

{File}
{Procedure GetFAttr(var f:File; var attr: word);}
procedure GetFTime(var f:File; var time: longint);
procedure GetFTime(f:string; var time: longint);
Procedure SetFTime(var f:File; time : longint);
Function  FSearch(path: pathstr; dirlist: string): pathstr;
Function  FExpand(const path: pathstr): pathstr;
Procedure FSplit(path: pathstr; var dir: dirstr; var name: namestr; var ext: extstr);




{Environment}
{Function  EnvCount: longint;
Function  EnvStr(index: integer): string;}

{Misc}
{Procedure SetFAttr(var f; attr: word);
Procedure SetFTime(var f; time: longint);
Procedure GetVerify(var verify: boolean);
Procedure SetVerify(verify: boolean);}

{Do Nothing Functions}
Procedure SwapVectors;
{Procedure GetIntVec(intno: byte; var vector: pointer);
Procedure SetIntVec(intno: byte; vector: pointer);
Procedure Keep(exitcode: word);}
function GetEnv(EnvVar: String): String;


Procedure EpochToLocal(epoch:longint;var year,month,day,hour,minute,second:Word);


implementation

uses strings;


procedure GetFTime(var f:file; var time: longint);
var info:stat;
    t:longint;
    dt:DateTime;
begin
  if not FStat(F,Info) then begin
    t:=0;
    doserror:=3;
    exit;
  end else t:=info.ctime;
  EpochToLocal(t,dt.year,dt.month,dt.day,dt.hour,dt.min,dt.sec);
  packtime(dt,time);
end;

procedure GetFTime(f:string; var time: longint);
var info:stat;
    t:longint;
    dt:DateTime;
begin
  if not FStat(F,Info) then begin
    t:=0;
    doserror:=3;
    exit;
  end else t:=info.ctime;
  EpochToLocal(t,dt.year,dt.month,dt.day,dt.hour,dt.min,dt.sec);
  packtime(dt,time);
end;


type utimbuf=record actime,modtime:longint; end;
{function _utime (path:pchar;var buf:utimbuf):longint; cdecl; external name 'utime';}

Procedure setftime(var f:file; time : longint);
{var buf:utimbuf;}
begin
{  buf.actime:=time;
  buf.modtime:=time;}
{  writeln ('SetFTime ',PChar(@FileRec(f).Name),' := ',time);}
{  if _utime(PChar(@FileRec(f).Name),buf)<>0 then doserror:=3;}
end;


{******************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}


procedure getdate(var year,month,mday,wday : word);
begin
end;

function sys_time:longint; cdecl; external name 'sys_time';


procedure GetTime(var hour,min,sec,msec,usec:word);
{
  Gets the current time, adjusted to local time
}
var
  year,day,month:Word;
  t : longint;
begin
  t:=sys_time;
  EpochToLocal(t,year,month,day,hour,min,sec);
  msec:=0;
  usec:=0;
end;

procedure GetTime(var hour,min,sec,sec100:word);
{ Gets the current time, adjusted to local time }
var usec : word;
begin
  gettime(hour,min,sec,sec100,usec);
  sec100:=sec100 div 10;
end;

procedure GetTime(Var Hour,Min,Sec:Word);
{
  Gets the current time, adjusted to local time
  }
  var
    msec,usec : Word;
    Begin
      gettime(hour,min,sec,msec,usec);
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


Procedure Exec(const path: pathstr; const comline: comstr);
var p:string;
begin
  p:=path+' '+comline;
  doserror:=beos.shell(p);
end;

Function DosExitCode: word;
begin
    dosexitcode:=doserror;
end;




{******************************************************************************
                               --- File ---
******************************************************************************}

Procedure FSplit(Path: PathStr; Var Dir: DirStr; Var Name: NameStr;Var Ext: ExtStr);

Begin
  beos.FSplit(Path,Dir,Name,Ext);
End;

Function FExpand(Const Path: PathStr): PathStr;
Begin
  FExpand:=beos.FExpand(Path);
End;

Function FSearch(path : pathstr;dirlist : string) : pathstr;
Var info:stat;
Begin
if (length(Path)>0) and (path[1]='/') and FStat(path,info) then
    FSearch:=path
  else
      FSearch:=beos.FSearch(path,dirlist);
End;



{******************************************************************************
                     --- Findfirst FindNext ---
******************************************************************************}

{procedure dossearchrec2searchrec(var f : searchrec);
var
  len : longint;
begin
  len:=StrLen(@f.Name);
  Move(f.Name[0],f.Name[1],Len);
  f.Name[0]:=chr(len);
end;}

type dirent = packed record
        d_dev:longint;
        d_pdev:longint;
        d_ino:int64;
        d_pino:int64;
        d_reclen:word;
        d_name:array[0..255] of char;
end;

function sys_opendir (a:dword;path:pchar;b:longint):longint; cdecl; external name 'sys_opendir';
function sys_readdir (fd:longint;var de:dirent;a:longint;b:byte):longint; cdecl; external name 'sys_readdir';

procedure findnext(var f : searchRec);
var len:longint;
    ent:dirent;
    info:stat;
    dt:DateTime;
begin
   if sys_readdir(f.fd,ent,$11C,1)=0 then begin
    doserror:=3;
    exit;
  end;
{   writeln ('NAME: ',pchar(@ent.d_name[0]));}

  len:=StrLen(@ent.d_name);
  Move(ent.d_name,f.name[1],len);
  f.name[0]:=chr(len);
{  writeln ('NAME: "',f.path+f.name,'"');}

  if not FStat(f.path+f.name,info) then begin
    writeln ('NOT FOUND');
    doserror:=3;
    exit;
  end;
  writeln ('OK');

  f.size:=info.size;

  f.attr:=0;
  if (info.mode and S_IFMT)=S_IFDIR then f.attr:=directory;

  EpochToLocal(info.mtime,dt.year,dt.month,dt.day,dt.hour,dt.min,dt.sec);
  packtime(dt,f.time);
  doserror:=0;

end;


procedure findfirst(const path : pathstr;attr : word;var f : searchRec);
var tmp:string;
    info:stat;
    ext:string;
begin
  tmp:=path;
  if tmp='' then tmp:='.';

  if FStat(tmp,info) then begin
    if ((info.mode and S_IFMT)=S_IFDIR) and (tmp[length(tmp)]<>'/') then tmp:=tmp+'/';
  end;

  FSplit (tmp,f.path,f.fname,ext);
{  f.path:=FExpand(f.path);}
  f.fname:=f.fname+ext;
  if length(f.fname)=0 then f.fname:='*';

  tmp:=tmp+#0;
  f.fd:=sys_opendir ($FF000000,@tmp[1],0);
  writeln ('F.PATH=',f.path,'   ;   ',f.fname);
  findnext(f);
end;

Procedure FindClose(Var f: SearchRec);
begin
  DosError:=0;
end;


procedure swapvectors;
begin
{ no beos equivalent }
  DosError:=0;
end;



{******************************************************************************
                             --- Environment ---
******************************************************************************}

function envcount : longint;
var
  hp : ppchar;
begin
  hp:=envp;
  envcount:=0;
  while assigned(hp^) do
   begin
     inc(envcount);
     hp:=hp+4;
   end;
end;


function envstr(index : integer) : string;
begin
  if (index<=0) or (index>envcount) then
   begin
     envstr:='';
     exit;
   end;
  envstr:=strpas(ppchar(envp+4*(index-1))^);
end;


{******************************************************************************
                             --- Not Supported ---
******************************************************************************}

Procedure keep(exitcode : word);
Begin
End;

Procedure getintvec(intno : byte;var vector : pointer);
Begin
End;

Procedure setintvec(intno : byte;vector : pointer);
Begin
End;




{******************************************************************************
                       Date and Time related calls
******************************************************************************}

Const
{Date Translation}
  C1970=2440588;
  D0   =   1461;
  D1   = 146097;
  D2   =1721119;

Procedure JulianToGregorian(JulianDN:LongInt;Var Year,Month,Day:Word);
Var
  YYear,XYear,Temp,TempMonth : LongInt;
Begin
  Temp:=((JulianDN-D2) shl 2)-1;
  JulianDN:=Temp Div D1;
  XYear:=(Temp Mod D1) or 3;
  YYear:=(XYear Div D0);
  Temp:=((((XYear mod D0)+4) shr 2)*5)-3;
  Day:=((Temp Mod 153)+5) Div 5;
  TempMonth:=Temp Div 153;
  If TempMonth>=10 Then
  Begin
    inc(YYear);
    dec(TempMonth,12);
  End;
  inc(TempMonth,3);
  Month := TempMonth;
  Year:=YYear+(JulianDN*100);
end;


Procedure EpochToLocal(epoch:longint;var year,month,day,hour,minute,second:Word);
{ Transforms Epoch time into local time (hour, minute,seconds) }
Var
  DateNum: LongInt;
Begin
  Datenum:=(Epoch Div 86400) + c1970;
  JulianToGregorian(DateNum,Year,Month,day);
  Epoch:=Epoch Mod 86400;
  Hour:=Epoch Div 3600;
  Epoch:=Epoch Mod 3600;
  Minute:=Epoch Div 60;
  Second:=Epoch Mod 60;
End;


{
  $Log$
  Revision 1.3  2002-09-07 16:01:17  peter
    * old logs removed and tabs fixed

}



Function StringToPPChar(Var S:STring):ppchar;
{
  Create a PPChar to structure of pchars which are the arguments specified
  in the string S. Especially usefull for creating an ArgV for Exec-calls
}
var
  nr  : longint;
  Buf : ^char;
  p   : ppchar;
begin
  s:=s+#0;
  buf:=@s[1];
  nr:=0;
  while(buf^<>#0) do
   begin
     while (buf^ in [' ',#8,#10]) do
      inc(buf);
     inc(nr);
     while not (buf^ in [' ',#0,#8,#10]) do
      inc(buf);
   end;
  getmem(p,nr*4);
  StringToPPChar:=p;
  if p=nil then
   begin
{     LinuxError:=sys_enomem;}
     exit;
   end;
  buf:=@s[1];
  while (buf^<>#0) do
   begin
     while (buf^ in [' ',#8,#10]) do
      begin
        buf^:=#0;
        inc(buf);
      end;
     p^:=buf;
     inc(p);
     p^:=nil;
     while not (buf^ in [' ',#0,#8,#10]) do
      inc(buf);
   end;
end;



Function Dirname(Const path:pathstr):pathstr;
{
  This function returns the directory part of a complete path.
  Unless the directory is root '/', The last character is not
  a slash.
}
var
  Dir  : PathStr;
  Name : NameStr;
  Ext  : ExtStr;
begin
  FSplit(Path,Dir,Name,Ext);
  if length(Dir)>1 then
   Delete(Dir,length(Dir),1);
  DirName:=Dir;
end;



Function Basename(Const path:pathstr;Const suf:pathstr):pathstr;
{
  This function returns the filename part of a complete path. If suf is
  supplied, it is cut off the filename.
}
var
  Dir  : PathStr;
  Name : NameStr;
  Ext  : ExtStr;
begin
  FSplit(Path,Dir,Name,Ext);
  if Suf<>Ext then
   Name:=Name+Ext;
  BaseName:=Name;
end;


function GetEnv(EnvVar: String): String;
var p:pchar;
begin
  p:=beos.GetEnv(EnvVar);
  if p=nil then
    GetEnv:=''
  else
    GetEnv:=StrPas(p);
end;


end.







