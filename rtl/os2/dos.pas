{****************************************************************************

    $Id$

                         Free Pascal Runtime-Library
                              DOS unit for OS/2
                   Copyright (c) 1997,1999-2000 by Daniel Mantione,
                   member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************}

unit dos;

{$ASMMODE ATT}

{***************************************************************************}

interface

{***************************************************************************}

{$PACKRECORDS 1}

uses    Strings, DosCalls;

const   {Bit masks for CPU flags.}
        fcarry      = $0001;
        fparity     = $0004;
        fauxiliary  = $0010;
        fzero       = $0040;
        fsign       = $0080;
        foverflow   = $0800;

        {Bit masks for file attributes.}
        readonly    = $01;
        hidden      = $02;
        sysfile     = $04;
        volumeid    = $08;
        directory   = $10;
        archive     = $20;
        anyfile     = $3F;

        fmclosed    = $D7B0;
        fminput     = $D7B1;
        fmoutput    = $D7B2;
        fminout     = $D7B3;

type    {Some string types:}
        comstr=string;              {Filenames can be long in OS/2.}
        pathstr=string;             {String for pathnames.}
        dirstr=string;              {String for a directory}
        namestr=string;             {String for a filename.}
        extstr=string[40];          {String for an extension. Can be 253
                                     characters long, in theory, but let's
                                     say fourty will be enough.}

        {Search record which is used by findfirst and findnext:}
        searchrec=record
            case boolean of
             false: (handle:longint;     {Used in os_OS2 mode}
                     FStat:PFileFindBuf3;
                     fill2:array[1..21-SizeOf(longint)-SizeOf(pointer)] of byte;
                     attr2:byte;
                     time2:longint;
                     size2:longint;
                     name2:string);      {Filenames can be long in OS/2!}
             true:  (fill:array[1..21] of byte;
                     attr:byte;
                     time:longint;
                     size:longint;
                     name:string);       {Filenames can be long in OS/2!}
        end;

{$i filerec.inc}
{$i textrec.inc}

        {Data structure for the registers needed by msdos and intr:}
       registers=packed record
            case i:integer of
                0:(ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,
                   f8,flags,fs,gs:word);
                1:(al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh:byte);
                2:(eax,ebx,ecx,edx,ebp,esi,edi:longint);
            end;

        {Record for date and time:}
        datetime=record
            year,month,day,hour,min,sec:word;
        end;

        {Flags for the exec procedure:

        Starting the program:
        efwait:        Wait until program terminates.
        efno_wait:     Don't wait until the program terminates. Does not work
                       in dos, as DOS cannot multitask.
        efoverlay:     Terminate this program, then execute the requested
                       program. WARNING: Exit-procedures are not called!
        efdebug:       Debug program. Details are unknown.
        efsession:     Do not execute as child of this program. Use a seperate
                       session instead.
        efdetach:      Detached. Function unknown. Info wanted!
        efpm:          Run as presentation manager program.

 Not found info about execwinflags

        Determining the window state of the program:
        efdefault:     Run the pm program in it's default situation.
        efminimize:    Run the pm program minimized.
        efmaximize:    Run the pm program maximized.
        effullscreen:  Run the non-pm program fullscreen.
        efwindowed:    Run the non-pm program in a window.

}
        type    execrunflags=(efwait,efno_wait,efoverlay,efdebug,efsession,
                              efdetach,efpm);
                execwinflags=(efdefault,efminimize,efmaximize,effullscreen,
                              efwindowed);

const
(* For compatibility with VP/2, used for runflags in Exec procedure. *)
    ExecFlags: cardinal = ord (efwait);

var doserror:integer;
    dosexitcode:word;

procedure getdate(var year,month,day,dayofweek:word);
procedure gettime(var hour,minute,second,sec100:word);
function dosversion:word;
procedure setdate(year,month,day:word);
procedure settime(hour,minute,second,sec100:word);
procedure getcbreak(var breakvalue:boolean);
procedure setcbreak(breakvalue:boolean);
procedure getverify(var verify:boolean);
procedure setverify(verify : boolean);

function DiskFree (Drive: byte) : int64;
function DiskSize (Drive: byte) : int64;

procedure findfirst(const path:pathstr;attr:word;var f:searchRec);
procedure findnext(var f:searchRec);
procedure findclose(var f:searchRec);

{Is a dummy:}
procedure swapvectors;

{Not supported:
procedure getintvec(intno:byte;var vector:pointer);
procedure setintvec(intno:byte;vector:pointer);
procedure keep(exitcode:word);
procedure msdos(var regs:registers);
procedure intr(intno : byte;var regs:registers);
}

procedure getfattr(var f;var attr:word);
procedure setfattr(var f;attr:word);

function fsearch(path:pathstr;dirlist:string):pathstr;
procedure getftime(var f;var time:longint);
procedure setftime(var f;time:longint);
procedure packtime (var d:datetime; var time:longint);
procedure unpacktime (time:longint; var d:datetime);
function fexpand(const path:pathstr):pathstr;
procedure fsplit(path:pathstr;var dir:dirstr;var name:namestr;
                 var ext:extstr);
procedure exec(const path:pathstr;const comline:comstr);
function exec(path:pathstr;runflags:execrunflags;winflags:execwinflags;
              const comline:comstr):longint;
function envcount:longint;
function envstr(index:longint) : string;
function GetEnvPChar (EnvVar: string): PChar;
function getenv(const envvar:string): string;

implementation

var     LastSR: SearchRec;

type    TBA = array [1..SizeOf (SearchRec)] of byte;
        PBA = ^TBA;

const   FindResvdMask = $00003737; {Allowed bits in attribute
                                    specification for DosFindFirst call.}


function fsearch(path:pathstr;dirlist:string):pathstr;
Var
  A: array [0..255] of char;
  D, P: AnsiString;
begin
  P:=Path;
  D:=DirList;
  DosError:=DosSearchPath(0, PChar(D), PChar(P), @A, 255);
  fsearch := StrPas (@A);
end;

procedure getftime(var f;var time:longint);
var
  FStat: PFileStatus3;
begin
  New (FStat);
  DosError:=DosQueryFileInfo(FileRec(F).Handle, 1, FStat, SizeOf(FStat^));
  If DosError=0 then
    Time:=FStat^.timelastwrite+FStat^.DateLastWrite*256
  else
    Time:=0;
  Dispose (FStat);
end;

procedure SetFTime (var F; Time: longint);
var FStat: PFileStatus3;
    RC: longint;
begin
  New (FStat);
  RC := DosQueryFileInfo (FileRec (F).Handle, ilStandard, @FStat,
                                                    SizeOf (FStat));
  if RC = 0 then
  begin
    FStat^.DateLastAccess := Hi (Time);
    FStat^.DateLastWrite := Hi (Time);
    FStat^.TimeLastAccess := Lo (Time);
    FStat^.TimeLastWrite := Lo (Time);
    RC := DosSetFileInfo (FileRec (F).Handle, ilStandard,
                                       FStat, SizeOf (FStat^));
  end;
  DosError := integer(RC);
  Dispose (FStat);
end;

procedure exec(const path:pathstr;const comline:comstr);
{Execute a program.}
begin
    dosexitcode:=word(exec(path,execrunflags(ExecFlags),efdefault,comline));
end;

function exec(path:pathstr;runflags:execrunflags;winflags:execwinflags;
              const comline:comstr):longint;
{Execute a program. More suitable for OS/2 than the exec above.}
var args:Pbytearray;
    env:Pbytearray;
    i,argsize:word;
    esadr:pointer;
    d:dirstr;
    n:namestr;
    e:extstr;
    p : ppchar;
    j : integer;
    res: TResultCodes;
    ObjName: String;
const
    ArgsSize = 2048; (* Amount of memory reserved for arguments in bytes. *)
begin
    getmem(args,ArgsSize);
    GetMem(env, envc*sizeof(pchar)+16384);
    {Now setup the arguments. The first argument should be the program
     name without directory and extension.}
    fsplit(path,d,n,e);
//    args^[0]:=$80;
    argsize:=0;
    for i:=1 to length(n) do
        begin
            args^[argsize]:=byte(n[i]);
            inc(argsize);
        end;
    args^[argsize]:=0;
    inc(argsize);
    {Now do the real arguments.}
    i:=1;
    while i<=length(comline) do
        begin
            if comline[i]<>' ' then
                begin
                    {Commandline argument found. Copy it.}
//                    args^[argsize]:=$80;
//                    inc(argsize);
                    while (i<=length(comline)) and (comline[i]<>' ') do
                        begin
                            args^[argsize]:=byte(comline[i]);
                            inc(argsize);
                            inc(i);
                        end;
                    args^[argsize]:=32;//0;
                    inc(argsize);
                end;
            inc(i);
        end;
    args^[argsize]:=0;
    inc(argsize);

    {Commandline ready, now build the environment.

     Oh boy, I always had the opinion that executing a program under Dos
     was a hard job!}

    asm
        movl env,%edi       {Setup destination pointer.}
        movl envc,%ecx      {Load number of arguments in edx.}
        movl envp,%esi      {Load env. strings.}
        xorl %edx,%edx      {Count environment size.}
.Lexa1:
        lodsl               {Load a Pchar.}
        xchgl %eax,%ebx
.Lexa2:
        movb (%ebx),%al     {Load a byte.}
        incl %ebx           {Point to next byte.}
        stosb               {Store it.}
        incl %edx           {Increase counter.}
        cmpb $0,%al         {Ready ?.}
        jne .Lexa2
        loop .Lexa1           {Next argument.}
        stosb               {Store an extra 0 to finish. (AL is now 0).}
        incl %edx
//        movw %dx,ES.SizeEnv    {Store environment size.}
    end ['eax','ebx','ecx','edx','esi','edi'];

    //Not clear how to use
    exec:=DosExecPgm(ObjName, Longint(runflags), Args, Env, Res, Path);

    freemem(args,ArgsSize);
    FreeMem(env, envc*sizeof(pchar)+16384);
    {Phew! That's it. This was the most sophisticated procedure to call
     a system function I ever wrote!}
end;

function dosversion:word;
{Returns OS/2 version}
var
  Minor, Major: Cardinal;
begin
  DosQuerySysInfo(svMajorVersion, svMajorVersion, Major, 4);
  DosQuerySysInfo(svMinorVersion, svMinorVersion, Minor, 4);
  DosVersion:=Major or Minor shl 8;
end;

procedure GetDate (var Year, Month, Day, DayOfWeek: word);
Var
  dt: TDateTime;
begin
  DosGetDateTime(dt);
  Year:=dt.year;
  Month:=dt.month;
  Day:=dt.Day;
  DayofWeek:=dt.Weekday;
end;

procedure SetDate (Year, Month, Day: word);
var
  DT: TDateTime;
begin
  DosGetDateTime (DT);
  DT.Year := Year;
  DT.Month := byte (Month);
  DT.Day := byte (Day);
  DosSetDateTime (DT);
end;

procedure GetTime (var Hour, Minute, Second, Sec100: word);
var
  dt: TDateTime;
begin
  DosGetDateTime(dt);
  Hour:=dt.Hour;
  Minute:=dt.Minute;
  Second:=dt.Second;
  Sec100:=dt.Hundredths;
end;

procedure SetTime (Hour, Minute, Second, Sec100: word);
var
  DT: TDateTime;
begin
  DosGetDateTime (DT);
  DT.Hour := byte (Hour);
  DT.Minute := byte (Minute);
  DT.Second := byte (Second);
  DT.Sec100 := byte (Sec100);
  DosSetDateTime (DT);
end;

procedure getcbreak(var breakvalue:boolean);
begin
  breakvalue := True;
end;

procedure setcbreak(breakvalue:boolean);
begin
end;

procedure getverify(var verify:boolean);
begin
  verify := true;
end;

procedure setverify(verify:boolean);
begin
end;


function DiskFree (Drive: byte): int64;
var FI: TFSinfo;
    RC: longint;
begin
  {In OS/2, we use the filesystem information.}
  RC := DosQueryFSInfo (Drive, 1, FI, SizeOf (FI));
  if RC = 0 then
      DiskFree := int64 (FI.Free_Clusters) *
         int64 (FI.Sectors_Per_Cluster) * int64 (FI.Bytes_Per_Sector)
  else
      DiskFree := -1;
end;

function DiskSize (Drive: byte): int64;
var FI: TFSinfo;
    RC: longint;
begin
  RC := DosQueryFSinfo (Drive, 1, FI, SizeOf (FI));
  if RC = 0 then
      DiskSize := int64 (FI.Total_Clusters) *
         int64 (FI.Sectors_Per_Cluster) * int64 (FI.Bytes_Per_Sector)
  else
      DiskSize := -1;
end;


procedure SearchRec2DosSearchRec (var F: SearchRec);
begin
end;

procedure DosSearchRec2SearchRec (var F: SearchRec);
const
  NameSize=255;
var
  L, I: longint;
type
  TRec = record
    T, D: word;
  end;
begin
 with F do
    begin
        Name := FStat^.Name;
        Size := FStat^.FileSize;
        Attr := byte(FStat^.AttrFile and $FF);
        TRec (Time).T := FStat^.TimeLastWrite;
        TRec (Time).D := FStat^.DateLastWrite;
    end;
end;

procedure FindFirst (const Path: PathStr; Attr: word; var F: SearchRec);


var path0: array[0..255] of char;
    Count: cardinal;

begin
  {No error.}
  DosError := 0;
      New (F.FStat);
      F.Handle := longint ($FFFFFFFF);
      Count := 1;
      DosError := integer (DosFindFirst (Path, F.Handle,
                     Attr and FindResvdMask, F.FStat, SizeOf (F.FStat^),
                                                         Count, ilStandard));
      if (DosError = 0) and (Count = 0) then DosError := 18;
  DosSearchRec2SearchRec (F);
end;

procedure FindNext (var F: SearchRec);
var
  Count: cardinal;
begin
    {No error}
    DosError := 0;
    SearchRec2DosSearchRec (F);
        Count := 1;
        DosError := integer (DosFindNext (F.Handle, F.FStat, SizeOf (F.FStat^),
                                                                       Count));
        if (DosError = 0) and (Count = 0) then DosError := 18;
    DosSearchRec2SearchRec (F);
end;

procedure FindClose (var F: SearchRec);
begin
  if F.Handle <> $FFFFFFFF then DosError := DosFindClose (F.Handle);
  Dispose (F.FStat);
end;

procedure swapvectors;
{For TP compatibility, this exists.}
begin
end;

function envcount:longint;
begin
  envcount:=envc;
end;

function envstr(index : longint) : string;

var hp:Pchar;

begin
    if (index<=0) or (index>envcount) then
        begin
            envstr:='';
            exit;
        end;
    hp:=EnvP[index-1];
    envstr:=strpas(hp);
end;

function GetEnvPChar (EnvVar: string): PChar;
(* The assembler version is more than three times as fast as Pascal. *)
var
 P: PChar;
begin
 EnvVar := UpCase (EnvVar);
{$ASMMODE INTEL}
 asm
  cld
  mov edi, Environment
  lea esi, EnvVar
  xor eax, eax
  lodsb
@NewVar:
  cmp byte ptr [edi], 0
  jz @Stop
  push eax        { eax contains length of searched variable name }
  push esi        { esi points to the beginning of the variable name }
  mov ecx, -1     { our character ('=' - see below) _must_ be found }
  mov edx, edi    { pointer to beginning of variable name saved in edx }
  mov al, '='     { searching until '=' (end of variable name) }
  repne
  scasb           { scan until '=' not found }
  neg ecx         { what was the name length? }
  dec ecx         { corrected }
  dec ecx         { exclude the '=' character }
  pop esi         { restore pointer to beginning of variable name }
  pop eax         { restore length of searched variable name }
  push eax        { and save both of them again for later use }
  push esi
  cmp ecx, eax    { compare length of searched variable name with name }
  jnz @NotEqual   { ... of currently found variable, jump if different }
  xchg edx, edi   { pointer to current variable name restored in edi }
  repe
  cmpsb           { compare till the end of variable name }
  xchg edx, edi   { pointer to beginning of variable contents in edi }
  jz @Equal       { finish if they're equal }
@NotEqual:
  xor eax, eax    { look for 00h }
  mov ecx, -1     { it _must_ be found }
  repne
  scasb           { scan until found }
  pop esi         { restore pointer to beginning of variable name }
  pop eax         { restore length of searched variable name }
  jmp @NewVar     { ... or continue with new variable otherwise }
@Stop:
  xor eax, eax
  mov P, eax      { Not found - return nil }
  jmp @End
@Equal:
  pop esi         { restore the stack position }
  pop eax
  mov P, edi      { place pointer to variable contents in P }
@End:
 end ['eax','ecx','edx','esi','edi'];
 GetEnvPChar := P;
end;
{$ASMMODE ATT}

function GetEnv (const EnvVar: string): string;
(* The assembler version is more than three times as fast as Pascal. *)
begin
 GetEnv := StrPas (GetEnvPChar (EnvVar));
end;

procedure fsplit(path:pathstr;var dir:dirstr;var name:namestr;
                 var ext:extstr);

var p1,i : longint;
    dotpos : integer;

begin
    { allow slash as backslash }
    for i:=1 to length(path) do
      if path[i]='/' then path[i]:='\';
    {Get drive name}
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
end;

(*
function FExpand (const Path: PathStr): PathStr;
- declared in fexpand.inc
*)

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)

const
    LFNSupport = true;

{$I fexpand.inc}

{$UNDEF FPC_FEXPAND_DRIVES}
{$UNDEF FPC_FEXPAND_UNC}

procedure packtime(var d:datetime;var time:longint);

var zs:longint;

begin
    time:=-1980;
    time:=time+d.year and 127;
    time:=time shl 4;
    time:=time+d.month;
    time:=time shl 5;
    time:=time+d.day;
    time:=time shl 16;
    zs:=d.hour;
    zs:=zs shl 6;
    zs:=zs+d.min;
    zs:=zs shl 5;
    zs:=zs+d.sec div 2;
    time:=time+(zs and $ffff);
end;

procedure unpacktime (time:longint;var d:datetime);

begin
    d.sec:=(time and 31) * 2;
    time:=time shr 5;
    d.min:=time and 63;
    time:=time shr 6;
    d.hour:=time and 31;
    time:=time shr 5;
    d.day:=time and 31;
    time:=time shr 5;
    d.month:=time and 15;
    time:=time shr 4;
    d.year:=time+1980;
end;

procedure getfattr(var f;var attr : word);
var
  PathInfo: PFileStatus3;
begin
  New (PathInfo);
  Attr:=0;
  DosError:=DosQueryPathInfo(FileRec(F).Name, ilStandard, PathInfo, SizeOf(PathInfo^));
  if DosError=0 then
    Attr := PathInfo^.attrFile;
  Dispose (PathInfo);
end;

procedure setfattr(var f;attr : word);
var
  PathInfo: PFileStatus3;
begin
  New (PathInfo);
  DosError:=DosQueryPathInfo(FileRec(F).Name, ilStandard, PathInfo, SizeOf(PathInfo^));
  if DosError=0 then
  begin
    PathInfo^.attrFile:=Attr;
    DosError:=DosSetPathInfo(FileRec(F).Name, ilStandard, PathInfo, SizeOf(PathInfo^), doWriteThru);
  end;
  Dispose (PathInfo);
end;

begin
end.
{
  $Log$
  Revision 1.28  2003-10-05 22:06:43  hajny
    * result buffers must be allocated

  Revision 1.27  2003/10/03 21:46:41  peter
    * stdcall fixes

  Revision 1.26  2003/09/24 08:59:16  yuri
  * Prepared for native target (emx code replaced)

  Revision 1.25  2003/02/20 17:37:00  hajny
    * correction for previous mistyping

  Revision 1.24  2003/02/20 17:09:49  hajny
    * fixes for OS/2 v2.1 incompatibility

  Revision 1.23  2003/01/04 15:43:50  hajny
    + GetEnvPChar added

  Revision 1.22  2002/12/07 19:46:56  hajny
    * mistyping fixed

  Revision 1.21  2002/12/07 19:17:13  hajny
    * GetEnv correction, better PM support, ...

  Revision 1.20  2002/11/18 19:51:00  hajny
    * another bunch of type corrections

  Revision 1.19  2002/09/07 16:01:24  peter
    * old logs removed and tabs fixed

  Revision 1.18  2002/07/11 16:00:05  hajny
    * FindFirst fix (invalid attribute bits masked out)

  Revision 1.17  2002/07/07 18:00:48  hajny
    * DosGetInfoBlock modification to allow overloaded version (in DosCalls)

  Revision 1.16  2002/03/03 11:19:20  hajny
    * GetEnv rewritten to assembly - 3x faster now

}
