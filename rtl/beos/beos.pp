unit beos;

interface

type
    Stat = packed record
      dev:longint;     {"device" that this file resides on}
      ino:int64;       {this file's inode #, unique per device}
      mode:dword;      {mode bits (rwx for user, group, etc)}
      nlink:longint;   {number of hard links to this file}
      uid:dword;       {user id of the owner of this file}
      gid:dword;       {group id of the owner of this file}
      size:int64;      {size of this file (in bytes)}
      rdev:longint;    {device type (not used)}
      blksize:longint; {preferref block size for i/o}
      atime:longint;   {last access time}
      mtime:longint;   {last modification time}
      ctime:longint;   {last change time, not creation time}
      crtime:longint;  {creation time}
    end;
    PStat=^Stat;
    TStat=Stat;

                ComStr  = String[255];
                  PathStr = String[255];
                    DirStr  = String[255];
                      NameStr = String[255];
        ExtStr  = String[255];

function FStat(Path:String;Var Info:stat):Boolean;
function FStat(var f:File;Var Info:stat):Boolean;
function GetEnv(P: string): pchar;

function  FExpand(Const Path: PathStr):PathStr;
function  FSearch(const path:pathstr;dirlist:string):pathstr;
procedure FSplit(const Path:PathStr;Var Dir:DirStr;Var Name:NameStr;Var Ext:ExtStr);
function  Dirname(Const path:pathstr):pathstr;
function  Basename(Const path:pathstr;Const suf:pathstr):pathstr;
function  FNMatch(const Pattern,Name:string):Boolean;
{function  StringToPPChar(Var S:STring):ppchar;}

function PExists(path:string):boolean;
function FExists(path:string):boolean;

Function Shell(const Command:String):Longint;

implementation

uses strings;

{$i filerec.inc}
{$i textrec.inc}

function sys_stat (a:cardinal;path:pchar;info:pstat;n:longint):longint; cdecl; external name 'sys_stat';

function FStat(Path:String;Var Info:stat):Boolean;
{
  Get all information on a file, and return it in Info.
}
var tmp:string;
var p:pchar;
begin
  tmp:=path+#0;
  p:=@tmp[1];
  FStat:=(sys_stat($FF000000,p,@Info,0)=0);
end;

function FStat(var f:File;Var Info:stat):Boolean;
{
  Get all information on a file, and return it in Info.
}
begin
  FStat:=(sys_stat($FF000000,PChar(@FileRec(f).Name),@Info,0)=0);
end;



Function GetEnv(P:string):Pchar;
{
  Searches the environment for a string with name p and
  returns a pchar to it's value.
  A pchar is used to accomodate for strings of length > 255
}
var
  ep    : ppchar;
  found : boolean;
Begin
  p:=p+'=';            {Else HOST will also find HOSTNAME, etc}
  ep:=envp;
  found:=false;
  if ep<>nil then
   begin
     while (not found) and (ep^<>nil) do
      begin
        if strlcomp(@p[1],(ep^),length(p))=0 then
         found:=true
        else
         inc(ep);
      end;
   end;
  if found then
   getenv:=ep^+length(p)
  else
   getenv:=nil;
{  writeln ('GETENV (',P,') =',getenv);}
end;



Function StringToPPChar(Var S:String; Var nr:longint):ppchar;
{
  Create a PPChar to structure of pchars which are the arguments specified
  in the string S. Especially usefull for creating an ArgV for Exec-calls
}
var
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



{
function FExpand (const Path: PathStr): PathStr;
- declared in fexpand.inc
}

{$DEFINE FPC_FEXPAND_TILDE} { Tilde is expanded to home }
{$DEFINE FPC_FEXPAND_GETENVPCHAR} { GetEnv result is a PChar }

{$I fexpand.inc}

{$UNDEF FPC_FEXPAND_GETENVPCHAR}
{$UNDEF FPC_FEXPAND_TILDE}



Function FSearch(const path:pathstr;dirlist:string):pathstr;
{
  Searches for a file 'path' in the list of direcories in 'dirlist'.
  returns an empty string if not found. Wildcards are NOT allowed.
  If dirlist is empty, it is set to '.'
}
Var
  NewDir : PathStr;
  p1     : Longint;
  Info   : Stat;
Begin
{Replace ':' with ';'}
  for p1:=1to length(dirlist) do
   if dirlist[p1]=':' then
    dirlist[p1]:=';';
{Check for WildCards}
  If (Pos('?',Path) <> 0) or (Pos('*',Path) <> 0) Then
   FSearch:='' {No wildcards allowed in these things.}
  Else
   Begin
     Dirlist:='.;'+dirlist;{Make sure current dir is first to be searched.}
     Repeat
       p1:=Pos(';',DirList);
       If p1=0 Then
        p1:=255;
       NewDir:=Copy(DirList,1,P1 - 1);
       if NewDir[Length(NewDir)]<>'/' then
        NewDir:=NewDir+'/';
       NewDir:=NewDir+Path;
       Delete(DirList,1,p1);
       if FStat(NewDir,Info) then
        Begin
          If Pos('./',NewDir)=1 Then
           Delete(NewDir,1,2);
        {DOS strips off an initial .\}
        End
       Else
        NewDir:='';
     Until (DirList='') or (Length(NewDir) > 0);
     FSearch:=NewDir;
   End;
End;



Procedure FSplit(const Path:PathStr;Var Dir:DirStr;Var Name:NameStr;Var Ext:ExtStr);
Var
  DotPos,SlashPos,i : longint;
Begin
  SlashPos:=0;
  DotPos:=256;
  i:=Length(Path);
  While (i>0) and (SlashPos=0) Do
   Begin
     If (DotPos=256) and (Path[i]='.') Then
      DotPos:=i;
     If (Path[i]='/') Then
      SlashPos:=i;
     Dec(i);
   End;
  Ext:=Copy(Path,DotPos,255);
  Dir:=Copy(Path,1,SlashPos);
  Name:=Copy(Path,SlashPos + 1,DotPos - SlashPos - 1);
End;



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



Function FNMatch(const Pattern,Name:string):Boolean;
Var
  LenPat,LenName : longint;

  Function DoFNMatch(i,j:longint):Boolean;
  Var
    Found : boolean;
  Begin
  Found:=true;
  While Found and (i<=LenPat) Do
   Begin
     Case Pattern[i] of
      '?' : Found:=(j<=LenName);
      '*' : Begin
            {find the next character in pattern, different of ? and *}
              while Found and (i<LenPat) do
                begin
                inc(i);
                case Pattern[i] of
                  '*' : ;
                  '?' : begin
                          inc(j);
                          Found:=(j<=LenName);
                        end;
                else
                  Found:=false;
                end;
               end;
            {Now, find in name the character which i points to, if the * or ?
             wasn't the last character in the pattern, else, use up all the
             chars in name}
              Found:=true;
              if (i<=LenPat) then
                begin
                repeat
                {find a letter (not only first !) which maches pattern[i]}
                while (j<=LenName) and (name[j]<>pattern[i]) do
                  inc (j);
                 if (j<LenName) then
                  begin
                    if DoFnMatch(i+1,j+1) then
                     begin
                       i:=LenPat;
                       j:=LenName;{we can stop}
                       Found:=true;
                     end
                    else
                     inc(j);{We didn't find one, need to look further}
                  end;
               until (j>=LenName);
                end
              else
                j:=LenName;{we can stop}
            end;
     else {not a wildcard character in pattern}
       Found:=(j<=LenName) and (pattern[i]=name[j]);
     end;
     inc(i);
     inc(j);
   end;
  DoFnMatch:=Found and (j>LenName);
  end;

Begin {start FNMatch}
  LenPat:=Length(Pattern);
  LenName:=Length(Name);
  FNMatch:=DoFNMatch(1,1);
End;


function PExists(path:string):boolean;
begin
  PExists:=FExists(path);
end;

function FExists(path:string):boolean;
var
    info:stat;
begin
  FExists:=Fstat(path,info);
end;

function sys_load_image(a:cardinal; argp:ppchar; envp:ppchar):longint; cdecl; external name 'sys_load_image';
function sys_wait_for_thread (th:longint; var exitcode:longint):longint; cdecl; external name 'sys_wait_for_thread';

Function Shell(const Command:String):Longint;
var s:string;
    argv:ppchar;
    argc:longint;
    th:longint;
begin
  s:=Command;
  argv:=StringToPPChar(s,argc);
  th:=0;
{  writeln ('argc = ',argc);
  while argv[th]<>Nil do begin
    writeln ('argv[',th,'] = ',argv[th]);
    th:=th+1;
  end;
}
  th:=sys_load_image(argc,argv,system.envp);
  if th<0 then begin
    shell:=0;
    exit;
  end;
  sys_wait_for_thread(th,Shell);
end;



end.
