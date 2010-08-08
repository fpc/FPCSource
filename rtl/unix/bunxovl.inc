{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Marco van de Voort

    Some generic overloads for stringfunctions in the baseunix unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$I textrec.inc}
{$I filerec.inc}

Function  FpLink (existing : AnsiString; newone : AnsiString): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpLink:=FpLink(pchar(existing),pchar(newone));
End;

Function  FpMkfifo (path : AnsiString; Mode : TMode): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpMkfifo:=FpMkfifo(pchar(path),mode);
End;

Function  FpChmod (path : AnsiString; Mode : TMode): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpChmod:=FpChmod(pchar(path),mode);
End;

Function  FpChown (path : AnsiString; owner : TUid; group : TGid): cInt;{$ifdef VER2_0}inline;{$endif}
Begin
  FpChown:=FpChown(pchar(path),owner,group);
End;

Function  FpUtime (path : AnsiString; times : putimbuf): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpUtime:=FpUtime(pchar(path),times);
End;

{
Function  FpGetcwd (path:AnsiString; siz:TSize):AnsiString; {$ifdef VER2_0}inline;{$endif}
Begin
  FpGetcwd:=ansistring(pchar(FpGetcwd(pchar(path),siz)));
End;
}
Function  FpGetcwd :AnsiString;

Var
  Buf : Array[0..PATH_MAX+1]  of char;
Begin
  Buf[PATH_MAX+1]:=#0;
  If FpGetcwd(@Buf[0],PATH_MAX)=Nil then
    FpGetcwd:=''
  else
    FpGetcwd:=Buf;
End;

Function  FpExecve (path : AnsiString; argv : ppchar; envp: ppchar): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpExecve:=FpExecve (pchar(path),argv,envp);
End;

Function  FpExecv (path : AnsiString; argv : ppchar): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpExecv:=FpExecve (pchar(path),argv,envp);
End;


Function  FpChdir (path : AnsiString): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
 FpChDir:=FpChdir(pchar(Path));
End;

Function  FpOpen (path : AnsiString; flags : cInt; Mode: TMode):cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpOpen:=FpOpen(pchar(Path),flags,mode);
End;


Function  FpMkdir (path : AnsiString; Mode: TMode):cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpMkdir:=FpMkdir(pchar(Path),mode);
End;

Function  FpUnlink (path : AnsiString): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpUnlink:=FpUnlink(pchar(path));
End;

Function  FpRmdir (path : AnsiString): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpRmdir:=FpRmdir(pchar(path));
End;

Function  FpRename (old  : AnsiString;newpath: AnsiString): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpRename:=FpRename(pchar(old),pchar(newpath));
End;

Function  FpStat (path: AnsiString; var buf : stat): cInt; {$ifdef VER2_0}inline;{$endif}
begin
  FpStat:=FpStat(pchar(path),buf);
End;

Function  fpLstat   (path: Ansistring; Info: pstat):cint; inline;
begin
  fplstat:=fplstat(pchar(path), info);
end;

Function  fpLstat   (path:pchar;var Info:stat):cint; inline;

begin
  fpLstat:=fplstat(path,@info);
end;

Function  fpLstat   (Filename: ansistring;var Info:stat):cint; inline;

begin
  fpLstat:=fplstat(filename,@info);
end;

Function FpAccess (pathname : AnsiString; aMode : cInt): cInt; {$ifdef VER2_0}inline;{$endif}
Begin
  FpAccess:=FpAccess(pchar(pathname),amode);
End;

Function  FPFStat(var F:Text;Var Info:stat):Boolean; {$ifdef VER2_0}inline;{$endif}
{
  Get all information on a text file, and return it in info.
}
begin
  FPFStat:=FPFstat(TextRec(F).Handle,INfo)=0;
end;

Function  FPFStat(var F:File;Var Info:stat):Boolean; {$ifdef VER2_0}inline;{$endif}
{
  Get all information on a untyped file, and return it in info.
}
begin
  FPFStat:=FPFstat(FileRec(F).Handle,Info)=0;
end;

Function FpSignal(signum:longint;Handler:signalhandler):signalhandler;
// should be moved out of generic files. Too specific.

var sa,osa : sigactionrec;

begin
     sa.sa_handler:=SigActionHandler(handler);
     FillChar(sa.sa_mask,sizeof(sa.sa_mask),#0);
     sa.sa_flags := 0;
{     if (sigintr and signum) =0 then
 {restart behaviour needs libc}
      sa.sa_flags :=sa.sa_flags or SA_RESTART;
}
     FPSigaction(signum,@sa,@osa);
     if fpgetErrNo<>0 then
       fpsignal:=NIL
     else
       fpsignal:=signalhandler(osa.sa_handler);
end;

{$ifdef FPC_USE_LIBC} // can't remember why this is the case. Might be legacy.
function xFpread(fd: cint; buf: pchar; nbytes : size_t): ssize_t; cdecl; external clib name 'read';
{$else}
function xFpread(fd: cint; buf: pchar; nbytes : size_t): ssize_t; external name 'FPC_SYSC_READ';
{$endif}

Function  FpRead           (fd : cInt;var buf; nbytes : TSize): TSsize; {$ifdef VER2_0}inline;{$endif}

begin
  FPRead:=xFpRead(fd,pchar(@buf),nbytes);
end;

Function  FpWrite          (fd : cInt;const buf; nbytes : TSize): TSsize; {$ifdef VER2_0}inline;{$endif}
begin
 FpWrite:=FpWrite(fd,pchar(@buf),nbytes);
end;

{$ifdef linux}
function  FppRead           (fd : cInt;var buf; nbytes : TSize; offset:Toff): TSsize; {$ifdef VER2_0}inline;{$endif}

begin
  FppRead:=FppRead(fd,pchar(@buf),nbytes,offset);
end;

function  FppWrite          (fd : cInt;const buf; nbytes : TSize; offset:Toff): TSsize; {$ifdef VER2_0}inline;{$endif}

begin
  FppWrite:=FppWrite(fd,pchar(@buf),nbytes,offset);
end;
{$endif}

Function  FpOpen    (path : pChar; flags : cInt):cInt; {$ifdef VER2_0}inline;{$endif}

begin
 FpOpen:=FpOpen(path,flags,438);
end;

Function  FpOpen    (path : AnsiString; flags : cInt):cInt; {$ifdef VER2_0}inline;{$endif}

begin
 FpOpen:=FpOpen(pchar(path),flags,438);
end;

Function  FpOpen    (path : String; flags : cInt):cInt;

begin
 path:=path+#0;
 FpOpen:=FpOpen(@path[1],flags,438);
end;

Function  FpOpen    (path : String; flags : cInt; Mode: TMode):cInt;

begin
 path:=path+#0;
 FpOpen:=FpOpen(@path[1],flags,Mode);
end;

Function  FpOpendir (dirname : AnsiString): pDir; {$ifdef VER2_0}inline;{$endif}
Begin
  FpOpenDir:=FpOpenDir(pchar(dirname));
End;


Function  FpOpendir (dirname : shortString): pDir; {$ifdef VER2_0}inline;{$endif}
Begin
  dirname:=dirname+#0;
  FpOpenDir:=FpOpenDir(pchar(@dirname[1]));
End;


Function  FpStat (path: String; var buf : stat): cInt;

begin
 path:=path+#0;
 FpStat:=FpStat(pchar(@path[1]),buf);
end;

Function fpDup(var oldfile,newfile:text):cint;
{
  Copies the filedescriptor oldfile to newfile, after flushing the buffer of
  oldfile.
  After which the two textfiles are, in effect, the same, except
  that they don't share the same buffer, and don't share the same
  close_on_exit flag.
}
begin
  flush(oldfile);{ We cannot share buffers, so we flush them. }
  textrec(newfile):=textrec(oldfile);
  textrec(newfile).bufptr:=@(textrec(newfile).buffer);{ No shared buffer. }
  textrec(newfile).handle:=fpDup(textrec(oldfile).handle);
  fpdup:=textrec(newfile).handle;
end;

Function fpDup(var oldfile,newfile:file):cint;
{
  Copies the filedescriptor oldfile to newfile
}
begin
  filerec(newfile):=filerec(oldfile);
  filerec(newfile).handle:=fpDup(filerec(oldfile).handle);
  fpdup:=  filerec(newfile).handle;
end;


Function FpDup2(var oldfile,newfile:text):cint;
{
  Copies the filedescriptor oldfile to newfile, after flushing the buffer of
  oldfile. It closes newfile if it was still open.
  After which the two textfiles are, in effect, the same, except
  that they don't share the same buffer, and don't share the same
  close_on_exit flag.
}
var
  tmphandle : word;
begin
  case TextRec(oldfile).mode of
    fmOutput, fmInOut, fmAppend :
      flush(oldfile);{ We cannot share buffers, so we flush them. }
  end;
  case TextRec(newfile).mode of
    fmOutput, fmInOut, fmAppend :
      flush(newfile);
  end;
  tmphandle:=textrec(newfile).handle;
  textrec(newfile):=textrec(oldfile);
  textrec(newfile).handle:=tmphandle;
  textrec(newfile).bufptr:=@(textrec(newfile).buffer);{ No shared buffer. }
  fpDup2:=fpDup2(textrec(oldfile).handle,textrec(newfile).handle);
end;

Function FpDup2(var oldfile,newfile:file):cint;
{
  Copies the filedescriptor oldfile to newfile
}
var
  tmphandle : word;
begin
  tmphandle := filerec(newfile).handle;
  filerec(newfile):=filerec(oldfile);
  filerec(newfile).handle := tmphandle;
  fpDup2:=fpDup2(filerec(oldfile).handle,filerec(newfile).handle);
end;

function  fptime    :time_t;  {$ifdef VER2_0}inline;{$endif}
var t:time_t;
begin
  fptime:=fptime(t);
end;

Function fpSelect(N:cint;readfds,writefds,exceptfds:pfdset;TimeOut:cint):cint;
{
  Select checks whether the file descriptor sets in readfs/writefs/exceptfs
  have changed.
  This function allows specification of a timeout as a longint.
}
var
  p  : PTimeVal;
  tv : TimeVal;
begin
  if TimeOut=-1 then
   p:=nil
  else
   begin
     tv.tv_Sec:=Timeout div 1000;
     tv.tv_Usec:=(Timeout mod 1000)*1000;
     p:=@tv;
   end;
  fpSelect:=fpSelect(N,Readfds,WriteFds,ExceptFds,p);
end;

Function fpSelect(var T:Text;TimeOut :PTimeval):cint;
Var
  F:TfdSet;
begin
  if textrec(t).mode=fmclosed then
   begin
     fpSetErrNo(ESysEBADF);
     exit(-1);
   end;
  FpFD_ZERO(f);
  fpFD_SET(textrec(T).handle,f);
  if textrec(T).mode=fminput then
   fpselect:=fpselect(textrec(T).handle+1,@f,nil,nil,TimeOut)
  else
   fpSelect:=fpselect(textrec(T).handle+1,nil,@f,nil,TimeOut);
end;

Function fpSelect(var T:Text;TimeOut :time_t):cint;
var
  p  : PTimeVal;
  tv : TimeVal;
begin
  if TimeOut=-1 then
   p:=nil
  else
   begin
     tv.tv_Sec:=Timeout div 1000;
     tv.tv_Usec:=(Timeout mod 1000)*1000;
     p:=@tv;
   end;
  fpSelect:=fpSelect(T,p);
end;

function  FpWaitPid (pid : TPid; Var Status : cInt; Options : cint) : TPid;

begin
  fpWaitPID:=fpWaitPID(Pid,@Status,Options);
end;

Function fpReadLink(Name:ansistring):ansistring;
{
  Read a link (where it points to)
}
var
  LinkName : ansistring;
  i : cint;
begin
  SetLength(linkname,PATH_MAX);
  i:=fpReadLink(pchar(name),pchar(linkname),PATH_MAX);
  if i>0 then
   begin
     SetLength(linkname,i);
     fpReadLink:=LinkName;
   end
  else
    fpReadLink:='';
end;


