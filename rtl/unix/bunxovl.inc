{
    $Id$
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

Function  FpLink (existing : AnsiString; newone : AnsiString): cInt;
Begin
  FpLink:=FpLink(pchar(existing),pchar(newone));
End;

Function  FpMkfifo (path : AnsiString; Mode : TMode): cInt;
Begin
  FpMkfifo:=FpMkfifo(pchar(path),mode);
End;

Function  FpChmod (path : AnsiString; Mode : TMode): cInt;
Begin
  FpChmod:=FpChmod(pchar(path),mode);
End;

Function  FpChown (path : AnsiString; owner : TUid; group : TGid): cInt;
Begin
  FpChown:=FpChown(pchar(path),owner,group);
End;

Function  FpUtime (path : AnsiString; times : putimbuf): cInt;
Begin
  FpUtime:=FpUtime(pchar(path),times);
End;

Function  FpGetcwd (path:AnsiString; siz:TSize):AnsiString;
Begin
  FpGetcwd:=FpGetcwd(pchar(path),siz);
End;

Function  FpExecve (path : AnsiString; argv : ppchar; envp: ppchar): cInt;
Begin
  FpExecve:=FpExecve (pchar(path),argv,envp);
End;

Function  FpExecv (path : AnsiString; argv : ppchar): cInt;
Begin
  FpExecv:=FpExecv (pchar(path),argv);
End;

Function  FpOpendir (dirname : AnsiString): pDir;
Begin
  FpOpenDir:=FpOpenDir(dirname);
End;

Function  FpChdir (path : AnsiString): cInt;
Begin
 FpChDir:=FpChdir(pchar(Path));
End;		   

Function  FpOpen (path : AnsiString; flags : cInt; Mode: TMode):cInt;
Begin
  FpOpen:=FpOpen(pchar(Path),flags,mode);
End;

Function  FpMkdir (path : AnsiString; Mode: TMode):cInt;
Begin
  FpMkdir:=FpMkdir(pchar(Path),mode);
End;

Function  FpUnlink (path : AnsiString): cInt;
Begin
  FpUnlink:=FpUnlink(pchar(path));
End;

Function  FpRmdir (path : AnsiString): cInt;
Begin
  FpRmdir:=FpRmdir(pchar(path));
End;

Function  FpRename (old  : AnsiString;newpath: AnsiString): cInt;
Begin
  FpRename:=FpRename(pchar(old),pchar(newpath));
End;

Function  FpStat (path: AnsiString; var buf : stat): cInt;
begin
  FpStat:=FpStat(pchar(path),buf);
End;

Function FpAccess (pathname : AnsiString; aMode : cInt): cInt; 
Begin
  FpAccess:=FpAccess(pchar(pathname),amode);
End;

Function  FPFStat(var F:Text;Var Info:stat):Boolean;
{
  Get all information on a text file, and return it in info.
}
begin
  FPFStat:=FPFstat(TextRec(F).Handle,INfo)>0;
end;

Function  FPFStat(var F:File;Var Info:stat):Boolean;
{
  Get all information on a untyped file, and return it in info.
}
begin
  FPFStat:=FPFstat(FileRec(F).Handle,Info)>0;
end;

Function FpSignal(signum:longint;Handler:signalhandler):signalhandler;

var sa,osa : sigactionrec;

begin
     sa.sa_handler:=handler;
     FillChar(sa.sa_mask,sizeof(sigset),#0);
     sa.sa_flags := 0;
{     if (sigintr and signum) =0 then
 {restart behaviour needs libc}
      sa.sa_flags :=sa.sa_flags or SA_RESTART;
}
     FPSigaction(signum,@sa,@osa);
     if getErrNo<>0 then
       fpsignal:=NIL
     else
       fpsignal:=osa.sa_handler;
end;



{
 $Log$
 Revision 1.2  2003-06-01 16:28:41  marco
  * Enhancements to make the compiler baseunix using.

 Revision 1.1  2002/12/18 16:49:02  marco
  * New RTL. Linux system unit and baseunix operational.


}

