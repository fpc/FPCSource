{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ These things are set in the makefile, }
{ But you can override them here.}

{ If you want to link to the C library, set the conditional crtlib }
{ $define crtlib}

{ If you use an aout system, set the conditional AOUT}
{ $Define AOUT}

Unit SysLinux;
Interface

{$ifdef m68k}
{ used for single computations }
const
  BIAS4 = $7f-1;
{$endif}

{$I systemh.inc}
{$I heaph.inc}

const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

var
  argc : longint;
  argv : ppchar;
  envp : ppchar;

Implementation

{$I system.inc}

{$ifdef crtlib}
  Procedure _rtl_exit(l: longint); cdecl;
  Function  _rtl_paramcount: longint; cdecl;
  Procedure _rtl_paramstr(st: pchar; l: longint); cdecl;
  Function  _rtl_open(f: pchar; flags: longint): longint; cdecl;
  Procedure _rtl_close(h: longint); cdecl;
  Procedure _rtl_write(h: longint; addr: longInt; len : longint); cdecl;
  Procedure _rtl_erase(p: pchar); cdecl;
  Procedure _rtl_rename(p1: pchar; p2 : pchar); cdecl;
  Function  _rtl_read(h: longInt; addr: longInt; len : longint) : longint; cdecl;
  Function  _rtl_filepos(Handle: longint): longint; cdecl;
  Procedure _rtl_seek(Handle: longint; pos:longint); cdecl;
  Function  _rtl_filesize(Handle:longint): longInt; cdecl;
  Procedure _rtl_rmdir(buffer: pchar); cdecl;
  Procedure _rtl_mkdir(buffer: pchar); cdecl;
  Procedure _rtl_chdir(buffer: pchar); cdecl;
{$else}
  { used in syscall to report errors.}
  var
    Errno : longint;

  { Include constant and type definitions }
  {$i errno.inc    }  { Error numbers                 }
  {$i sysnr.inc    }  { System call numbers           }
  {$i sysconst.inc }  { Miscellaneous constants       }
  {$i systypes.inc }  { Types needed for system calls }

  { Read actual system call definitions. }
  {$i syscalls.inc }
{$endif}

{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

procedure prthaltproc;external name '_haltproc';

procedure System_exit;
begin
{$ifdef i386}
  asm
        jmp     prthaltproc
  end;
{$else}
  asm
        jmp     prthaltproc
  end;
{$endif}
End;


Function ParamCount: Longint;
Begin
  Paramcount:=argc-1
End;


Function ParamStr(l: Longint): String;
var
  link,
  hs : string;
  i : longint;
begin
  if l=0 then
   begin
     str(sys_getpid,hs);
     hs:='/proc/'+hs+'/exe'#0;
     i:=Sys_readlink(@hs[1],@link[1],high(link));
     if i>0 then
      begin
        link[0]:=chr(i);
        paramstr:=link;
      end
     else
      paramstr:=strpas(argv[0]);
   end
  else
   if (l>0) and (l<argc) then
    paramstr:=strpas(argv[l])
  else
    paramstr:='';
end;


Procedure Randomize;
Begin
  randseed:=sys_time;
End;


{*****************************************************************************
                              Heap Management
*****************************************************************************}

var
  _HEAP : longint;external name 'HEAP';
  _HEAPSIZE : longint;external name 'HEAPSIZE';

function getheapstart:pointer;assembler;
{$ifdef i386}
asm
        leal    _HEAP,%eax
end ['EAX'];
{$else}
asm
        lea.l   _HEAP,a0
        move.l  a0,d0
end;
{$endif}


function getheapsize:longint;assembler;
{$ifdef i386}
asm
        movl    _HEAPSIZE,%eax
end ['EAX'];
{$else}
asm
       move.l   _HEAPSIZE,d0
end ['D0'];
{$endif}


Function sbrk(size : longint) : Longint;
type
  tmmapargs=packed record
    address : longint;
    size    : longint;
    prot    : longint;
    flags   : longint;
    fd      : longint;
    offset  : longint;
  end;
var
  t     : syscallregs;
  mmapargs : tmmapargs;
begin
  mmapargs.address:=0;
  mmapargs.size:=Size;
  mmapargs.prot:=3;
  mmapargs.flags:=$22;
  mmapargs.fd:=-1;
  mmapargs.offset:=0;
  t.reg2:=longint(@mmapargs);
  Sbrk:=syscall(syscall_nr_mmap,t);
  if ErrNo<>0 then
   Sbrk:=0;
end;


{ include standard heap management }
{$I heap.inc}


{*****************************************************************************
                          Low Level File Routines
*****************************************************************************}

{
  The lowlevel file functions should take care of setting the InOutRes to the
  correct value if an error has occured, else leave it untouched
}

Procedure Errno2Inoutres;
{
  Convert ErrNo error to the correct Inoutres value
}

begin
  if ErrNo=0 then { Else it will go through all the cases }
   exit;
  case ErrNo of
   Sys_ENFILE,
   Sys_EMFILE : Inoutres:=4;
   Sys_ENOENT : Inoutres:=2;
    Sys_EBADF : Inoutres:=6;
   Sys_ENOMEM,
   Sys_EFAULT : Inoutres:=217;
   Sys_EINVAL : Inoutres:=218;
    Sys_EPIPE,
    Sys_EINTR,
      Sys_EIO,
   Sys_EAGAIN,
   Sys_ENOSPC : Inoutres:=101;
 Sys_ENAMETOOLONG,
    Sys_ELOOP,
  Sys_ENOTDIR : Inoutres:=3;
    Sys_EROFS,
   Sys_EEXIST,
   Sys_EACCES : Inoutres:=5;
  Sys_ETXTBSY : Inoutres:=162;
  end;
end;


Procedure Do_Close(Handle:Longint);
Begin
{$ifdef crtlib}
  _rtl_close(Handle);
{$else}
  sys_close(Handle);
{$endif}
End;


Procedure Do_Erase(p:pchar);
Begin
{$ifdef crtlib}
  _rtl_erase(p);
{$else}
  sys_unlink(p);
  Errno2Inoutres;
{$endif}
End;


Procedure Do_Rename(p1,p2:pchar);
Begin
{$ifdef crtlib}
  _rtl_rename(p1,p2);
{$else }
  sys_rename(p1,p2);
  Errno2Inoutres;
{$endif}
End;


Function Do_Write(Handle,Addr,Len:Longint):longint;
Begin
{$ifdef crtlib}
  _rtl_write(Handle,addr,len);
  Do_Write:=Len;
{$else}
  Do_Write:=sys_write(Handle,pchar(addr),len);
  Errno2Inoutres;
{$endif}
  if Do_Write<0 then
   Do_Write:=0;
End;


Function Do_Read(Handle,Addr,Len:Longint):Longint;
Begin
{$ifdef crtlib}
  Do_Read:=_rtl_read(Handle,addr,len);
{$else}
  Do_Read:=sys_read(Handle,pchar(addr),len);
  Errno2Inoutres;
{$endif}
  if Do_Read<0 then
   Do_Read:=0;
End;


Function Do_FilePos(Handle: Longint): Longint;
Begin
{$ifdef crtlib}
  Do_FilePos:=_rtl_filepos(Handle);
{$else}
  Do_FilePos:=sys_lseek(Handle, 0, Seek_Cur);
  Errno2Inoutres;
{$endif}
End;


Procedure Do_Seek(Handle,Pos:Longint);
Begin
{$ifdef crtlib}
  _rtl_seek(Handle, Pos);
{$else}
  sys_lseek(Handle, pos, Seek_set);
{$endif}
End;


Function Do_SeekEnd(Handle:Longint): Longint;
begin
{$ifdef crtlib}
  Do_SeekEnd:=_rtl_filesize(Handle);
{$else}
  Do_SeekEnd:=sys_lseek(Handle,0,Seek_End);
{$endif}
end;


Function Do_FileSize(Handle:Longint): Longint;
{$ifndef crtlib}
var
  regs : Syscallregs;
  Info : Stat;
{$endif}
Begin
{$ifdef crtlib}
  Do_FileSize:=_rtl_filesize(Handle);
{$else}
  regs.reg2:=Handle;
  regs.reg3:=longint(@Info);
  if SysCall(SysCall_nr_fstat,regs)=0 then
   Do_FileSize:=Info.Size
  else
   Do_FileSize:=0;
  Errno2Inoutres;
{$endif}
End;


Procedure Do_Truncate(Handle,Pos:longint);
{$ifndef crtlib}
var
  sr : syscallregs;
{$endif}
begin
{$ifndef crtlib}
  sr.reg2:=Handle;
  sr.reg3:=Pos;
  syscall(syscall_nr_ftruncate,sr);
  Errno2Inoutres;
{$endif}
end;


Procedure Do_Open(var f;p:pchar;flags:longint);
{
  FileRec and textrec have both Handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
var
{$ifndef crtlib}
  oflags : longint;
{$endif}
Begin
{ close first if opened }
  if ((flags and $10000)=0) then
   begin
     case FileRec(f).mode of
      fminput,fmoutput,fminout : Do_Close(FileRec(f).Handle);
      fmclosed : ;
     else
      begin
        inoutres:=102; {not assigned}
        exit;
      end;
     end;
   end;
{ reset file Handle }
  FileRec(f).Handle:=UnusedHandle;
{ We do the conversion of filemodes here, concentrated on 1 place }
  case (flags and 3) of
   0 : begin
         oflags :=Open_RDONLY;
         FileRec(f).mode:=fminput;
       end;
   1 : begin
         oflags :=Open_WRONLY;
         FileRec(f).mode:=fmoutput;
       end;
   2 : begin
         oflags :=Open_RDWR;
         FileRec(f).mode:=fminout;
       end;
  end;
  if (flags and $1000)=$1000 then
   oflags:=oflags or (Open_CREAT or Open_TRUNC)
  else
   if (flags and $100)=$100 then
    oflags:=oflags or (Open_APPEND);
{ empty name is special }
  if p[0]=#0 then
   begin
     case FileRec(f).mode of
       fminput :
         FileRec(f).Handle:=StdInputHandle;
       fminout, { this is set by rewrite }
       fmoutput :
         FileRec(f).Handle:=StdOutputHandle;
       fmappend :
         begin
           FileRec(f).Handle:=StdOutputHandle;
           FileRec(f).mode:=fmoutput; {fool fmappend}
         end;
     end;
     exit;
   end;
{ real open call }
{$ifdef crtlib}
  FileRec(f).Handle:=_rtl_open(p, oflags);
  if FileRec(f).Handle<0 then
   InOutRes:=2
  else
   InOutRes:=0;
{$else}
  FileRec(f).Handle:=sys_open(p,oflags,438);
  if (ErrNo=Sys_EROFS) and ((OFlags and Open_RDWR)<>0) then
   begin
     Oflags:=Oflags and not(Open_RDWR);
     FileRec(f).Handle:=sys_open(p,oflags,438);
   end;
  Errno2Inoutres;
{$endif}
End;


Function Do_IsDevice(Handle:Longint):boolean;
{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}
var
  sr: SysCallRegs;
  Data : array[0..255] of byte; {Large enough for termios info}
begin
  sr.reg2:=Handle;
  sr.reg3:=$5401; {=TCGETS}
  sr.reg4:=Longint(@Data);
  Do_IsDevice:=(SysCall(Syscall_nr_ioctl,sr)=0);
end;


{*****************************************************************************
                           UnTyped File Handling
*****************************************************************************}

{$i file.inc}

{*****************************************************************************
                           Typed File Handling
*****************************************************************************}

{$i typefile.inc}

{*****************************************************************************
                           Text File Handling
*****************************************************************************}

{$DEFINE SHORT_LINEBREAK}
{$DEFINE EXTENDED_EOF}

{$i text.inc}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}

Procedure MkDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  If InOutRes <> 0 then exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
{$ifdef crtlib}
  _rtl_mkdir(@buffer);
{$else}
  sys_mkdir(@buffer, 511);
  Errno2Inoutres;
{$endif}
End;


Procedure RmDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  If InOutRes <> 0 then exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
{$ifdef crtlib}
  _rtl_rmdir(@buffer);
{$else}
  sys_rmdir(@buffer);
  Errno2Inoutres;
{$endif}
End;


Procedure ChDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  If InOutRes <> 0 then exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
{$ifdef crtlib}
  _rtl_chdir(@buffer);
{$else}
  sys_chdir(@buffer);
  Errno2Inoutres;
{$endif}
End;


procedure getdir(drivenr : byte;var dir : shortstring);
{$ifndef crtlib}
var
  thisdir      : stat;
  rootino,
  thisino,
  dotdotino    : longint;
  rootdev,
  thisdev,
  dotdotdev    : word;
  thedir,dummy : string[255];
  dirstream    : pdir;
  d            : pdirent;
  mountpoint,validdir : boolean;
  predot       : string[255];
{$endif}
begin
  drivenr:=0;
  dir:='';
{$ifndef crtlib}
  thedir:='/'#0;
  if sys_stat(@thedir[1],thisdir)<0 then
   exit;
  rootino:=thisdir.ino;
  rootdev:=thisdir.dev;
  thedir:='.'#0;
  if sys_stat(@thedir[1],thisdir)<0 then
   exit;
  thisino:=thisdir.ino;
  thisdev:=thisdir.dev;
  { Now we can uniquely identify the current and root dir }
  thedir:='';
  predot:='';
  while not ((thisino=rootino) and (thisdev=rootdev)) do
   begin
   { Are we on a mount point ? }
     dummy:=predot+'..'#0;
     if sys_stat(@dummy[1],thisdir)<0 then
      exit;
     dotdotino:=thisdir.ino;
     dotdotdev:=thisdir.dev;
     mountpoint:=(thisdev<>dotdotdev);
   { Now, Try to find the name of this dir in the previous one }
     dirstream:=opendir (@dummy[1]);
     if dirstream=nil then
      exit;
     repeat
       d:=sys_readdir (dirstream);
       validdir:=false;
       if (d<>nil) and
          (not ((d^.name[0]='.') and ((d^.name[1]=#0) or ((d^.name[1]='.') and (
d^.name[2]=#0))))) and
          (mountpoint or (d^.ino=thisino)) then
        begin
          dummy:=predot+'../'+strpas(@(d^.name[0]))+#0;
          validdir:=not (sys_stat (@(dummy[1]),thisdir)<0);
        end
       else
        validdir:=false;
     until (d=nil) or
           ((validdir) and (thisdir.dev=thisdev) and (thisdir.ino=thisino) );
     if (closedir(dirstream)<0) or (d=nil) then
      exit;
   { At this point, d.name contains the name of the current dir}
     thedir:='/'+strpas(@(d^.name[0]))+thedir;
     thisdev:=dotdotdev;
     thisino:=dotdotino;
     predot:=predot+'../';
   end;
{ Now rootino=thisino and rootdev=thisdev so we've reached / }
  dir:=thedir
{$endif}
end;


{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

{$ifndef newSignal}
Procedure SignalToRunError(Sig:longint);
begin
  case sig of
    8 : HandleError(200);
   11 : HandleError(216);
  end;
end;

Procedure InstallSignals;
var
  sr : syscallregs;
begin
  sr.reg3:=longint(@SignalToRunError);
  { sigsegv }
  sr.reg2:=11;
  syscall(syscall_nr_signal,sr);
  { sigfpe }
  sr.reg2:=8;
  syscall(syscall_nr_signal,sr);
end;
{$else newSignal}

{$i i386/signal.inc}

procedure SignalToRunerror(Sig: longint); cdecl;
begin
  case sig of
    8 : HandleError(200);
   11 : HandleError(216);
  end;
end;

Procedure InstallSignals;
const
  act: SigActionRec = (handler:(Sh:@SignalToRunError);sa_mask:0;sa_flags:$40000000 or $10000000;
                       Sa_restorer: NIL);
  oldact: PSigActionRec = Nil;
begin
  SigAction(8,@act,oldact);
  SigAction(11,@act,oldact);
end;
{$endif newSignal}



procedure SetupCmdLine;
var
  bufsize,
  len,j,
  size,i : longint;
  found  : boolean;
  buf    : array[0..1026] of char;

  procedure AddBuf;
  begin
    reallocmem(cmdline,size+bufsize);
    move(buf,cmdline[size],bufsize);
    inc(size,bufsize);
    bufsize:=0;
  end;

begin
  size:=0;
  bufsize:=0;
  i:=0;
  while (i<argc) do
   begin
     len:=strlen(argv[i]);
     if len>sizeof(buf)-2 then
      len:=sizeof(buf)-2;
     found:=false;
     for j:=1 to len do
      if argv[i][j]=' ' then
       begin
         found:=true;
         break;
       end;
     if bufsize+len>=sizeof(buf)-2 then
      AddBuf;
     if found then
      begin
        buf[bufsize]:='"';
        inc(bufsize);
      end;
     move(argv[i]^,buf[bufsize],len);
     inc(bufsize,len);
     if found then
      begin
        buf[bufsize]:='"';
        inc(bufsize);
      end;
     if i<argc then
      buf[bufsize]:=' '
     else
      buf[bufsize]:=#0;
     inc(bufsize);
     inc(i);
   end;
  AddBuf;
end;


Begin
{ Set up signals handlers }
  InstallSignals;
{ Setup heap }
  InitHeap;
  InitExceptions;
{ Arguments }
  SetupCmdLine;
{ Setup stdin, stdout and stderr }
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{ Reset IO Error }
  InOutRes:=0;
End.

{
  $Log$
  Revision 1.40  2000-03-31 13:24:28  jonas
    * signal handling using sigaction when compiled with -dnewsignal
      (allows multiple signals to be received in one run)

  Revision 1.39  2000/03/25 12:28:37  peter
    * patch for getdir from Pierre

  Revision 1.38  2000/03/23 15:24:18  peter
    * remove handle check for do_close

  Revision 1.37  2000/02/09 16:59:32  peter
    * truncated log

  Revision 1.36  2000/02/09 12:17:51  peter
    * moved halt to system.inc
    * syslinux doesn't use direct asm anymore

  Revision 1.35  2000/02/08 11:47:09  peter
    * paramstr(0) support

  Revision 1.34  2000/01/20 23:38:02  peter
    * support fm_inout as stdoutput for assign(f,'');rewrite(f,1); becuase
      rewrite opens always with filemode 2

  Revision 1.33  2000/01/16 22:25:38  peter
    * check handle for file closing

  Revision 1.32  2000/01/07 16:41:41  daniel
    * copyright 2000

  Revision 1.31  2000/01/07 16:32:28  daniel
    * copyright 2000 added

  Revision 1.30  1999/12/01 22:57:31  peter
    * cmdline support

  Revision 1.29  1999/11/06 14:39:12  peter
    * truncated log

  Revision 1.28  1999/10/28 09:50:06  peter
    * use mmap instead of brk

  Revision 1.27  1999/09/10 15:40:35  peter
    * fixed do_open flags to be > $100, becuase filemode can be upto 255

  Revision 1.26  1999/09/08 16:14:43  peter
    * pointer fixes

  Revision 1.25  1999/07/28 23:18:36  peter
    * closedir fixes, which now disposes the pdir itself

}
