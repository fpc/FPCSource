{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    This is the core of the system unit *nix systems (now FreeBSD
     and Unix).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ These things are set in the makefile, }
{ But you can override them here.}

{ If you use an aout system, set the conditional AOUT}
{ $Define AOUT}

{$I system.inc}

{ used in syscall to report errors.}
var
  Errno : longint;

{ Include constant and type definitions }
{$i errno.inc    }  { Error numbers                 }
{$i sysnr.inc    }  { System call numbers           }
{$i sysconst.inc }  { Miscellaneous constants       }
{$i systypes.inc }  { Types needed for system calls }

{ Read actual system call definitions. }
{$i signal.inc}
{$i syscalls.inc }


{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

{$ifdef I386}
{ this should be defined in i386 directory !! PM }
const
  fpucw : word = $1332;
  FPU_Invalid = 1;
  FPU_Denormal = 2;
  FPU_DivisionByZero = 4;
  FPU_Overflow = 8;
  FPU_Underflow = $10;
  FPU_StackUnderflow = $20;
  FPU_StackOverflow = $40;

{$endif I386}

Procedure ResetFPU;
begin
{$ifdef I386}
  asm
    fninit
    fldcw   fpucw
  end;
{$endif I386}
end;


procedure prthaltproc;external name '_haltproc';

Procedure System_exit;
Begin
  prthaltproc;
End;


Function ParamCount: Longint;
Begin
  Paramcount:=argc-1;
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
     {$ifdef FreeBSD}
      hs:='/proc/'+hs+'/file'#0;
     {$else}
      hs:='/proc/'+hs+'/exe'#0;
     {$endif}
     i:=Sys_readlink(@hs[1],@link[1],high(link));
     { it must also be an absolute filename, linux 2.0 points to a memory
       location so this will skip that }
     if (i>0) and (link[1]='/') then
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
  _HEAP : pointer;external name 'HEAP';
  _HEAPSIZE : longint;external name 'HEAPSIZE';

function getheapstart:pointer;assembler;
{$undef fpc_getheapstart_ok}
{$ifdef i386}
{$define fpc_getheapstart_ok}
asm
        leal    _HEAP,%eax
end ['EAX'];
{$endif i386}
{$ifdef m68k}
{$define fpc_getheapstart_ok}
asm
        lea.l   _HEAP,a0
        move.l  a0,d0
end['A0','D0'];
{$endif m68k}
{$ifndef fpc_getheapstart_ok}
asm
end;
{$error Getheapstart code is not implemented }
{$endif not fpc_getheapstart_ok}


function getheapsize:longint;assembler;
{$undef fpc_getheapsize_ok}
{$ifdef i386}
{$define fpc_getheapsize_ok}
asm
        movl    _HEAPSIZE,%eax
end ['EAX'];
{$endif i386}
{$ifdef m68k}
{$define fpc_getheapsize_ok}
asm
       move.l   _HEAPSIZE,d0
end ['D0'];
{$endif m68k}
{$ifndef fpc_getheapsize_ok}
asm
end;
{$error Getheapsize code is not implemented }
{$endif not fpc_getheapsize_ok}


Function sbrk(size : longint) : Longint;
begin
  sbrk:=Sys_mmap(0,Size,3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0);
  if sbrk<>-1 then
   errno:=0;
  {! It must be -1, not 0 as before, see heap.inc. Should be in sysmmap?}
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
  If errno<0 then Errno:=-errno;
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
   Sys_EISDIR,
   Sys_ENOTEMPTY,
   Sys_EACCES : Inoutres:=5;
  Sys_ETXTBSY : Inoutres:=162;
  else
    InOutRes := Integer(Errno);
  end;
end;


Procedure Do_Close(Handle:Longint);
Begin
  sys_close(Handle);
  {Errno2Inoutres;}
End;


Procedure Do_Erase(p:pchar);
{$ifdef BSD}
 var FileInfo : Stat;
{$endif}

Begin
  {$ifdef BSD} {or POSIX}
  { verify if the filename is actually a directory }
  { if so return error and do nothing, as defined  }
  { by POSIX                                       }
  if sys_stat(p,fileinfo)<0 then
   begin
     Errno2Inoutres;
     exit;
   end;
  {$ifdef BSD}
   if (fileinfo.mode and STAT_IFMT)=STAT_IFDIR then
  {$else}
   if s_ISDIR(fileinfo.st_mode) then
  {$endif}
   begin
     InOutRes := 2;
     exit;
   end;
  {$endif}
  sys_unlink(p);
  Errno2Inoutres;
  {$ifdef Linux}
  { tp compatible result }
  if (Errno=Sys_EISDIR) then
   InOutRes:=2;
  {$endif}
End;


Procedure Do_Rename(p1,p2:pchar);
Begin
  sys_rename(p1,p2);
  Errno2Inoutres;
End;

Function Do_Write(Handle,Addr,Len:Longint):longint;
Begin
  repeat
    Do_Write:=sys_write(Handle,pchar(addr),len);
  until ErrNo<>Sys_EINTR;
  Errno2Inoutres;
  if Do_Write<0 then
   Do_Write:=0;
End;


Function Do_Read(Handle,Addr,Len:Longint):Longint;
Begin
  repeat
    Do_Read:=sys_read(Handle,pchar(addr),len);
  until ErrNo<>Sys_EINTR;
  Errno2Inoutres;
  if Do_Read<0 then
   Do_Read:=0;
End;


Function Do_FilePos(Handle: Longint): Longint;
Begin
  Do_FilePos:=sys_lseek(Handle, 0, Seek_Cur);
  Errno2Inoutres;
End;


Procedure Do_Seek(Handle,Pos:Longint);
Begin
  sys_lseek(Handle, pos, Seek_set);
  errno2inoutres;
End;


Function Do_SeekEnd(Handle:Longint): Longint;
begin
  Do_SeekEnd:=sys_lseek(Handle,0,Seek_End);
  errno2inoutres;
end;

Function Do_FileSize(Handle:Longint): Longint;
var
  Info : Stat;
Begin
  if sys_fstat(handle,info)=0 then
   Do_FileSize:=Info.Size
  else
   Do_FileSize:=0;
  Errno2Inoutres;
End;


Procedure Do_Truncate(Handle,fPos:longint);
begin
  sys_ftruncate(handle,fpos);
  Errno2Inoutres;
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
  oflags : longint;

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
  FileRec(f).Handle:=sys_open(p,oflags,438);
  if (ErrNo=Sys_EROFS) and ((OFlags and Open_RDWR)<>0) then
   begin
     Oflags:=Oflags and not(Open_RDWR);
     FileRec(f).Handle:=sys_open(p,oflags,438);
   end;
  Errno2Inoutres;
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
  Data : array[0..255] of byte; {Large enough for termios info}
begin
  Do_IsDevice:=(sys_ioctl(handle,IOCTL_TCGETS,@data)<>-1);
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
  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  sys_mkdir(@buffer, 511);
  Errno2Inoutres;
End;


Procedure RmDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  if (s ='.') then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  sys_rmdir(@buffer);
  {$ifdef BSD}
    if (Errno=Sys_EINVAL) Then
     InOutRes:=5
    Else
   {$endif}
  Errno2Inoutres;
End;


Procedure ChDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  sys_chdir(@buffer);
  Errno2Inoutres;
  { file not exists is path not found under tp7 }
  if InOutRes=2 then
   InOutRes:=3;
End;


procedure GetDir (DriveNr: byte; var Dir: ShortString);
var
  thisdir      : stat;
  rootino,
  thisino,
  dotdotino    : longint;
  rootdev,
  thisdev,
  dotdotdev    : dev_t;
  thedir,dummy : string[255];
  dirstream    : pdir;
  d            : pdirent;
  mountpoint,validdir : boolean;
  predot       : string[255];
begin
  drivenr:=0;
  dir:='';
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
          (not ((d^.name[0]='.') and ((d^.name[1]=#0) or ((d^.name[1]='.')
                                 and (d^.name[2]=#0))))) and
                                 (mountpoint or (d^.ino=thisino)) then
        begin
          dummy:=predot+'../'+strpas(@(d^.name[0]))+#0;
          validdir:=not (sys_stat (@(dummy[1]),thisdir)<0);
        end
       else
        validdir:=false;
     until (d=nil) or
           ((validdir) and (thisdir.dev=thisdev) and (thisdir.ino=thisino) );
     { At this point, d.name contains the name of the current dir}
     if (d<>nil) then
      thedir:='/'+strpas(@(d^.name[0]))+thedir;
     { closedir also makes d invalid }
     if (closedir(dirstream)<0) or (d=nil) then
      exit;
     thisdev:=dotdotdev;
     thisino:=dotdotino;
     predot:=predot+'../';
   end;
{ Now rootino=thisino and rootdev=thisdev so we've reached / }
  dir:=thedir
end;

{$ifdef Unix}
{*****************************************************************************
                             Thread Handling
*****************************************************************************}

{ include threading stuff, this is os independend part }
{$I thread.inc}
{$endif Unix}

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

{$ifdef BSD}
 procedure SignalToRunerror(Sig: longint; SigContext: SigContextRec;someptr:pointer); cdecl;
{$else}
 {$ifdef Solaris}
  procedure SignalToRunerror(Sig: longint; SigContext: SigContextRec;someptr:pointer); cdecl;
 {$else}
  procedure SignalToRunerror(Sig: longint; SigContext: SigContextRec); cdecl;
 {$endif}
{$ENDIF}
var

  res,fpustate : word;
begin
  res:=0;
  case sig of
    SIGFPE :
      begin
    { this is not allways necessary but I don't know yet
      how to tell if it is or not PM }
{$ifdef I386}
          fpustate:=0;
          res:=200;
  {$ifndef FreeBSD}
           if assigned(SigContext.fpstate) then
             fpuState:=SigContext.fpstate^.sw;
  {$else}
            fpustate:=SigContext.en_sw;
    {$ifdef SYSTEM_DEBUG}
           writeln('xx:',sigcontext.en_tw,' ',sigcontext.en_cw);
    {$endif SYSTEM_DEBUG}
  {$endif}
  {$ifdef SYSTEM_DEBUG}
          Writeln(stderr,'FpuState = ',FpuState);
  {$endif SYSTEM_DEBUG}
          if (FpuState and $7f) <> 0 then
            begin
              { first check te more precise options }
              if (FpuState and FPU_DivisionByZero)<>0 then
                res:=200
              else if (FpuState and FPU_Overflow)<>0 then
                res:=205
              else if (FpuState and FPU_Underflow)<>0 then
                res:=206
              else if (FpuState and FPU_Denormal)<>0 then
                res:=216
              else if (FpuState and (FPU_StackOverflow or FPU_StackUnderflow))<>0 then
                res:=207
              else if (FpuState and FPU_Invalid)<>0 then
                res:=216
              else
                res:=207;  {'Coprocessor Error'}
            end;
{$endif I386}
          ResetFPU;
        end;
   SIGILL,
   SIGBUS,
   SIGSEGV :
        res:=216;
  end;
{ give runtime error at the position where the signal was raised }
  if res<>0 then
   begin
{$ifdef I386}
     {$ifdef FreeBSD}
      HandleErrorAddrFrame(res,SigContext.sc_eip,SigContext.sc_ebp);
     {$else}
      HandleErrorAddrFrame(res,SigContext.eip,SigContext.ebp);
     {$endif}
{$else}
     HandleError(res);
{$endif}
   end;
end;


Procedure InstallSignals;
const
{$Ifndef BSD}
 {$ifdef solaris}
  act: SigActionRec =(sa_flags:SA_SIGINFO;Handler:(sa:@signaltorunerror;sa_mask:0);
 {$else}
  act: SigActionRec = (handler:(Sa:@SignalToRunError);sa_mask:0;sa_flags:0;
                       Sa_restorer: NIL);
 {$endif}
{$ELSE}
   act: SigActionRec = (handler:(Sa:@SignalToRunError);sa_flags:SA_SIGINFO;
    sa_mask:0);
{$endif}

  oldact: PSigActionRec = Nil;          {Probably not necessary anymore, now
                                         VAR is removed}
begin
  ResetFPU;
  SigAction(SIGFPE,@act,oldact);
{$ifndef Solaris}
  SigAction(SIGSEGV,@act,oldact);
  SigAction(SIGBUS,@act,oldact);
  SigAction(SIGILL,@act,oldact);
{$endif}
end;


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
  IsConsole := TRUE;
  IsLibrary := FALSE;
  StackBottom := Sptr - StackLength;
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
  Revision 1.23  2002-07-28 20:43:49  florian
    * several fixes for linux/powerpc
    * several fixes to MT

  Revision 1.22  2002/05/31 13:37:24  marco
   * more Renamefest

  Revision 1.21  2002/04/21 15:55:00  carl
  + initialize some global variables

  Revision 1.20  2002/04/12 17:43:28  carl
  + generic stack checking

  Revision 1.19  2002/03/11 19:10:33  peter
    * Regenerated with updated fpcmake

  Revision 1.18  2001/10/14 13:33:21  peter
    * start of thread support for linux

  Revision 1.17  2001/09/30 21:10:20  peter
    * erase(directory) returns now 2 to be tp compatible

  Revision 1.16  2001/08/05 12:24:20  peter
    * m68k merges

  Revision 1.15  2001/07/16 19:51:36  marco
   * A small note, copied from the Solaris patch. Do_close needs errnotoiores?

  Revision 1.14  2001/07/15 11:57:16  peter
    * merged m68k updates

  Revision 1.13  2001/07/13 22:05:09  peter
    * cygwin updates

  Revision 1.12  2001/06/02 19:24:49  peter
    * chdir rte 2 mapped to 3

  Revision 1.11  2001/06/02 00:31:31  peter
    * merge unix updates from the 1.0 branch, mostly related to the
      solaris target

  Revision 1.10  2001/04/23 20:33:31  peter
    * also install sig handlers for sigill,sigbus

  Revision 1.9  2001/04/13 22:39:05  peter
    * removed warning

  Revision 1.8  2001/04/12 17:53:43  peter
    * fixed usage of already release memory in getdir

  Revision 1.7  2001/03/21 21:08:20  hajny
    * GetDir fixed

  Revision 1.6  2001/03/16 20:09:58  hajny
    * universal FExpand

  Revision 1.5  2001/02/20 21:31:12  peter
    * chdir,mkdir,rmdir with empty string fixed

  Revision 1.4  2000/12/17 14:00:57  peter
    * removed debug writelns

  Revision 1.3  2000/10/09 16:35:51  marco
   * Fixed the first (of many) ioctls that make building the IDE hard.

  Revision 1.2  2000/09/18 13:14:51  marco
   * Global Linux +bsd to (rtl/freebsd rtl/unix rtl/linux structure)

  Revision 1.6  2000/09/11 13:48:08  marco
   * FreeBSD support and removal of old sighandler

  Revision 1.5  2000/08/13 08:43:45  peter
    * don't check for directory in do_open (merged)

  Revision 1.4  2000/08/05 18:33:51  peter
    * paramstr(0) fix for linux 2.0 kernels (merged)

  Revision 1.3  2000/07/14 10:33:10  michael
  + Conditionals fixed

  Revision 1.2  2000/07/13 11:33:49  michael
  + removed logs

}
