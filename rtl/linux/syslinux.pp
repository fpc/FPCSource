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

{ If you use an aout system, set the conditional AOUT}
{ $Define AOUT}

Unit SysLinux;
Interface

{$ifdef m68k}
{ used for single computations }
const
  BIAS4 = $7f-1;
{$endif}

{$define newsignal}

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


{$ifdef bsd}
Function sbrk(size : longint) : Longint;

CONST MAP_PRIVATE   =2;
      MAP_ANONYMOUS =$1000;             {$20 under linux}

begin
  Sbrk:=do_syscall(syscall_nr_mmap,0,size,3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0,0);
  if ErrNo<>0 then
   Sbrk:=0;
end;

{$else}
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
{$endif}

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
  sys_close(Handle);
End;


Procedure Do_Erase(p:pchar);
Begin
  sys_unlink(p);
  Errno2Inoutres;
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
End;


Function Do_SeekEnd(Handle:Longint): Longint;
begin
  Do_SeekEnd:=sys_lseek(Handle,0,Seek_End);
end;

{$ifdef BSD}
Function Do_FileSize(Handle:Longint): Longint;
var
  Info : Stat;
Begin
  if do_SysCall(syscall_nr_fstat,handle,longint(@info))=0 then
   Do_FileSize:=Info.Size
  else
   Do_FileSize:=0;
  Errno2Inoutres;
End;
{$ELSE}
Function Do_FileSize(Handle:Longint): Longint;
var
  regs : Syscallregs;
  Info : Stat;
Begin
  regs.reg2:=Handle;
  regs.reg3:=longint(@Info);
  if SysCall(SysCall_nr_fstat,regs)=0 then
   Do_FileSize:=Info.Size
  else
   Do_FileSize:=0;
  Errno2Inoutres;
End;
{$endif}

Procedure Do_Truncate(Handle,Pos:longint);
{$ifndef bsd}
var
  sr : syscallregs;
{$endif}
begin
{$ifdef bsd}
  do_syscall(syscall_nr_ftruncate,handle,pos,0);
{$else}
  sr.reg2:=Handle;
  sr.reg3:=Pos;
  syscall(syscall_nr_ftruncate,sr);
{$endif}
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
  dirtest : stat;
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
{ Check if it's a directory, then we should return io error 2 }
  if ErrNo=0 then
   begin
     if (Sys_fstat(filerec(f).handle,dirtest)<>0) then
      inoutres:=2
     else
      if (dirtest.mode and STAT_IFMT)<>STAT_IFREG then
       inoutres:=2;
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
{$ifndef BSD}
  sr: SysCallRegs;
{$endif}
  Data : array[0..255] of byte; {Large enough for termios info}
begin
{$ifdef BSD}
  Do_IsDevice:=(do_SysCall(syscall_nr_ioctl,handle,$5401,longint(@data))=0);
{$else}
  sr.reg2:=Handle;
  sr.reg3:=$5401; {=TCGETS}
  sr.reg4:=Longint(@Data);
  Do_IsDevice:=(SysCall(Syscall_nr_ioctl,sr)=0);
{$endif}
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
  sys_mkdir(@buffer, 511);
  Errno2Inoutres;
End;


Procedure RmDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  If InOutRes <> 0 then exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  sys_rmdir(@buffer);
  Errno2Inoutres;
End;


Procedure ChDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  If InOutRes <> 0 then exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  sys_chdir(@buffer);
  Errno2Inoutres;
End;


procedure getdir(drivenr : byte;var dir : shortstring);
var
  thisdir      : stat;
  rootino,
  thisino,
  dotdotino    : longint;
  rootdev,
  thisdev,
  dotdotdev    : {$ifdef bsd}longint{$else}word{$endif};
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
end;


{*****************************************************************************
                         SystemUnit Initialization
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
{$ifndef CORRECTFLDCW}
  {$asmmode direct}
{$endif}
  asm
    fninit
    fldcw   fpucw
  end;
{$ifndef CORRECTFLDCW}
  {$asmmode att}
{$endif}
{$endif I386}
end;

{$ifndef BSD}

{$ifndef newSignal}
Procedure SignalToRunError(Sig:longint);
begin
  case sig of
    8 : begin
    { this is not allways necessary but I don't know yet
      how to tell if it is or not PM }
          ResetFPU;
          HandleError(200);
        end;
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

procedure SignalToRunerror(Sig: longint; SigContext: SigContextRec); cdecl;
var
  res,fpustate : word;
begin
  res:=0;
  case sig of
    8 : begin
    { this is not allways necessary but I don't know yet
      how to tell if it is or not PM }
{$ifdef I386}
          fpustate:=0;
          res:=200;
          if assigned(SigContext.fpstate) then
            fpuState:=SigContext.fpstate^.sw;
{$ifdef SYSTEMDEBUG}
          Writeln(stderr,'FpuState = ',Hexstr(FpuState,4));
{$endif SYSTEMDEBUG}
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
   11 : res:=216;
  end;
{ give runtime error at the position where the signal was raised }
  if res<>0 then
   begin
{$ifdef I386}
     HandleErrorAddrFrame(res,SigContext.eip,SigContext.ebp);
{$else}
     HandleError(res);
{$endif}
   end;
end;

Procedure InstallSignals;
const
  act: SigActionRec = (handler:(Sa:@SignalToRunError);sa_mask:0;sa_flags:0;
                       Sa_restorer: NIL);
  oldact: PSigActionRec = Nil;
begin
  ResetFPU;
  SigAction(8,@act,oldact);
  SigAction(11,@act,oldact);
end;
{$endif newSignal}

{$endif bsd}

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
  {$ifndef bsd}
   InstallSignals;
  {$endif}
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
  Revision 1.2  2000-07-13 11:33:49  michael
  + removed logs
 
}
