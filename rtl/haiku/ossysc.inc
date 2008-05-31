{
    Copyright (c) 2002 by Marco van de Voort

    The base *BSD syscalls required to implement the system unit. These
    are aliased for use in other units (to avoid poluting the system units
    interface)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************
}

{$i ostypes.inc}

{$ifdef FPC_USE_LIBC}
  {$Linklib root}
  // Out of date atm.
const clib = 'root';
const netlib = 'network';


{$ifdef FPC_IS_SYSTEM}
{$i oscdeclh.inc}
{$endif}
{$I osmacro.inc}

{   var
     Errno : cint; external name 'errno';

    function Fptime(tloc:ptime_t): time_t; cdecl; external name 'time';
    function Fpopen(const path: pchar; flags : cint; mode: mode_t):cint; cdecl; external name 'open';
    function Fpclose(fd : cint): cint; cdecl; external name 'close';
    function Fplseek(fd : cint; offset : off_t; whence : cint): off_t; cdecl; external name 'lseek';
    function Fpread(fd: cint; buf: pchar; nbytes : size_t): ssize_t; cdecl; external name 'read';
    function Fpwrite(fd: cint;const buf:pchar; nbytes : size_t): ssize_t; cdecl; external name 'write';
    function Fpunlink(const path: pchar): cint; cdecl; external name 'unlink';
    function Fprename(const old : pchar; const newpath: pchar): cint; cdecl;external name 'rename';
    function Fpstat(const path: pchar; var buf : stat): cint; cdecl; external name 'stat';
    function Fpchdir(const path : pchar): cint; cdecl; external name 'chdir';
    function Fpmkdir(const path : pchar; mode: mode_t):cint; cdecl; external name 'mkdir';
    function Fprmdir(const path : pchar): cint; cdecl; external name 'rmdir';
    function Fpopendir(const dirname : pchar): pdir; cdecl; external name 'opendir';
    function Fpreaddir(var dirp : dir) : pdirent;cdecl; external name 'readdir';
    function Fpclosedir(var dirp : dir): cint; cdecl; external name 'closedir';
    procedure Fpexit(status : cint); cdecl; external name '_exit';
    function Fpsigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint; cdecl; external name 'sigaction';
    function Fpftruncate(fd : cint; flength : off_t): cint; cdecl; external name 'ftruncate';
    function Fprename(const old : pchar; const newpath: pchar): cint; cdecl;external name 'rename';
    function Fpfstat(fd : cint; var sb : stat): cint; cdecl; external name 'fstat';
    function Fpfork : pid_t; cdecl; external name 'fork';
    function Fpexecve(const path : pchar; const argv : ppchar; const envp: ppchar): cint; cdecl; external name 'execve';
    function Fpwaitpid(pid : pid_t; tat_loc : pcint; options: cint): pid_t; cdecl; external name 'waitpid';
    function Fpaccess(const pathname : pchar; amode : cint): cint; cdecl; external name 'access';

    function Fpuname(var name: utsname): cint; cdecl; external name 'uname';

    function FpDup(oldd:cint):cint; cdecl; external name 'dup';
    function FpDup2(oldd:cint;newd:cint):cint; cdecl; external name 'dup2';
}
{$else}

{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

{ The system designed for Linux can't be used for *BSD so easily, since
  *BSD pushes arguments, instead of loading them to registers.}

// Var ErrNo : Longint;

{$I syscallh.inc}
{$I syscall.inc}
{$I sysnr.inc}
{$I osmacro.inc}

// Should be moved to a FreeBSD specific unit in the future.

function Fptime( tloc:ptime): time_t; [public, alias : 'FPC_SYSC_TIME'];

{VAR tv     : timeval;
    tz     : timezone;
    retval : longint;
}
var
  args : SysCallArgs;
begin
    { don't treat errno, since there is never any }
    tloc^ := Do_Syscall(syscall_nr_time,args);
    fptime := tloc^;
{begin
//  Retval:=do_syscall(syscall_nr_gettimeofday,TSysParam(@tv),TSysParam(@tz));
  If retval=-1 then
   Fptime:=-1
  else
   Begin
   If Assigned(tloc) Then
     TLoc^:=tv.tv_sec;
    Fptime:=tv.tv_sec;
   End;
}
End;

{*****************************************************************************
               --- File:File handling related calls ---
*****************************************************************************}

function Fpopen(path: pchar; flags : cint; mode: mode_t):cint; [public, alias : 'FPC_SYSC_OPEN'];
var
  args: SysCallArgs;
begin
  args.param[1] := $FFFFFFFF;
  args.param[2] := cint(path);
  args.param[3] := flags;
  args.param[4] := cint(mode);
  args.param[5] := 0;               { close on execute flag }
  fpopen:= SysCall(syscall_nr_open, args);   
{Begin
 Fpopen:=do_syscall(syscall_nr_open,TSysParam(path),TSysParam(flags),TSysParam(mode));
}
End;

function Fpclose(fd : cint): cint; [public, alias : 'FPC_SYSC_CLOSE'];
var
  args : SysCallArgs;
begin
  args.param[1] := fd;
  fpclose:=SysCall(syscall_nr_close,args);
{begin
 Fpclose:=do_syscall(syscall_nr_close,fd);
}
end;

{$ifdef netbsd}
  {$ifdef cpupowerpc}
    {$define netbsdmacppc}
  {$endif}
{$endif}

{$ifdef netbsdmacppc}
{$i sysofft.inc}                        // odd ball calling convention.
{$else}
  // generic versions.
function Fplseek(fd : cint; offset : off_t; whence : cint): off_t; [public, alias : 'FPC_SYSC_LSEEK'];

{
this one is special for the return value being 64-bit..
hi/lo offset not yet tested.

NetBSD: ok, but implicit return value in edx:eax
FreeBSD: same implementation as NetBSD.
}
var
  args: SysCallArgs;

begin
  args.param[1] := fd;
  args.param[2] := cint(offset and $FFFFFFFF);      
  args.param[3] := cint((offset shr 32) and $FFFFFFFF);
  args.param[4] := whence;
  { we currently only support seeks upto 32-bit in length }
  fplseek := off_t(SysCall(syscall_nr_lseek,args));
(*begin
  Fplseek:=do_syscall(syscall_nr___syscall,syscall_nr_lseek,0,TSysParam(fd),0,lo(Offset),{0} hi(offset),Whence);
*)
end;

type
  { _kwstat_ kernel call structure }
  pwstat = ^twstat;
  twstat = packed record
{00}   filler : array[1..3] of longint;
{12}   newmode : mode_t;     { chmod mode_t parameter }
{16}   unknown1 : longint;  
{20}   newuser : uid_t;      { chown uid_t parameter  } 
{24}   newgroup : gid_t;     { chown gid_t parameter  }
{28}   trunc_offset : off_t; { ftrucnate parameter    }
{36}   unknown2 : array[1..2] of longint;
{44}   utime_param: int64;  
{52}   unknown3 : array[1..2] of longint;
  end;
  
function Fpftruncate(fd : cint; flength : off_t): cint; [public, alias : 'FPC_SYSC_FTRUNCATE'];
var
  args: SysCallArgs;
  wstat : pwstat;
begin
  New(wstat);
  FillChar(wstat^,sizeof(wstat),0);
  wstat^.trunc_offset := flength;
  args.param[1] := fd;
  args.param[2] := $00000000;
  args.param[3] := cint(wstat);
  args.param[4] := $00000008;
  args.param[5] := $00000001;
  fpftruncate:=SysCall(syscall_nr_ftruncate, args);
  Dispose(wstat);
{begin
 Fpftruncate:=Do_syscall(syscall_nr___syscall,syscall_nr_ftruncate,0,fd,0,lo(flength),hi(flength));
}
end;

const
  B_OS_NAME_LENGTH = 32;
  B_PAGE_SIZE = 4096;  

const
  B_NO_LOCK       = 0;
  B_LAZY_LOCK     = 1;
  B_FULL_LOCK     = 2;
  B_CONTIGUOUS    = 3;
  B_LOMEM         = 4;

  B_ANY_ADDRESS        = 0;
  B_EXACT_ADDRESS      = 1;
  B_BASE_ADDRESS       = 2;
  B_CLONE_ADDRESS      = 3;
  B_ANY_KERNEL_ADDRESS = 4;

  B_READ_AREA  = 1;
  B_WRITE_AREA = 2;

type
  area_id   = Longint;
  
function create_area(name : pchar; var addr : longint;
  addr_typ : longint; size : longint; lock_type: longint; protection : longint): area_id;
var
 args : SysCallArgs;
begin
 args.param[1] := cint(name);
 args.param[2] := cint(@addr);
 args.param[3] := cint(addr_typ);
 args.param[4] := cint(size);
 args.param[5] := cint(lock_type);
 args.param[6] := cint(protection);
 create_area := SysCall(syscall_nr_create_area, args);
end;

Function Fpmmap(start:pointer;len:size_t;prot:cint;flags:cint;fd:cint;offst:off_t):pointer; [public, alias:  'FPC_SYSC_MMAP'];
var
  heap_handle : area_id;
const
  zero=0;
  myheapsize=$20000;
  myheaprealsize=$20000;
var
  myheapstart:pointer;
  s : string;
begin
  WriteLn('fpmmap');
  Str(len, s);
  WriteLn(s);
  myheapstart:=start;
{$IFDEF FPC_USE_LIBC}  
  heap_handle := create_area('fpcheap',myheapstart,0,len,0,3);//!!
{$ELSE}
  heap_handle := create_area('fpcheap',longint(myheapstart),0,len,0,3);//!!
{$ENDIF}
  case heap_handle of
    B_BAD_VALUE : WriteLn('B_BAD_VALUE');
    B_PAGE_SIZE : WriteLn('B_PAGE_SIZE');
    B_NO_MEMORY : WriteLn('B_NO_MEMORY');
    B_ERROR : WriteLn('B_ERROR');
  end;

  fpmmap := myheapstart;
// not available under BeOS
//  Fpmmap:=pointer(longint(do_syscall(syscall_nr_mmap,TSysParam(Start),Len,Prot,Flags,fd,{$ifdef cpupowerpc}0,{$endif}offst{$ifdef cpui386},0{$endif})));
end;

{$endif}


function Fpread(fd: cint; buf: pchar; nbytes : size_t): ssize_t; [public, alias : 'FPC_SYSC_READ'];
var
  args : SysCallArgs;
  funcresult: ssize_t;
  errorcode : cint;
begin
  args.param[1] := fd;
  args.param[2] := cint(buf);
  args.param[3] := cint(nbytes);
  args.param[4] := cint(@errorcode);
  funcresult := ssize_t(Do_SysCall(syscall_nr_read,args));
  if funcresult >= 0 then
   begin
     fpread := funcresult;
     errno := 0;
   end
  else
   begin
     fpread := -1;
     errno := errorcode;
   end;
{begin
  Fpread:=do_syscall(syscall_nr_read,Fd,TSysParam(buf),nbytes);
}
end;

//function Fpmywrite(fd: cint;const buf:pchar; nbytes : size_t): ssize_t; cdecl; external name 'write';

function Fpwrite(fd: cint;buf:pchar; nbytes : size_t): ssize_t; [public, alias : 'FPC_SYSC_WRITE'];
var
  args : SysCallArgs;
  funcresult : ssize_t;
  errorcode : cint;
begin
  errorcode := 0;
  // There is a bug in syscall in 1.9 under BeOS !!!
  // Fixed ! 26/05/2004 ! See in syscall.inc
  args.param[1] := fd;
  args.param[2] := cint(buf);
  args.param[3] := cint(nbytes);
  args.param[4] := cint(@errorcode);
  funcresult := Do_SysCall(syscall_nr_write,args);

//  funcresult := Fpmywrite(fd, buf, nbytes);

  if funcresult >= 0 then
   begin
     fpwrite := funcresult;
     errno := 0;
   end
  else
   begin
     fpwrite := -1; 
     errno := errorcode;
   end;
{begin
 Fpwrite:=do_syscall(syscall_nr_write,Fd,TSysParam(buf),nbytes);
}
end;

function Fpunlink(const path: pchar): cint; [public, alias : 'FPC_SYSC_UNLINK'];
var
  args :SysCallArgs;
begin
  args.param[1] := $FFFFFFFF;
  args.param[2] := cint(path);
  fpunlink := SysCall(syscall_nr_unlink,args);
{begin
  Fpunlink:=do_syscall(syscall_nr_unlink,TSysParam(path));
}
end;

function Fprename(old : pchar; newpath: pchar): cint; [public, alias : 'FPC_SYSC_RENAME'];
var
  args: SysCallArgs;
begin
  args.param[1] := $FFFFFFFF;
  args.param[2] := cint(old);
  args.param[3] := $FFFFFFFF;
  args.param[4] := cint(newpath);
  fprename := SysCall(syscall_nr_rename,args);
{begin
  Fprename:=do_syscall(syscall_nr_rename,TSysParam(old),TSysParam(newpath));
}
end;

function Fpstat(const path: pchar; var buf : stat):cint; [public, alias : 'FPC_SYSC_STAT'];
var
  args : SysCallArgs;
begin
  args.param[1] := $FFFFFFFF;
  args.param[2] := cint(path);
  args.param[3] := cint(@buf);
  args.param[4] := $01000000;
  fpstat := SysCall(syscall_nr_stat, args);
{begin
 Fpstat:=do_syscall(syscall_nr_stat,TSysParam(path),TSysParam(@buf));
}
end;


{*****************************************************************************
               --- Directory:Directory related calls ---
*****************************************************************************}

function Fpchdir(path : pchar): cint; [public, alias : 'FPC_SYSC_CHDIR'];
var
  args: SysCallArgs;
begin
  args.param[1] := $FFFFFFFF;
  args.param[2] := cint(path);
  fpchdir := SysCall(syscall_nr_chdir, args);
{begin
 Fpchdir:=do_syscall(syscall_nr_chdir,TSysParam(path));
}
end;

function Fpmkdir(path : pchar; mode: mode_t):cint; [public, alias : 'FPC_SYSC_MKDIR'];
var
  args :SysCallArgs;
begin
  args.param[1] := $FFFFFFFF;
  args.param[2] := cint(path);
  args.param[3] := cint(mode);
  fpmkdir := SysCall(syscall_nr_mkdir,args);
(*begin {Mode is 16-bit on F-BSD 4!}
  Fpmkdir:=do_syscall(syscall_nr_mkdir,TSysParam(path),mode);
*)
end;

function Fprmdir(path : pchar): cint;  [public, alias : 'FPC_SYSC_RMDIR'];
var
  args: SysCallArgs;
begin
  args.param[1] := $FFFFFFFF;
  args.param[2] := cint(path);
  fprmdir := SysCall(syscall_nr_rmdir,args);
{begin
 Fprmdir:=do_syscall(syscall_nr_rmdir,TSysParam(path));
}
end;

{$ifndef NewReaddir}

const DIRBLKSIZ=1024;


function Fpopendir(dirname : pchar): pdir;  [public, alias : 'FPC_SYSC_OPENDIR'];
var
  args : SysCallArgs;
  dirp: pdir;
  fd : cint;
begin
  New(dirp);
  { just in case }
  FillChar(dirp^,sizeof(dir),#0);
  if assigned(dirp) then
	 begin
	   args.param[1] := $FFFFFFFF;
     args.param[2] := cint(dirname);
 	   args.param[3] := 0;
     fd:=SysCall(syscall_nr_opendir,args);
	   if fd = -1 then
	    begin
	      Dispose(dirp);
	      fpopendir := nil;
	      exit;
	    end;
	   dirp^.fd := fd;
	   fpopendir := dirp;
	   exit;
	 end;
  Errno := ESysEMFILE;
  fpopendir := nil;
(*var
  fd:longint;
  st:stat;
  ptr:pdir;
begin
  Fpopendir:=nil;
  if Fpstat(dirname,st)<0 then
   exit;
{ Is it a dir ? }
  if not((st.st_mode and $f000)=$4000)then
   begin
     errno:=ESysENOTDIR;
     exit
   end;
{ Open it}
  fd:=Fpopen(dirname,O_RDONLY,438);
  if fd<0 then
   Begin
    Errno:=-1;
    exit;
   End;
  new(ptr);
  if ptr=nil then
   Begin
    Errno:=1;
    exit;
   End;
  Getmem(ptr^.dd_buf,2*DIRBLKSIZ);
  if ptr^.dd_buf=nil then
   exit;
  ptr^.dd_fd:=fd;
  ptr^.dd_loc:=-1;
  ptr^.dd_rewind:=longint(ptr^.dd_buf);
  ptr^.dd_size:=0;
//  ptr^.dd_max:=sizeof(ptr^.dd_buf^);
  Fpopendir:=ptr;
*)
end;

function Fpclosedir(dirp : pdir): cint; [public, alias : 'FPC_SYSC_CLOSEDIR'];
var
  args : SysCallArgs;
begin
  if assigned(dirp) then
   begin
	   args.param[1] := dirp^.fd;
	   fpclosedir := SysCall(syscall_nr_closedir,args);
	   Dispose(dirp);
	   dirp := nil;
	   exit;
    end;
   Errno := ESysEBADF;
   fpclosedir := -1;
{begin
  Fpclosedir:=Fpclose(dirp^.dd_fd);
  Freemem(dirp^.dd_buf);
  dispose(dirp);
}
end;

function Fpreaddir(dirp : pdir) : pdirent; [public, alias : 'FPC_SYSC_READDIR'];

{Different from Linux, Readdir on BSD is based on Getdents, due to the
missing of the readdir syscall.
Getdents requires the buffer to be larger than the blocksize.
This usually the sectorsize =512 bytes, but maybe tapedrives and harddisks
with blockmode have this higher?}

(*function readbuffer:longint;

var retval :longint;

begin
 Retval:=do_syscall(syscall_nr_getdents,TSysParam(dirp^.dd_fd),TSysParam(@dirp^.dd_buf^),DIRBLKSIZ {sizeof(getdentsbuffer)});
   dirp^.dd_rewind:=TSysParam(dirp^.dd_buf);
   if retval=0 then
    begin
     dirp^.dd_rewind:=0;
     dirp^.dd_loc:=0;
    end
   else
    dirP^.dd_loc:=retval;
 readbuffer:=retval;
end;*)
var
  args : SysCallArgs;
  funcresult : cint;
begin
  args.param[1] := dirp^.fd;
  args.param[2] := cint(@(dirp^.ent));
  args.param[3] := $0000011C;
  args.param[4] := $00000001;
  { the error will be processed here }
  funcresult := Do_SysCall(syscall_nr_readdir, args);
  if funcresult <> 1 then
   begin
     if funcresult <> 0 then
       errno := funcresult;
     fpreaddir := nil;
     exit;
   end;
  errno := 0;
  fpreaddir := @dirp^.ent
(*
var
    FinalEntry     : pdirent;
    novalid        : boolean;
    Reclen         : Longint;
    CurEntry       : PDirent;

begin
 if (dirp^.dd_buf=nil) or (dirp^.dd_loc=0) THEN
  exit(nil);
 if (dirp^.dd_loc=-1)   OR     {First readdir on this pdir. Initial fill of buffer}
   (dirp^.dd_rewind>=(longint(dirp^.dd_buf)+dirblksiz)) then  {no more entries left?}
  Begin
    if readbuffer=0 then        {succesful read?}
     Exit(NIL);                 {No more data}
  End;
 FinalEntry:=NIL;
 CurEntry:=nil;
 repeat
  novalid:=false;
  CurEntry:=pdirent(dirp^.dd_rewind);
  RecLen:=CurEntry^.d_reclen;
  if RecLen<>0 Then
   begin {valid direntry?}
    if CurEntry^.d_fileno<>0 then
     FinalEntry:=CurEntry;
    inc(dirp^.dd_rewind,Reclen);
   end
  else
   begin {block entirely searched or reclen=0}
    Novalid:=True;
    if dirp^.dd_loc<>0 THEN             {blocks left?}
     if readbuffer()<>0 then        {succesful read?}
      novalid:=false;
   end;
 until (FinalEntry<>nil) or novalid;
 If novalid then
  FinalEntry:=nil;
 FpReadDir:=FinalEntry;*)
end;
{$endif}

{*****************************************************************************
        --- Process:Process & program handling - related calls ---
*****************************************************************************}

procedure Fpexit(status : cint); [public, alias : 'FPC_SYSC_EXIT'];
var
  args : SysCallArgs;
begin
//  sys_exit(status);
  args.param[1] := status;
  do_syscall(syscall_nr_exit, args);
end;

{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}

function Fpsigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint; [public, alias : 'FPC_SYSC_SIGACTION'];

{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}
var
  args : SysCallArgs;
begin
  args.param[1] := sig;
  args.param[2] := cint(@act);
  args.param[3] := cint(@oact);
  fpsigaction := SysCall(syscall_nr_sigaction, args);
//begin
//  do_syscall(syscall_nr_sigaction,TSysParam(sig),TSysParam(@act),TSysParam(@oact));
end;

(*=================== MOVED from sysunix.inc ========================*)


function Fpfstat(fd : cint; var sb : stat): cint;  [public, alias : 'FPC_SYSC_FSTAT'];

var
  args : SysCallArgs;
begin
  args.param[1] := fd;
  args.param[2] := $00;
  args.param[3] := cint(@sb);
  args.param[4] := $00000001;
  fpfstat := SysCall(syscall_nr_fstat, args);

{begin
  fpFStat:=do_SysCall(syscall_nr_fstat,fd,TSysParam(@sb));
}
end;

{$ifdef NewReaddir}
{$I readdir.inc}
{$endif}


function fork : pid_t; external 'root' name 'fork';
{ These routines are currently not required for BeOS }
function Fpfork : pid_t;  [public, alias : 'FPC_SYSC_FORK'];
{
  This function issues the 'fork' System call. the program is duplicated in memory
  and Execution continues in parent and child process.
  In the parent process, fork returns the PID of the child. In the child process,
  zero is returned.
  A negative value indicates that an error has occurred, the error is returned in
  LinuxError.
}

Begin
  WriteLn('fpfork');
  fpfork := fork;
// Not required for BeOS
// Fpfork:=Do_syscall(SysCall_nr_fork);
End;

{
function Fpexecve(const path : pathstr; const argv : ppchar; const envp: ppchar): cint;
}
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}

{
Begin
  path:=path+#0;
  do_syscall(syscall_nr_Execve,TSysParam(@path[1]),TSysParam(Argv),TSysParam(envp));
End;
}
{
function Fpexecve(const path : pchar; const argv : ppchar; const envp: ppchar): cint;  [public, alias : 'FPC_SYSC_EXECVE'];
}
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}
{
Begin
  do_syscall(syscall_nr_Execve,TSysParam(path),TSysParam(Argv),TSysParam(envp));
End;
}
function waitpid(pid : pid_t; stat_loc : pcint; options: cint): pid_t; external 'root' name 'waitpid';
function Fpwaitpid(pid : pid_t; stat_loc : pcint; options: cint): pid_t; [public, alias : 'FPC_SYSC_WAITPID'];
{
  Waits until a child with PID Pid exits, or returns if it is exited already.
  Any resources used by the child are freed.
  The exit status is reported in the adress referred to by Status. It should
  be a longint.
}

begin // actually a wait4() call with 4th arg 0.
  FpWaitPID := waitpid(pid, stat_loc, options);
// FpWaitPID:=do_syscall(syscall_nr_WaitPID,PID,TSysParam(Stat_loc),options,0);
end;

function Fpaccess(const pathname : pchar; amode : cint): cint; [public, alias : 'FPC_SYSC_ACCESS'];
{
  Test users access rights on the specified file.
  Mode is a mask xosisting of one or more of R_OK, W_OK, X_OK, F_OK.
  R,W,X stand for read,write and Execute access, simultaneously.
  F_OK checks whether the test would be allowed on the file.
  i.e. It checks the search permissions in all directory components
  of the path.
  The test is done with the real user-ID, instead of the effective.
  If access is denied, or an error occurred, false is returned.
  If access is granted, true is returned.
  Errors other than no access,are reported in unixerror.
}
var
  args : SysCallArgs;
begin
  args.param[1] := $FFFFFFFF;
  args.param[2] := cint(pathname);
  args.param[3] := amode;
  fpaccess := SysCall(syscall_nr_access,args);

{begin
 FpAccess:=do_syscall(syscall_nr_access,TSysParam(pathname),amode);
}
end;
(*
function Fpaccess(const pathname : pathstr; amode : cint): cint;

{
  Test users access rights on the specified file.
  Mode is a mask xosisting of one or more of R_OK, W_OK, X_OK, F_OK.
  R,W,X stand for read,write and Execute access, simultaneously.
  F_OK checks whether the test would be allowed on the file.
  i.e. It checks the search permissions in all directory components
  of the path.
  The test is done with the real user-ID, instead of the effective.
  If access is denied, or an error occurred, false is returned.
  If access is granted, true is returned.
  Errors other than no access,are reported in unixerror.
}

begin
 pathname:=pathname+#0;
 Access:=do_syscall(syscall_nr_access, TSysParam(@pathname[1]),mode)=0;
end;
*)

Function FpDup(fildes:cint):cint; [public, alias : 'FPC_SYSC_DUP'];

begin
  {$warning TODO BeOS FpDup implementation}
//  Fpdup:=Do_syscall(syscall_nr_dup,TSysParam(fildes));
end;

Function FpDup2(fildes,fildes2:cint):cint; [public, alias : 'FPC_SYSC_DUP2'];

begin
  {$warning TODO BeOS FpDup2 implementation}
// Fpdup2:=do_syscall(syscall_nr_dup2,TSysParam(fildes),TSysParam(fildes2));
end;



Function Fpmunmap(start:pointer;len:size_t):cint;    [public, alias :'FPC_SYSC_MUNMAP'];
begin
  {$warning TODO BeOS Fpmunmap implementation}
//  Fpmunmap:=do_syscall(syscall_nr_munmap,TSysParam(start),Len);
end;


{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}

Function FpIOCtl(Handle:cint;Ndx: culong;Data: Pointer):cint; [public, alias : 'FPC_SYSC_IOCTL'];
// This was missing here, instead hardcoded in Do_IsDevice
begin
  {$warning TODO BeOS FpIOCtl implementation}
//  FpIOCtl:=do_SysCall(syscall_nr_ioctl,handle,Ndx,TSysParam(data));
end;


Function FpGetPid:LongInt;   [public, alias : 'FPC_SYSC_GETPID'];
{
  Get Process ID.
}

begin
  {$warning TODO BeOS FpGetPid implementation}
// FpGetPID:=do_syscall(syscall_nr_getpid);
end;

function fpgettimeofday(tp: ptimeval;tzp:ptimezone):cint; [public, alias: 'FPC_SYSC_GETTIMEOFDAY'];

begin
  {$warning TODO BeOS fpgettimeofday implementation}
// fpgettimeofday:=do_syscall(syscall_nr_gettimeofday,TSysParam(tp),TSysParam(tzp));
end;

function FPSigProcMask(how:cint;nset : psigset;oset : psigset):cint; [public, alias : 'FPC_SYSC_SIGPROCMASK'];

{
  Change the list of currently blocked signals.
  How determines which signals will be blocked :
   SigBlock   : Add SSet to the current list of blocked signals
   SigUnBlock : Remove the signals in SSet from the list of blocked signals.
   SigSetMask : Set the list of blocked signals to SSet
  if OldSSet is non-null, the old set will be saved there.
}

begin
  {$warning TODO BeOS FPSigProcMask implementation}
//  FPsigprocmask:=do_syscall(syscall_nr_sigprocmask,longint(how),longint(nset),longint(oset));
end;
{$user BLA!}
Function FpNanoSleep(req : ptimespec;rem : ptimespec) : cint; [public, alias : 'FPC_SYSC_NANOSLEEP'];
begin
  {$warning TODO BeOS FpNanoSleep implementation}
{$ifndef darwin}
//  FpNanoSleep:=Do_SysCall(syscall_nr_nanosleep,TSysParam(req),TSysParam(rem));
{$else not darwin}
{$warning: TODO: nanosleep!!!}
{$endif not darwin}
end;

function Fpgetcwd(pt:pchar; _size:size_t):pchar;[public, alias :'FPC_SYSC_GETCWD'];
{$ifndef darwin}
const intpathmax = 1024-4;      // didn't use POSIX data in libc
                                // implementation.
var ept,bpt : pchar;
    c       : char;
    ret     : cint;

begin
  {$warning TODO BeOS Fpgetcwd implementation}
(*   if pt=NIL Then
    begin
      // POSIX: undefined. (exit(nil) ?)
      // BSD  : allocate mem for path.
      getmem(pt,intpathmax);
      if pt=nil Then
        exit(nil);
      ept:=pt+intpathmax;
    end
   else
    Begin
      if (_size=0) Then
        Begin
          seterrno(ESysEINVAL);
          exit(nil);
        End;
      if (_size=1) Then
        Begin
          seterrno(ESysERANGE);
          exit(nil);
        End;
      ept:=pt+_size;
    end;

    ret := do_syscall(syscall_nr___getcwd,TSysParam(pt),TSysParam( ept - pt));
    If (ret = 0) Then
        If (pt[0] <> '/') Then
           Begin
             bpt := pt;
             ept := pt + strlen(pt) - 1;
             While (bpt < ept) Do
               Begin
                 c := bpt^;
                 bpt^:=ept^;
                 inc(bpt);
                 ept^:=c;
                 dec(ept);
               End;
           End;
 Fpgetcwd:=pt;*)
end;
{$else not darwin}
{$i getcwd.inc}
{$endif darwin}

{$endif}

Function Do_IsDevice(Handle:Longint):boolean;
{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}
begin
  do_isdevice:= (handle=StdInputHandle) or
                (handle=StdOutputHandle) or
                (handle=StdErrorHandle);
end;

{
extern _IMPEXP_ROOT status_t  get_image_symbol(image_id imid,
                  const char *name, int32 sclass,  void **ptr);
extern _IMPEXP_ROOT status_t  get_nth_image_symbol(image_id imid, int32 index,
                  char *buf, int32 *bufsize, int32 *sclass,
                  void **ptr);
}

// 
{$ifdef FPC_USE_LIBC}

// private; use the macros, below
function _get_image_info(image : image_id; var info : image_info; size : size_t)
         : status_t; cdecl; external 'root' name '_get_image_info';

function _get_next_image_info(team : team_id; var cookie : Longint; var info : image_info; size : size_t)
         : status_t; cdecl; external 'root' name '_get_next_image_info';

function get_image_info(image : image_id; var info : image_info) : status_t;
begin
  Result := _get_image_info(image, info, SizeOf(info));
end;

function get_next_image_info(team : team_id; var cookie : Longint; var info : image_info) : status_t;
begin
  Result := _get_next_image_info(team, cookie, info, SizeOf(info));
end;

{$else}

    function wait_for_thread(thread: thread_id; var status : status_t): status_t;
     var
      args: SysCallArgs;
      i: longint;
     begin
       args.param[1] := cint(thread);
       args.param[2] := cint(@status);
       wait_for_thread := SysCall(syscall_nr_wait_thread, args);
     end;

    function get_team_info(team: team_id; var info : team_info): status_t;
     var
      args: SysCallArgs;
     begin
       args.param[1] := cint(team);
       args.param[2] := cint(@info);
       get_team_info := SysCall(syscall_nr_get_team_info, args);
     end;


    function kill_team(team: team_id): status_t;
     var
      args: SysCallArgs;
     begin
       args.param[1] := cint(team);
       kill_team := SysCall(syscall_nr_kill_team, args);
     end;

  function get_next_image_info(team : team_id; var cookie: longint;var info : image_info): status_t;
     var
      args: SysCallArgs;
   begin
       args.param[1] := cint(team);
       args.param[2] := cint(@cookie);
       args.param[3] := cint(@info);
       args.param[4] := cint(sizeof(image_info));
       get_next_image_info := SysCall(syscall_nr_get_next_image_info, args);
   end;       

    function load_image(argc : longint; argv : ppchar; envp : ppchar): thread_id;
     var
      args: SysCallArgs;
      i: longint;
     begin
       args.param[1] := cint(argc);
       args.param[2] := cint(argv);
       args.param[3] := cint(envp);
       load_image := SysCall(syscall_nr_load_image, args);
     end;
    
    function get_system_info(var info: system_info): status_t;
     var
      args: SysCallArgs;
      i: longint;
     begin
       args.param[1] := cint(@info);
       i := SysCall(syscall_nr_get_system_info, args);
       get_system_info := i;
     end;

    function dev_for_path(const pathname : pchar): dev_t;
     var
      args: SysCallArgs;
      buffer: array[1..15] of longint;
      i: cint;
     begin
       args.param[1] := $FFFFFFFF;
       args.param[2] := cint(pathname);
       args.param[3] := cint(@buffer);
       args.param[4] := $01000000;
       if SysCall(syscall_nr_rstat, args)=0 then
          i:=buffer[1]
       else
          i:=-1;
       dev_for_path := i;
     end;


    function fs_stat_dev(device: dev_t; var info: fs_info): dev_t;
     var
      args: SysCallArgs;
     begin
       args.param[1] := cint(device);
       args.param[2] := 0;
       args.param[3] := $FFFFFFFF;
       args.param[4] := 0;
       args.param[5] := cint(@info);
       fs_stat_dev := SysCall(syscall_nr_statfs, args);
     end;
     
{$endif}


(* Implemented in sytem under BeOS
CONST
 { Constansts for MMAP }
  MAP_PRIVATE   =2;
  MAP_ANONYMOUS =$1000;

Function sbrk(size : cint) : pointer;
begin
  sbrk:=Fpmmap(nil,cardinal(Size),3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0);
  if sbrk=pointer(-1) then
    sbrk:=nil
  else
    seterrno(0);
end;
*)


