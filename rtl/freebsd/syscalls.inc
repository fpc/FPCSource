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

{BSD version of the syscalls required to implement SysLinux.}

{No debugging for syslinux include !}
{$IFDEF SYS_LINUX}
  {$UNDEF SYSCALL_DEBUG}
{$ENDIF SYS_LINUX}

{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

{ The system designed for Linux can't be used for FreeBSD so easily, since
  FreeBSD pushes arguments, instead of loading them to registers.

For now I do them in assembler, which makes it easier to test them (copy and
paste to and AS source). Ultimately I hope to design something like this}

{actualsyscall:
  _actualsyscall : int $0x80
                   jb someerror
                   ret
      someerror:   storeerrorsomewhere
                   ret
}

{
procedure actualsyscall; cdecl; EXTERNAL NAME '_actualsyscall';
}

procedure actualsyscall; assembler; {inline requires a dummy push IIRC}
    asm
         int $0x80
         jb .LErrorcode
         xor %ebx,%ebx
         ret
.LErrorcode:
	 mov %eax,%ebx
         mov $-1,%eax
   end;

function Do_SysCall(sysnr:LONGINT):longint; assembler;

asm
  movl  sysnr,%eax
  call  actualsyscall
  movw  %bx,Errno
end;

function Do_SysCall(sysnr,param1:longint):longint; assembler;

 asm
  movl  sysnr,%eax
  pushl Param1
  call  actualsyscall
  addl  $4,%esp
  movw  %bx,Errno
 end;

function Do_SysCall(sysnr,param1:integer):longint; assembler;

 asm
  movl  sysnr,%eax
  pushw Param1
  call  actualsyscall
  add   $2,%esp
  movw  %bx,Errno
 end;


function Do_SysCall(sysnr,param1,param2:LONGINT):longint; assembler;

 asm
   movl  sysnr,%eax
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $8,%esp
   movw  %bx,Errno
 end;

function Do_SysCall(sysnr,param1,param2,param3:LONGINT):longint; assembler;

 asm
   movl  sysnr,%eax
   pushl param3
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $12,%esp
   movw  %bx,Errno
 end;

function Do_SysCall(sysnr,param1,param2,param3,param4:LONGINT):longint; assembler;

asm
   movl  sysnr,%eax
   pushl param4
   pushl param3
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $16,%esp
   movw  %bx,Errno
end;


function Do_SysCall(sysnr,param1,param2,param3,param4,param5:LONGINT):longint;  assembler;

 asm
   movl  sysnr,%eax
   pushl param5
   pushl param4
   pushl param3
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $20,%esp
   movw  %bx,Errno
 end;

function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6:LONGINT):longint;  assembler;

asm
   movl  sysnr,%eax
   pushl param6
   pushl param5
   pushl param4
   pushl param3
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $24,%esp
   movw  %bx,Errno
end;


function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6,param7:LONGINT):longint;  assembler;

asm
   movl  sysnr,%eax
   pushl param7
   pushl param6
   pushl param5
   pushl param4
   pushl param3
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $28,%esp
   movw  %bx,Errno
end;

Function Sys_Time:longint;

VAR tv     : timeval;
    tz     : timezone;
    retval : longint;

begin
  Retval:=do_syscall(116,longint(@tv),longint(@tz));
  If retval=-1 then
   sys_time:=-1
  else
   sys_time:=tv.sec;
end;

{*****************************************************************************
               --- File:File handling related calls ---
*****************************************************************************}


Function Sys_Open(f:pchar;flags:longint;mode:integer):longint;

Begin
 sys_open:=do_syscall(syscall_nr_open,longint(f),flags,mode);
End;

Function Sys_Close(f:longint):longint;

begin
 sys_close:=do_syscall(syscall_nr_close,f);
end;

{
Function Sys_Lseek(F:longint;Off:longint;Whence:longint):longint;

var returnvalue64 : array[0..1] of longint;

begin
 {Lseek's offset is 64-bit, the highword  is the 0}
 do_syscall(syscall_nr_lseek,longint(@returnvalue64),F,Off,0,Whence);
 sys_lseek:=returnvalue64[0];
end;

}

Function Sys_Lseek(F:longint;Off:longint;Whence:longint):longint; assembler;

{this one is special for the return value being 64-bit..}

 asm
  pushl Whence
  pushl $0      // high dword
  pushl Off
  pushl $0
  pushl F
  pushl $0      // Your guess is as good as mine.
  pushl $0xc7   // Actual lseek syscall number.
  movl  $0xc6,%eax
  call  actualsyscall
  addl  $28,%esp
  mov   %ebx,Errno
 end;

Function Sys_Read(f:longint;buffer:pchar;count:longint):longint;

begin
  sys_read:=do_syscall(syscall_nr_read,F,longint(buffer),count);
end;

Function Sys_Write(f:longint;buffer:pchar;count:longint):longint;

begin
 sys_write:=do_syscall(syscall_nr_write,F,longint(buffer),count);
end;

Function Sys_Unlink(Filename:pchar):longint;

begin
  sys_unlink:=do_syscall(syscall_nr_unlink,longint(Filename));
end;

Function Sys_Rename(Oldname,Newname:pchar):longint;

begin
  sys_rename:=do_syscall(syscall_nr_rename,longint(oldname),longint(newname));
end;

Function Sys_Stat(Filename:pchar;var Buffer: stat):longint;
{
   We need this for getcwd
}

begin
 sys_stat:=do_syscall(syscall_nr_stat,longint(filename),longint(@buffer));
end;

Function Sys_Symlink(oldname,newname:pchar):longint;
{
  We need this for erase
}

begin
 sys_symlink:=do_syscall(syscall_nr_symlink,longint(oldname),longint(newname));
end;


Function Sys_ReadLink(name,linkname:pchar;maxlen:longint):longint;

begin
  sys_readlink:=do_syscall(syscall_nr_readlink, longint(name),longint(linkname),maxlen);
end;



{*****************************************************************************
               --- Directory:Directory related calls ---
*****************************************************************************}

Function Sys_Chdir(Filename:pchar):longint;

begin
 sys_chdir:=do_syscall(syscall_nr_chdir,longint(filename));
end;

Function Sys_Mkdir(Filename:pchar;mode:longint):longint;

begin {Mode is 16-bit on F-BSD}
  sys_mkdir:=do_syscall(syscall_nr_mkdir,longint(filename),mode);
end;

Function Sys_Rmdir(Filename:pchar):longint;

begin
 sys_rmdir:=do_syscall(syscall_nr_rmdir,longint(filename));
end;

{$ifndef NewReaddir}

const DIRBLKSIZ=1024;


{ we need this for getcwd, NOT touched for BSD version }
Function OpenDir(f:pchar):pdir;

var
  fd:longint;
  st:stat;
  ptr:pdir;
begin
  opendir:=nil;
  if sys_stat(f,st)<0 then
   exit;
{ Is it a dir ? }
  if not((st.mode and $f000)=$4000)then
   begin
     errno:=sys_enotdir;
     exit
   end;
{ Open it}
  fd:=sys_open(f,OPEN_RDONLY,438);
  if fd<0 then
   exit;
  new(ptr);
  if ptr=nil then
   exit;
  Getmem(ptr^.buf,2*DIRBLKSIZ);
  if ptr^.buf=nil then
   exit;
  ptr^.fd:=fd;
  ptr^.loc:=-1;
  ptr^.rewind:=longint(ptr^.buf);
  ptr^.size:=0;
//  ptr^.dd_max:=sizeof(ptr^.buf^);
  opendir:=ptr;
end;

function CloseDir(p:pdir):integer;
begin
  closedir:=sys_close(p^.fd);
  Freemem(p^.buf);
  dispose(p);
end;


Function Sys_ReadDir(p:pdir):pdirent;
{Different from Linux, Readdir on BSD is based on Getdents, due to the
missing of the readdir syscall.
Getdents requires the buffer to be larger than the blocksize.
This usually the sectorsize =512 bytes, but maybe tapedrives and harddisks
with blockmode have this higher?}

function readbuffer:longint;

var retval :longint;

begin
   retval:=do_syscall(syscall_nr_getdents,longint(p^.fd),longint(@p^.buf^),DIRBLKSIZ {sizeof(getdentsbuffer)});
   p^.rewind:=longint(p^.buf);
   if retval=0 then
    begin
     p^.rewind:=0;
     p^.loc:=0;
    end
   else
    P^.loc:=retval;
 readbuffer:=retval;
end;

var
    FinalEntry     : pdirent;
    novalid        : boolean;
    Reclen	   : Longint;
    CurEntry	   : PDirent;
begin
 if (p^.buf=nil) or (p^.loc=0) THEN
  exit(nil);
 if (p^.loc=-1)   OR     {First readdir on this pdir. Initial fill of buffer}
   (p^.rewind>=(longint(p^.buf)+dirblksiz)) then  {no more entries left?}
  Begin
    if readbuffer=0 then        {succesful read?}
     Exit(NIL);			{No more data}
  End;
 FinalEntry:=NIL;
 CurEntry:=nil;
 repeat
  novalid:=false;
  CurEntry:=pdirent(p^.rewind);
  RecLen:=CurEntry^.reclen;
  if RecLen<>0 Then
   begin {valid direntry?}
    if CurEntry^.ino<>0 then
     FinalEntry:=CurEntry;
    inc(p^.rewind,Reclen);
   end
  else
   begin {block entirely searched or reclen=0}
    Novalid:=True;
    if p^.loc<>0 THEN             {blocks left?}
     if readbuffer()<>0 then        {succesful read?}
      novalid:=false;
   end;
 until (FinalEntry<>nil) or novalid;
 If novalid then
  FinalEntry:=nil;
 Sys_ReadDir:=FinalEntry;
end;

{$endif}


{*****************************************************************************
        --- Process:Process & program handling - related calls ---
*****************************************************************************}


Function sys_GetPid:LongInt;
{
  Get Process ID.
}

begin
 sys_GetPID:=do_syscall(syscall_nr_getpid);
end;

Procedure Sys_Exit(ExitCode:longint);

begin
  do_syscall(syscall_nr_exit,exitcode);
end;

{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}



Procedure SigAction(Signum:longint;Act,OldAct:PSigActionRec );
{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}

begin
  do_syscall(syscall_nr_sigaction,longint(signum),longint(act),longint(oldact));
 {$ifdef linuxunit}
  LinuxError:=Errno;
 {$endif}
end;

(*=================== MOVED from syslinux.inc ========================*)
{
Function Sys_FTruncate(Handle,Pos:longint):longint;  //moved from sysunix.inc Do_Truncate
begin
  Sys_FTruncate:=do_syscall(syscall_nr_ftruncate,handle,pos,0);
end;
}

Function Sys_ftruncate(handle,pos:longint):longint; assembler;

{this one is special for the return value being 64-bit..}

 asm
  pushl $0
  pushl pos
  pushl $0      // Your guess is as good as mine.
  pushl handle
  pushl $0
  pushl $0xc9   // Actual lseek syscall number.
  movl  $0xc6,%eax
  call  actualsyscall
  addl  $24,%esp
  mov   %ebx,Errno
 end;


Function Sys_fstat(fd : longint;var Info:stat):Longint; // This was missing here, instead an fstat call was included in Do_FileSize
begin
  Sys_FStat:=do_SysCall(syscall_nr_fstat,fd,longint(@info));
end;

{$ifdef NewReaddir} 
{$I readdir.inc}
{$endif}


{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}
Function Sys_IOCtl(Handle,Ndx: Longint;Data: Pointer):LongInt;  // This was missing here, instead hardcoded in Do_IsDevice
begin
  Sys_IOCtl:=do_SysCall(syscall_nr_ioctl,handle,Ndx,longint(data));
end;

Function Sys_mmap(adr,len,prot,flags,fdes,off:longint):longint; // moved from sysunix.inc, used in sbrk
begin
  Sys_mmap:=do_syscall(syscall_nr_mmap,Adr,Len,Prot,Flags,fdes,off,0);
end;


{
  $Log$
  Revision 1.8  2002-05-06 07:27:39  marco
   * Fixed a readdir bug, already fixed in januari in 1.0.x

  Revision 1.7  2001/08/27 09:35:07  marco
   * Ftruncate intermediate fix.

  Revision 1.6  2001/06/19 08:34:16  marco
   * Peter didn't merge the FreeBSD directory when he merged the Unix one. Fixed

  Revision 1.5  2001/04/23 11:38:30  marco
   * Small readdir fix from fixes branch.

  Revision 1.4  2000/09/19 09:57:35  marco
   * Fixed mkdir

  Revision 1.3  2000/09/19 09:25:17  marco
   * addw fix for main thread.

  Revision 1.2  2000/09/18 13:42:35  marco
   * FreeBSD support into 1.1

  Revision 1.4  2000/09/11 14:38:10  marco
   * 14 april version killed, and replaced by newer fixes branch version

  Revision 1.1.2.1  2000/09/10 16:12:14  marco
  Initial signals, sockets and clone

  Revision 1.1  2000/07/13 06:30:32  michael
  + Initial import

  Revision 1.15  2000/04/16 16:08:53  marco
   * Fixes (mainly opendir/Readdir/closedir)

  Revision 1.14  2000/04/14 17:04:13  marco
   * Working!

  Revision 1.13  2000/04/10 15:46:52  marco
   * worked all day. probably a lot changed

  Revision 1.11  2000/04/05 13:58:40  marco
   * syscall variablenames reintroduced.

  Revision 1.10  2000/03/16 16:18:12  marco
   * Last changes before next test. ppc386 -h works with these srcs.

  Revision 1.9  2000/03/02 15:34:07  marco
   * added a syscall for 5 longints

  Revision 1.8  2000/03/01 20:03:57  marco
   * small fixes for syslinux

  Revision 1.7  2000/03/01 17:28:40  marco
   * some changes due to updating linux.pp to new syscall

  Revision 1.6  2000/02/27 23:45:39  marco
   * Redone the syscalls

  Revision 1.5  2000/02/04 16:53:26  marco
   * Finished Linux (and rest syscalls) roughly. Some things still need to be
    tested, and checked (off_t calls specially)

  Revision 1.4  2000/02/03 17:04:47  marco
   * additions fixes due to port linux

  Revision 1.3  2000/02/02 18:07:27  marco
   * Ported except for readdir which is 200 lines C code in FBSD linux
        emulator

  Revision 1.2  2000/02/02 16:35:10  marco
   * Ported more functions. Half done now.

  Revision 1.1  2000/02/02 15:41:56  marco
   * Initial BSD version. Still needs a lot of work.

}
