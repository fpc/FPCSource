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

procedure actualsyscall; cdecl; EXTERNAL NAME '_actualsyscall';

function Do_SysCall(sysnr:LONGINT):longint;

var retval:longint;

begin
 asm
  movl  sysnr,%eax
  call  actualsyscall
  mov   %eax,Retval
 end;
 if RetVal<0 then
   begin
     ErrNo:=-RetVal;
     do_syscall:=-1;
   end
  else
   begin
     do_syscall:=Retval;
     errno:=0
   end;
end;

function Do_SysCall(sysnr,param1:LONGINT):longint;

var retval:longint;

begin
 asm
  movl  sysnr,%eax
  pushl Param1
  call  actualsyscall
  addl  $4,%esp
  mov   %eax,Retval
 end;
 if RetVal<0 then
   begin
     ErrNo:=-RetVal;
     do_syscall:=-1;
   end
  else
   begin
     do_syscall:=Retval;
     errno:=0
   end;
end;


function Do_SysCall(sysnr:longint;param1:integer):longint;

var retval:longint;

begin
 asm
  movl  sysnr,%eax
  pushw Param1
  call  actualsyscall
  addl  $2,%esp
  mov   %eax,Retval
 end;
 if RetVal<0 then
   begin
     ErrNo:=-RetVal;
     do_syscall:=-1;
   end
  else
   begin
     do_syscall:=Retval;
     errno:=0
   end;
end;


function Do_SysCall(sysnr,param1,param2:LONGINT):longint;

var retval:longint;

begin
 asm
   movl  sysnr,%eax
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $8,%esp
   mov   %eax,Retval
 end;
 if RetVal<0 then
   begin
     ErrNo:=-RetVal;
     do_syscall:=-1;
   end
  else
   begin
     do_syscall:=Retval;
     errno:=0
   end;
 end;

function Do_SysCall(sysnr,param1,param2,param3:LONGINT):longint;
var retval:longint;

begin
 asm
   movl  sysnr,%eax
   pushl param3
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $12,%esp
   mov   %eax,Retval
 end;
 if RetVal<0 then
   begin
     ErrNo:=-RetVal;
     do_syscall:=-1;
   end
  else
   begin
     do_syscall:=Retval;
     errno:=0
   end;
end;

function Do_SysCall(sysnr,param1,param2:longint;param3:word):longint;
var retval:longint;

begin
 asm
   movl  sysnr,%eax
   pushw param3
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $12,%esp
   mov   %eax,Retval
 end;
 if RetVal<0 then
   begin
     ErrNo:=-RetVal;
     do_syscall:=-1;
   end
  else
   begin
     do_syscall:=Retval;
     errno:=0
   end;
end;


function Do_SysCall(sysnr,param1,param2,param3,param4:LONGINT):longint;

var retval:longint;

begin
 asm
   movl  sysnr,%eax
   pushl param4
   pushl param3
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $16,%esp
   mov   %eax,Retval
 end;
 if RetVal<0 then
   begin
     ErrNo:=-RetVal;
     do_syscall:=-1;
   end
  else
   begin
     do_syscall:=Retval;
     errno:=0
   end;
 end;


function Do_SysCall(sysnr,param1,param2,param3,param4,param5:LONGINT):longint;

var retval:longint;

begin
 asm
   movl  sysnr,%eax
   pushl param5
   pushl param4
   pushl param3
   pushl param2
   pushl Param1
   call  actualsyscall
   addl  $20,%esp
   mov   %eax,Retval
 end;
 if RetVal<0 then
   begin
     ErrNo:=-RetVal;
     do_syscall:=-1;
   end
  else
   begin
     do_syscall:=Retval;
     errno:=0
   end;
 end;

function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6,param7:LONGINT):longint;

var retval:longint;

begin
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
   mov   %eax,Retval
 end;
 if RetVal<0 then
   begin
     ErrNo:=-RetVal;
     do_syscall:=-1;
   end
  else
   begin
     do_syscall:=Retval;
     errno:=0
   end;
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

Function Sys_Lseek(F:longint;Off:longint;Whence:longint):longint;

begin
 sys_lseek:=do_syscall(syscall_nr_lseek,F,Off,Whence);
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
  sys_rename:=do_syscallsyscall_nr_rename,longint(oldname),longint(newname));
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
  sys_mkdir:=do_syscall(syscall_nr_mkdir,longint(filename),mode shl 8);
end;

Function Sys_Rmdir(Filename:pchar):longint;

begin
 sys_rmdir:=do_syscall(syscall_nr_rmdir,longint(filename));
end;

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
  new(ptr^.buf);
  if ptr^.buf=nil then
   exit;
  ptr^.fd:=fd;
  ptr^.loc:=0;
  ptr^.size:=0;
  ptr^.dd_max:=sizeof(ptr^.buf^);
  opendir:=ptr;
end;

function CloseDir(p:pdir):integer;
begin
  closedir:=sys_close(p^.fd);
  dispose(p^.buf);
  dispose(p);
end;


Function Sys_ReadDir(p:pdir):pdirent;
var
    retval : longint;
begin
  retval:=do_syscall(syscall_nr_getdents,longint(p^.fd),longint(p^.buf),sizeof(dirent));
  if retval=0 then
   sys_readdir:=nil
  else
   sys_readdir:=p^.buf
end;

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
  do_syscall(syscall_nr_,exitcode);
end;

{
  $Log$
  Revision 1.11  2000-04-05 13:58:40  marco
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