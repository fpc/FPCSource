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

type size_t=longint;
     off_t=int64;
     cint=longint;


{$i syscallh.inc}
{$i ossysch.inc}

function Fplseek(fd : cint; offset : off_t; whence : cint): off_t; cdecl; external name 'FPC_SYSC_LSEEK';

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

{$ifndef m68k}
Function Sys_Lseek(F:longint;Off:longint;Whence:longint):longint;
var returnvalue64 : int64;
begin
 {Lseek's offset is 64-bit, the highword  is the 0}
{$ifdef i386} 
 do_syscall(syscall_nr___syscall,syscall_nr_lseek,0,f,0,off,0,whence);
 asm
  lea  returnvalue64,%ebx
  movl %eax,(%ebx)
  movl %edx,4(%ebx)
 end;
 sys_lseek:=longint(returnvalue64);
 {$else}		// powerpc
  sys_lseek:=fplseek(f,off,whence);
 {$endif}
end;
{$endif m68k}


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
  sys_mkdir:=do_syscall(syscall_nr_mkdir,longint(filename),mode );
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
     errno:=Esysenotdir;
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
    l              : pdirent;
    novalid        : boolean;

begin
 if (p^.buf=nil) or (p^.loc=0) THEN
  exit(nil);
 if p^.loc=cardinal(-1) then         {First readdir on this pdir. Initial fill of buffer}
  begin
   if readbuffer()=0 Then    {nothing to be read}
    exit(nil)
  end;
 l:=nil;
 repeat
  novalid:=false;
  if (pdirent(p^.rewind)^.reclen<>0) then
   begin {valid direntry?}
    if pdirent(P^.rewind)^.ino<>0 then
     l:=pdirent(p^.rewind);
    inc(p^.rewind,pdirent(p^.rewind)^.reclen);
    if p^.rewind>=(cardinal(p^.buf)+dirblksiz) then
     novalid:=true;
   end
  else
   novalid:=true;
  if novalid then
   begin {block entirely searched or reclen=0}
    if p^.loc<>0 THEN             {blocks left?}
     if readbuffer()<>0 then        {succesful read?}
      novalid:=false;
   end;
 until (l<>nil) or novalid;
 If novalid then
  l:=nil;
 Sys_ReadDir:=l;
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
Function Sys_FTruncate(Handle,Pos:longint):longint;  //moved from sysunix.inc Do_Truncate
begin
  Sys_FTruncate:=do_syscall(syscall_nr___syscall, syscall_nr_ftruncate,0,handle,0,pos,0);
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
  Sys_mmap:=fpmmap(Adr,Len,Prot,Flags,fdes,off);
end;


{
  $Log$
  Revision 1.5  2003-05-30 19:58:40  marco
   * Getting NetBSD/i386 to compile.

  Revision 1.3  2003/01/21 15:39:45  marco
   * NetBSD first rtl. Still not 100%, but close

  Revision 1.2  2003/01/17 22:13:47  marco
   * some updates

  Revision 1.1.2.4  2002/09/20 07:04:44  pierre
  * avoid compiler warning

  Revision 1.1.2.3  2002/01/23 09:10:05  marco
   * Similar truncate test. truncate tests from testsuite now seem to pass

  Revision 1.1.2.2  2001/08/29 09:43:07  marco
   * first lseek fix. Still not 100%, but don't know exact problem yet.

  Revision 1.1.2.1  2001/08/14 20:22:53  pierre
   New file

  Revision 1.1.2.7  2001/04/04 10:38:36  marco
   * Small fix to readdir. Is this *the* bug?

  Revision 1.1.2.6  2001/03/14 17:19:10  marco
   * Readdir compiles (some conflicts in Linux). Untested. Use NewReaddir to enable

  Revision 1.1.2.5  2001/03/12 20:37:50  marco
   * [Solaris] Now cycles for FreeBSD (wrong version Linux unit commited)

  Revision 1.1.2.4  2001/03/12 14:57:38  marco
   * [solaris] added some sys functions to decrease amount of ifdefs needed

  Revision 1.1.2.3  2000/09/19 09:58:49  marco
   * Mkdir fix

  Revision 1.1.2.2  2000/09/18 12:14:41  marco
   * An addw in the do_syscall(integer) caused warnings. Fixed

  Revision 1.1.2.1  2000/09/16 11:19:08  marco
   * Moved files from BSD to FreeBSD directory, with some small changes

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
