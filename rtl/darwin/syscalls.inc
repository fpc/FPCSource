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

{Darwin version of the syscalls required to implement SysLinux.}

{No debugging for syslinux include !}
{$IFDEF SYS_LINUX}
  {$UNDEF SYSCALL_DEBUG}
{$ENDIF SYS_LINUX}

{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

Procedure Do_SysCall( callnr:longint;var regs : SysCallregs );assembler;
{ 
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
{$ifdef powerpc}
{$define fpc_syscall_ok}
asm 
{ load the registers... }
  lwz  r5, 12(r4)
  lwz  r6, 16(r4)
  lwz  r7, 20(r4)
  lwz  r8, 24(r4)
  lwz  r9, 28(r4)
  mr   r0, r3
  lwz  r3, 4(r4)      
  stw  r4, regs
  lwz  r4, 8(r4)
{ Go ! }
  sc
{ Put back the registers... }
  lwz    r8, regs
  stw    r3, 4(r8)
  stw    r4, 8(r8)
  stw    r5, 12(r8)
  stw    r6, 16(r8)
  stw    r7, 20(r8)
end;
{$endif powerpc}
{$ifndef fpc_syscall_ok}
{$error Cannot decide which processor you have!}
asm
end;
{$endif not fpc_syscall_ok}


{$IFDEF SYSCALL_DEBUG}
Const
  DoSysCallDebug : Boolean = False;

var
  LastCnt,
  LastR0,
  LastCall : longint;
  DebugTxt : string[20];
{$ENDIF}
Function SysCall( callnr:longint;var regs : SysCallregs ):longint;
{
  This function serves as an interface to do_SysCall.
  If the SysCall returned a negative number, it returns -1, and puts the
  SysCall result in errno. Otherwise, it returns the SysCall return value
}
begin
  do_SysCall(callnr,regs);
  if regs.reg1<0 then
   begin
{$IFDEF SYSCALL_DEBUG}
     If DoSysCallDebug then
       debugtxt:=' syscall error: ';
{$endif}
     ErrNo:=-regs.reg1;
     SysCall:=-1;
   end
  else
   begin
{$IFDEF SYSCALL_DEBUG}
  if DoSysCallDebug then
       debugtxt:=' syscall returned: ';
{$endif}
     SysCall:=regs.reg1;
     errno:=0
   end;
{$IFDEF SYSCALL_DEBUG}
  if DoSysCallDebug then
    begin
    inc(lastcnt);
    if (callnr<>lastcall) or (regs.reg1<>lastR0) then
      begin
      if lastcnt>1 then
        writeln(sys_nr_txt[lastcall],debugtxt,lasteax,' (',lastcnt,'x)');
      lastcall:=callnr;
      lasteax:=regs.reg1;
      lastcnt:=0;
      writeln(sys_nr_txt[lastcall],debugtxt,lasteax);
      end;
    end;  
{$endif}  
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



Function Sys_Time:longint;
var
   tv     : timeval;
   tz     : timezone;
   regs : SysCallregs;
   retval: longint;
begin
  regs.reg2:=@tv;
  regs.reg3:=@tz;
  retval:=SysCall(SYS_gettimeofday,regs);
  if retval = -1 then
     Sys_Time := -1
  else
     Sys_Time := tv.sec;
end;

{*****************************************************************************
               --- File:File handling related calls ---
*****************************************************************************}


Function Sys_Open(f:pchar;flags:longint;mode:integer):longint;
var
  regs : SysCallregs;
Begin 
  regs.reg2:=longint(f);
  regs.reg3:=flags;
  regs.reg4:=mode;
  Sys_Open:=SysCall(SysCall_nr_open,regs);
End;


Function Sys_Close(f:longint):longint;
var
  regs : SysCallregs;   
begin
  regs.reg2:=f;  
  Sys_Close:=SysCall(SysCall_nr_close,regs);
end;


Function Sys_Lseek(F:longint;Off:longint;Whence:longint):longint;
var
  regs : SysCallregs;
begin
 {Lseek's offset is 64-bit, the highword  is the 0}
 regs.reg2:=f;
 { strange, even on the PPC, the order of the low/high dword in the }
 { registers is in "little endian" here (JM)                        }
 regs.reg3:=off;
 regs.reg4:=0;
 regs.reg5:=whence
 Sys_Lseek:=do_syscall(syscall_nr_lseek,regs);
end;


Function Sys_Read(f:longint;buffer:pchar;count:longint):longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=f;
  regs.reg3:=longint(buffer);
  regs.reg4:=count;
  Sys_Read:=SysCall(SysCall_nr_read,regs);
end;  


Function Sys_Write(f:longint;buffer:pchar;count:longint):longint;
var  
  regs : SysCallregs;
begin
  regs.reg2:=f;
  regs.reg3:=longint(buffer);
  regs.reg4:=count;
  Sys_Write:=SysCall(SysCall_nr_write,regs);
end;


Function Sys_Unlink(Filename:pchar):longint;
var  
  regs : SysCallregs;
begin
  regs.reg2:=longint(filename);
  Sys_Unlink:=SysCall(SysCall_nr_unlink,regs);
end;  


Function Sys_fstat(fd : longint;var Info:stat):Longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=fd;
  regs.reg3:=longint(@Info);
  Sys_fStat:=SysCall(SysCall_nr_fstat,regs);
end;


Function Sys_Rename(Oldname,Newname:pchar):longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=longint(oldname);
  regs.reg3:=longint(newname);
  Sys_Rename:=SysCall(SysCall_nr_rename,regs);
end;


Function Sys_Stat(Filename:pchar;var Buffer: stat):longint;
{
   We need this for getcwd
}
var  
  regs : SysCallregs;
begin
  regs.reg2:=longint(filename);
  regs.reg3:=longint(@buffer);
  Sys_Stat:=SysCall(SysCall_nr_stat,regs);
end;

Function Sys_Symlink(oldname,newname:pchar):longint;
{
  We need this for erase
}
var
  regs : SysCallregs;
begin
  regs.reg2:=longint(oldname);
  regs.reg3:=longint(newname);
  Sys_symlink:=SysCall(SysCall_nr_symlink,regs);
end;


Function Sys_ReadLink(name,linkname:pchar;maxlen:longint):longint;
var
  regs : SysCallRegs;
begin
  regs.reg2:=longint(name);   
  regs.reg3:=longint(linkname);
  regs.reg4:=maxlen;
  Sys_ReadLink:=SysCall(Syscall_nr_readlink,regs);
end;


{*****************************************************************************
               --- Directory:Directory related calls ---
*****************************************************************************}

Function Sys_Chdir(Filename:pchar):longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=longint(filename);
  Sys_ChDir:=SysCall(SysCall_nr_chdir,regs);
end;


Function Sys_Mkdir(Filename:pchar;mode:longint):longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=longint(filename);
  regs.reg3:=mode;
  Sys_MkDir:=SysCall(SysCall_nr_mkdir,regs);
end; 


Function Sys_Rmdir(Filename:pchar):longint;
var
  regs : SysCallregs;
begin
  regs.reg2:=longint(filename);
  Sys_Rmdir:=SysCall(SysCall_nr_rmdir,regs);
end; 

{*****************************************************************************
        --- Process:Process & program handling - related calls ---
*****************************************************************************}


Function sys_GetPid:LongInt;
{
  Get Process ID.
}
var
  regs : SysCallregs;
begin
  Sys_GetPid:=SysCall(SysCall_nr_getpid,regs);
end;


Procedure Sys_Exit(ExitCode:longint);
var
  regs : SysCallregs;
begin
  regs.reg2:=exitcode;
  SysCall(SysCall_nr_exit,regs)
end;


Procedure SigAction(Signum:longint;Act,OldAct:PSigActionRec );
{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}

Var
  sr : Syscallregs;  
begin
  sr.reg2:=Signum;
  sr.reg3:=Longint(act);
  sr.reg4:=Longint(oldact);
  SysCall(Syscall_nr_sigaction,sr);
 {$ifdef linuxunit}
  LinuxError:=Errno;
 {$endif}
end;

(*=================== MOVED from syslinux.inc ========================*)

Function Sys_FTruncate(Handle,Pos:longint):longint;  //moved from sysunix.inc Do_Truncate
var  
  sr : syscallregs;   
begin
  sr.reg2:=Handle;
  sr.reg3:=Pos;
  sr.reg4:=0;
  Sys_FTruncate:=syscall(syscall_nr_ftruncate,sr);
end;

{ getdents is declared obsolete in Darwin! }
{$I readdir.inc}


Function Sys_mmap(adr,len,prot,flags,fdes,off:longint):longint; // moved from sysunix.inc, used in sbrk
var
  sr: syscallregs;
begin
  sr.reg2:=adr;
  sr.reg3:=len;
  sr.reg4:=prot;
  sr.reg5:=flags;
  sr.reg6:=fdes;
  sr.reg7:=off;
  sr.reg8:=0;
  Sys_mmap:=do_syscall(syscall_nr_mmap,sr);
end;



{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}
Function Sys_IOCtl(Handle,Ndx: Longint;Data: Pointer):LongInt;  // This was missing here, instead hardcoded in Do_IsDevice
var
  sr: SysCallRegs;
begin
  sr.reg2:=Handle;
  sr.reg3:=Ndx;
  sr.reg4:=Longint(Data);   
  Sys_IOCtl:=SysCall(Syscall_nr_ioctl,sr);
end;

{
  $Log$
  Revision 1.1  2002-09-06 18:35:59  jonas
    * implemented most syscalls, except readdir because the getdents
      syscall is declared obsolete in Darwin...

}
