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

{
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
    if (callnr<>lastcall) or (regs.reg1<>lasteax) then
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
}

{$PACKRECORDS C}

TYPE timeval=RECORD
	      tv_sec,
	      tv_used : int64;
	      END;
    timezone=RECORD
	      tz_minuteswest,
	      tz_dsttime   : LONGINT;
              END;


function checkreturnvalue(retval:LONGINT;value:LONGINT):LONGINT;

begin
 if retval<0 THEN
   begin
     errno:=-retval;
     checkreturnvalue:=-1;
   end
  else
   begin
     checkreturnvalue:=value;
     errno:=0
   end;
end;

Function Sys_Time:longint;

VAR tv : timeval;
    tz : timezone;
    retval : longint;
begin
  asm
   lea   tv,%ebx
   pushl %ebx
   lea   tz,%ecx
   pushl %ecx
   mov   $116,%eax
   int   $0x80
   add   $8,%esp
   mov   %eax,retval
  end;
  
 sys_time:=checkreturnvalue(retval,tv.tv_sec);
end;

{*****************************************************************************
               --- File:File handling related calls ---
*****************************************************************************}


Function Sys_Open(f:pchar;flags:longint;mode:integer):longint;

var retval: LONGINT;

Begin
 asm 
   pushl mode
   pushl flags
   pushl f
   movl  $5,%eax
   int   $0x80
   add   $12,%esp
   mov   %eax,retval
  end;
 sys_open:=checkreturnvalue(retval,retval);
End;

Function Sys_Close(f:longint):longint;

var retval: LONGINT;

begin
 asm 
   pushl f
   movl  $6,%eax
   int   $0x80
   addl  $4,%esp
   mov   %eax,retval
 end;
 Sys_Close:=checkreturnvalue(retval,retval);
end;

Function Sys_Lseek(F:longint;Off:longint;Whence:longint):longint;

var retval: LONGINT;

begin
  asm
    pushl Whence
    pushl Off
    pushl F
    mov   $199,%eax
    int   $0x80
    addl  $12,%eax
    mov   %eax,retval
  end;
  Sys_Lseek:=checkreturnvalue(retval,retval);
end;

Function Sys_Read(f:longint;buffer:pchar;count:longint):longint;

var retval: LONGINT;

begin
  asm
    pushl Count
    pushl Buffer
    pushl F
    mov   $3,%eax
    int   $0x80
    addl  $12,%eax
    mov   %eax,retval
  end;   
  Sys_Read:=checkreturnvalue(retval,retval);

end;

Function Sys_Write(f:longint;buffer:pchar;count:longint):longint;

var retval: LONGINT;

begin
  asm
    pushl Count
    pushl Buffer
    pushl F
    mov   $4,%eax
    int   $0x80
    addl  $12,%eax
    mov   %eax,retval
  end;   
 Sys_Write:=checkreturnvalue(retval,retval);
end;

Function Sys_Unlink(Filename:pchar):longint;

var retval: LONGINT;

begin
  asm
    pushl FileName
    mov   $10,%eax
    int   $0x80
    addl  $4,%eax
    mov   %eax,retval
  end;   
 Sys_UnLink:=checkreturnvalue(retval,retval);
end;

Function Sys_Rename(Oldname,Newname:pchar):longint;

var retval: LONGINT;

begin
  asm
    pushl NewName
    pushl OldName
    mov   $38,%eax
    int   $0x80
    addl  $8,%eax
    mov   %eax,retval
  end;   
 Sys_Rename:=checkreturnvalue(retval,retval);
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



{ we need this for getcwd }
Function OpenDir(f:pchar):pdir;
var
  fd:integer;
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
  regs :SysCallregs;
  dummy:longint;
begin
  regs.reg3:=longint(p^.buf);
  regs.reg2:=p^.fd;
  regs.reg4:=1;
  dummy:=SysCall(SysCall_nr_readdir,regs);
{ the readdir system call returns the number of bytes written }
  if dummy=0 then 
   sys_readdir:=nil 
  else 
   sys_readdir:=p^.buf
end;


{*****************************************************************************
        --- Process:Process & program handling - related calls ---
*****************************************************************************}

Procedure Sys_Exit(ExitCode:Integer);
var
  regs : SysCallregs;
begin
  regs.reg2:=exitcode;
  SysCall(SysCall_nr_exit,regs)
end;

{
  $Log$
  Revision 1.1  2000-02-02 15:41:56  marco
   * Initial BSD version. Still needs a lot of work.


}