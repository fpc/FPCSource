{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    The syscalls for the new RTL, moved to platform dependant dir.
    Old linux calling convention is stil kept.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Type

  TSysResult = longint; // all platforms, cint=32-bit.
			// On platforms with off_t =64-bit, people should
			// use int64, and typecast all other calls to cint.

// I don't think this is going to work on several platforms 64-bit machines
// don't have only 64-bit params.

  TSysParam  = Longint;

{$ASMMODE ATT}

function Do_SysCall(sysnr:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL0'];

asm
{ load the registers... }
  movl sysnr,%eax
  int $0x80
  test %eax,%eax
  jae   .LSysc
  negl %eax
  mov  %eax,Errno
  mov  $-1,%eax
.LSysc:
end;     

function Do_SysCall(sysnr,param1 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  int $0x80
  test %eax,%eax
  jae   .LSysc
  negl %eax
  mov  %eax,Errno
  mov  $-1,%eax
.LSysc:
end;

function Do_SysCall(sysnr,param1,param2 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  int $0x80
  test %eax,%eax
  jae   .LSysc
  negl %eax
  mov  %eax,Errno
  mov  $-1,%eax
.LSysc:

end;

function Do_SysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  movl param3,%edx
  int $0x80
  test %eax,%eax
  jae   .LSysc
  negl %eax
  mov  %eax,Errno
  mov  $-1,%eax
.LSysc:
end;

function Do_SysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  movl param3,%edx
  movl param4,%esi
  int $0x80
  test %eax,%eax
  jae   .LSysc
  negl %eax
  mov  %eax,Errno
  mov  $-1,%eax
.LSysc:
end;

function Do_SysCall(sysnr,param1,param2,param3,param4,param5 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  movl param3,%edx
  movl param4,%esi
  movl param5,%edi
  int $0x80
  test %eax,%eax
  jae   .LSysc
  negl %eax
  mov  %eax,Errno
  mov  $-1,%eax
.LSysc:
end;

function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  movl param3,%edx
  movl param4,%esi
  movl param5,%edi
  int $0x80
  test %eax,%eax
  jae   .LSysc
  negl %eax
  mov  %eax,Errno
  mov  $-1,%eax
.LSysc:
end;

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
{$ASMMODE ATT}
{$define fpc_syscall_ok}
asm
{ load the registers... }
  movl 12(%ebp),%eax
  movl 4(%eax),%ebx
  movl 8(%eax),%ecx
  movl 12(%eax),%edx
  movl 16(%eax),%esi
  movl 20(%eax),%edi
{ set the call number }
  movl 8(%ebp),%eax
{ Go ! }
  int $0x80
{ Put back the registers... }
  pushl %eax
  movl 12(%ebp),%eax
  movl %edi,20(%eax)
  movl %esi,16(%eax)
  movl %edx,12(%eax)
  movl %ecx,8(%eax)
  movl %ebx,4(%eax)
  popl %ebx
  movl %ebx,(%eax)
end;

{$ASMMODE DEFAULT}

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
     setErrNo(-regs.reg1);
     SysCall:=-1;
   end
  else
   begin
{$IFDEF SYSCALL_DEBUG}
  if DoSysCallDebug then
       debugtxt:=' syscall returned: ';
{$endif}
     SysCall:=regs.reg1;
     seterrno(0);
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



{
  $Log$
  Revision 1.1  2002-11-12 13:31:32  marco
   * New syscall.inc


}
 