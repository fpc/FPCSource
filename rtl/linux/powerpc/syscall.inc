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

{No debugging for syslinux include !}
{$IFDEF SYS_LINUX}
  {$UNDEF SYSCALL_DEBUG}
{$ENDIF SYS_LINUX}


{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

function Do_SysCall(sysnr:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  mr  r0,r3
  sc
  bnslr
  neg   r3, r3
  lis   r4,Errno@ha
  stw   r3,Errno@l(r4)
  li    r3,-1
end;

function Do_SysCall(sysnr,param1:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  mr  r0,r3
  mr  r3,r4   
  sc
  bnslr
  neg   r3, r3
  lis   r4,Errno@ha
  stw   r3,Errno@l(r4)
  li    r3,-1
end;


function Do_SysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  mr  r0,r3
  mr  r3,r4   
  mr  r4,r5
  sc
   bnslr
   neg   r3, r3
   lis   r4,Errno@ha
   stw   r3,Errno@l(r4)
   li    r3,-1
end;

function Do_SysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  mr  r0,r3
  mr  r3,r4   
  mr  r4,r5
  mr  r5,r6
  sc 
   bnslr
   neg   r3, r3
   lis   r4,Errno@ha
   stw   r3,Errno@l(r4)
   li    r3,-1

end;


function Do_SysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  mr  r0,r3
  mr  r3,r4   
  mr  r4,r5
  mr  r5,r6
  mr  r6,r7
  sc 
   bnslr
   neg   r3, r3
   lis   r4,Errno@ha
   stw   r3,Errno@l(r4)
   li    r3,-1
end;

function Do_SysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  mr  r0,r3
  mr  r3,r4   
  mr  r4,r5
  mr  r5,r6
  mr  r6,r7
  mr  r7,r8
  sc 
   bnslr
   neg   r3, r3
   lis   r4,Errno@ha
   stw   r3,Errno@l(r4)
   li    r3,-1
end;

// Old style syscall:
// Better use ktrace/strace/gdb for debugging.

Procedure Do_SysCall( callnr:longint;var regs : SysCallregs );assembler;
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
{ load the registers... }
  lwz  r5, 12(r4)
  lwz  r6, 16(r4)
  lwz  r7, 20(r4)
  mr   r0, r3
  lwz  r3, 4(r4)
  stw  r4, regs
  lwz  r4, 8(r4)
{ Go ! }
  sc
  nop
{ Put back the registers... }
  lwz    r8, regs
  stw    r3, 0(r8)
  stw    r4, 4(r8)
  stw    r5, 8(r8)
  stw    r6, 12(r8)
  stw    r7, 16(r8)
end;

{$IFDEF SYSCALL_DEBUG}
Const
  DoSysCallDebug : Boolean = False;

var
  LastCnt,
  LastEax,
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
  Revision 1.2  2002-12-22 16:00:28  jonas
    + added syscallh.inc, adapted syscall.inc

  Revision 1.1  2002/11/09 20:32:14  marco
   * powerpc version. Threadsafe errno access not yet done.

  Revision 1.1  2002/10/14 19:39:44  peter
    * syscall moved into seperate include

}
 
