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
{Use direct assembler, dumps directly assembler code in sources}
{$ASMMODE DIRECT}
{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

function FpSysCall(sysnr:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL0'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        or      %i0,%g0,%g1
        ta      0x10
        bcc     .LSyscOK0
        nop
        sethi   %hi(FPC_THREADVAR_RELOCATE),%o0
        or      %o0,%lo(FPC_THREADVAR_RELOCATE),%o0
        ld      [%o0],%o7
        subcc   %o7,%g0,%g0
        bne     .LThread0
        nop
        sethi   %hi(U_SYSTEM_ERRNO+4),%o0
        ba      .LNoThread0
        or      %o0,%lo(U_SYSTEM_ERRNO+4),%o0
.LThread0:
        sethi   %hi(U_SYSTEM_ERRNO),%o0
        ld      [%o7],%o1
        or      %o0,%lo(U_SYSTEM_ERRNO),%o0
        call    %o1
.LNoThread0:
        st      %i0,[%o0]
        mov     -1,%o0
.LSyscOK0:
        mov     %o0,%i0
end;


function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        or      %i0,%g0,%g1
        or      %i1,%g0,%o0
        ta      0x10
        bcc     .LSyscOK1
        nop
        sethi   %hi(FPC_THREADVAR_RELOCATE),%o0
        or      %o0,%lo(FPC_THREADVAR_RELOCATE),%o0
        ld      [%o0],%o7
        subcc   %o7,%g0,%g0
        bne     .LThread1
        nop
        sethi   %hi(U_SYSTEM_ERRNO+4),%o0
        ba      .LNoThread1
        or      %o0,%lo(U_SYSTEM_ERRNO+4),%o0
.LThread1:
        sethi   %hi(U_SYSTEM_ERRNO),%o0
        ld      [%o7],%o1
        or      %o0,%lo(U_SYSTEM_ERRNO),%o0
        call    %o1
.LNoThread1:
        st      %i0,[%o0]
        mov     -1,%o0
.LSyscOK1:
        mov     %o0,%i0
end;


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        or      %i0,%g0,%g1
        or      %i1,%g0,%o0
        or      %i2,%g0,%o1
        ta      0x10
        bcc     .LSyscOK2
        nop
        sethi   %hi(FPC_THREADVAR_RELOCATE),%o0
        or      %o0,%lo(FPC_THREADVAR_RELOCATE),%o0
        ld      [%o0],%o7
        subcc   %o7,%g0,%g0
        bne     .LThread2
        nop
        sethi   %hi(U_SYSTEM_ERRNO+4),%o0
        ba      .LNoThread2
        or      %o0,%lo(U_SYSTEM_ERRNO+4),%o0
.LThread2:
        sethi   %hi(U_SYSTEM_ERRNO),%o0
        ld      [%o7],%o1
        or      %o0,%lo(U_SYSTEM_ERRNO),%o0
        call    %o1
.LNoThread2:
        st      %i0,[%o0]
        mov     -1,%o0
.LSyscOK2:
        mov     %o0,%i0
end;


function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        or      %i0,%g0,%g1
        or      %i1,%g0,%o0
        or      %i2,%g0,%o1
        or      %i3,%g0,%o2
        ta      0x10
        bcc     .LSyscOK3
        nop
        sethi   %hi(FPC_THREADVAR_RELOCATE),%o0
        or      %o0,%lo(FPC_THREADVAR_RELOCATE),%o0
        ld      [%o0],%o7
        subcc   %o7,%g0,%g0
        bne     .LThread3
        nop
        sethi   %hi(U_SYSTEM_ERRNO+4),%o0
        ba      .LNoThread3
        or      %o0,%lo(U_SYSTEM_ERRNO+4),%o0
.LThread3:
        sethi   %hi(U_SYSTEM_ERRNO),%o0
        ld      [%o7],%o1
        or      %o0,%lo(U_SYSTEM_ERRNO),%o0
        call    %o1
.LNoThread3:
        st      %i0,[%o0]
        mov     -1,%o0
.LSyscOK3:
        mov     %o0,%i0
end;


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        or      %i0,%g0,%g1
        or      %i1,%g0,%o0
        or      %i2,%g0,%o1
        or      %i3,%g0,%o2
        or      %i4,%g0,%o3
        ta      0x10
        bcc     .LSyscOK4
        nop
        sethi   %hi(FPC_THREADVAR_RELOCATE),%o0
        or      %o0,%lo(FPC_THREADVAR_RELOCATE),%o0
        ld      [%o0],%o7
        subcc   %o7,%g0,%g0
        bne     .LThread4
        nop
        sethi   %hi(U_SYSTEM_ERRNO+4),%o0
        ba      .LNoThread4
        or      %o0,%lo(U_SYSTEM_ERRNO+4),%o0
.LThread4:
        sethi   %hi(U_SYSTEM_ERRNO),%o0
        ld      [%o7],%o1
        or      %o0,%lo(U_SYSTEM_ERRNO),%o0
        call    %o1
.LNoThread4:
        st      %i0,[%o0]
        mov     -1,%o0
.LSyscOK4:
        mov     %o0,%i0
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        or      %i0,%g0,%g1
        or      %i1,%g0,%o0
        or      %i2,%g0,%o1
        or      %i3,%g0,%o2
        or      %i4,%g0,%o3
        or      %i5,%g0,%o4
        ta      0x10
        bcc     .LSyscOK5
        nop
        sethi   %hi(FPC_THREADVAR_RELOCATE),%o0
        or      %o0,%lo(FPC_THREADVAR_RELOCATE),%o0
        ld      [%o0],%o7
        subcc   %o7,%g0,%g0
        bne     .LThread5
        nop
        sethi   %hi(U_SYSTEM_ERRNO+4),%o0
        ba      .LNoThread5
        or      %o0,%lo(U_SYSTEM_ERRNO+4),%o0
.LThread5:
        sethi   %hi(U_SYSTEM_ERRNO),%o0
        ld      [%o7],%o1
        or      %o0,%lo(U_SYSTEM_ERRNO),%o0
        call    %o1
.LNoThread5:
        st      %i0,[%o0]
        mov     -1,%o0
.LSyscOK5:
        mov     %o0,%i0
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        or      %i0,%g0,%g1
        or      %i1,%g0,%o0
        or      %i2,%g0,%o1
        or      %i3,%g0,%o2
        or      %i4,%g0,%o3
        ld      [%i6+92],%o5
        or      %i5,%g0,%o4
        ta      0x10
        bcc     .LSyscOK6
        nop
        sethi   %hi(FPC_THREADVAR_RELOCATE),%o0
        or      %o0,%lo(FPC_THREADVAR_RELOCATE),%o0
        ld      [%o0],%o7
        subcc   %o7,%g0,%g0
        bne     .LThread6
        nop
        sethi   %hi(U_SYSTEM_ERRNO+4),%o0
        ba      .LNoThread6
        or      %o0,%lo(U_SYSTEM_ERRNO+4),%o0
.LThread6:
        sethi   %hi(U_SYSTEM_ERRNO),%o0
        ld      [%o7],%o1
        or      %o0,%lo(U_SYSTEM_ERRNO),%o0
        call    %o1
.LNoThread6:
        st      %i0,[%o0]
        mov     -1,%o0
.LSyscOK6:
        mov     %o0,%i0
end;


// Old style syscall:
// Better use ktrace/strace/gdb for debugging.

Procedure FpSysCall( callnr:longint;var regs : SysCallregs );assembler;
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  or   %i0,%g0,%g1
  ld   [%i1],%o0
  ld   [%i1+4],%o1
  ld   [%i1+8],%o2
  ld   [%i1+12],%o3
  ld   [%i1+16],%o4
{ Go ! }
  ta   0x10
{ Put back the registers... }
  st   %o0,[%i1]
  st   %o1,[%i1+4]
  st   %o2,[%i1+8]
  st   %o3,[%i1+12]
  st   %o4,[%i1+16]
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
  FpSysCall(callnr,regs);
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
  Revision 1.9  2003-08-11 13:19:08  mazen
  + added assembler mode directive to use direct assembler reader

  Revision 1.8  2003/07/06 22:08:05  peter
    * fix setting return value

  Revision 1.7  2003/07/06 20:40:10  peter
    * wrong return reg

  Revision 1.6  2003/07/03 21:03:57  peter
    * syscalls implemented

  Revision 1.5  2003/06/02 22:05:03  mazen
  * fixing naming conflict in public clause of
   FPC_SYSCALL? definition

  Revision 1.4  2003/05/23 22:36:39  florian
    * fixed compilation of sparc system unit

  Revision 1.3  2003/01/05 21:32:35  mazen
  * fixing several bugs compiling the RTL

  Revision 1.2  2002/12/24 21:30:20  mazen
  - some writeln(s) removed in compiler
  + many files added to RTL
  * some errors fixed in RTL

  Revision 1.1  2002/11/15 12:08:37  mazen
  + SPARC support added based on PowerPc sources

  Revision 1.1  2002/11/09 20:32:14  marco
   * powerpc version. Threadsafe errno access not yet done.

  Revision 1.1  2002/10/14 19:39:44  peter
    * syscall moved into seperate include

}

