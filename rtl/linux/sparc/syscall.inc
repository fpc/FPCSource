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

function FpSysCall(sysnr:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL0'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        mov     %i0,%g1
        ta      0x10
        bcc     .LSyscOK
        nop
        sethi   %hi(fpc_threadvar_relocate_proc),%o2
        or      %o2,%lo(fpc_threadvar_relocate_proc),%o2
        ld      [%o2],%o3
        subcc   %o3,%g0,%g0
        bne     .LThread
        nop
        sethi   %hi(Errno+4),%o2
        ba      .LNoThread
        or      %o2,%lo(Errno+4),%o2
.LThread:
        sethi   %hi(Errno),%o0
        ld      [%o3],%o1
        or      %o0,%lo(Errno),%o0
        call    %o1
.LNoThread:
        st      %o0,[%o2]
        mov     -1,%o0
.LSyscOK:
        mov     %o0,%i0
end;


function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        mov     %i0,%g1
        mov     %i1,%o0
        ta      0x10
        bcc     .LSyscOK
        nop
        sethi   %hi(fpc_threadvar_relocate_proc),%o2
        or      %o2,%lo(fpc_threadvar_relocate_proc),%o2
        ld      [%o2],%o3
        subcc   %o3,%g0,%g0
        bne     .LThread
        nop
        sethi   %hi(Errno+4),%o2
        ba      .LNoThread
        or      %o2,%lo(Errno+4),%o2
.LThread:
        sethi   %hi(Errno),%o0
        ld      [%o3],%o1
        or      %o0,%lo(Errno),%o0
        call    %o1
.LNoThread:
        st      %o0,[%o2]
        mov     -1,%o0
.LSyscOK:
        mov     %o0,%i0
end;


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        mov     %i0,%g1
        mov     %i1,%o0
        mov     %i2,%o1
        ta      0x10
        bcc     .LSyscOK
        nop
        sethi   %hi(fpc_threadvar_relocate_proc),%o2
        or      %o2,%lo(fpc_threadvar_relocate_proc),%o2
        ld      [%o2],%o3
        subcc   %o3,%g0,%g0
        bne     .LThread
        nop
        sethi   %hi(Errno+4),%o2
        ba      .LNoThread
        or      %o2,%lo(Errno+4),%o2
.LThread:
        sethi   %hi(Errno),%o0
        ld      [%o3],%o1
        or      %o0,%lo(Errno),%o0
        call    %o1
.LNoThread:
        st      %o0,[%o2]
        mov     -1,%o0
.LSyscOK:
        mov     %o0,%i0
end;


function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        mov     %i0,%g1
        mov     %i1,%o0
        mov     %i2,%o1
        mov     %i3,%o2
        ta      0x10
        bcc     .LSyscOK
        nop
        sethi   %hi(fpc_threadvar_relocate_proc),%o2
        or      %o2,%lo(fpc_threadvar_relocate_proc),%o2
        ld      [%o2],%o3
        subcc   %o3,%g0,%g0
        bne     .LThread
        nop
        sethi   %hi(Errno+4),%o2
        ba      .LNoThread
        or      %o2,%lo(Errno+4),%o2
.LThread:
        sethi   %hi(Errno),%o0
        ld      [%o3],%o1
        or      %o0,%lo(Errno),%o0
        call    %o1
.LNoThread:
        st      %o0,[%o2]
        mov     -1,%o0
.LSyscOK:
        mov     %o0,%i0
end;


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        mov     %i0,%g1
        mov     %i1,%o0
        mov     %i2,%o1
        mov     %i3,%o2
        mov     %i4,%o3
        ta      0x10
        bcc     .LSyscOK
        nop
        sethi   %hi(fpc_threadvar_relocate_proc),%o2
        or      %o2,%lo(fpc_threadvar_relocate_proc),%o2
        ld      [%o2],%o3
        subcc   %o3,%g0,%g0
        bne     .LThread
        nop
        sethi   %hi(Errno+4),%o2
        ba      .LNoThread
        or      %o2,%lo(Errno+4),%o2
.LThread:
        sethi   %hi(Errno),%o0
        ld      [%o3],%o1
        or      %o0,%lo(Errno),%o0
        call    %o1
.LNoThread:
        st      %o0,[%o2]
        mov     -1,%o0
.LSyscOK:
        mov     %o0,%i0
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        mov     %i0,%g1
        mov     %i1,%o0
        mov     %i2,%o1
        mov     %i3,%o2
        mov     %i4,%o3
        mov     %i5,%o4
        ta      0x10
        bcc     .LSyscOK
        nop
        sethi   %hi(fpc_threadvar_relocate_proc),%o2
        or      %o2,%lo(fpc_threadvar_relocate_proc),%o2
        ld      [%o2],%o3
        subcc   %o3,%g0,%g0
        bne     .LThread
        nop
        sethi   %hi(Errno+4),%o2
        ba      .LNoThread
        or      %o2,%lo(Errno+4),%o2
.LThread:
        sethi   %hi(Errno),%o0
        ld      [%o3],%o1
        or      %o0,%lo(Errno),%o0
        call    %o1
.LNoThread:
        st      %o0,[%o2]
        mov     -1,%o0
.LSyscOK:
        mov     %o0,%i0
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        mov     %i0,%g1
        mov     %i1,%o0
        mov     %i2,%o1
        mov     %i3,%o2
        mov     %i4,%o3
        ld      [%i6+92],%o5
        mov     %i5,%o4
        ta      0x10
        bcc     .LSyscOK
        nop
        sethi   %hi(fpc_threadvar_relocate_proc),%o2
        or      %o2,%lo(fpc_threadvar_relocate_proc),%o2
        ld      [%o2],%o3
        subcc   %o3,%g0,%g0
        bne     .LThread
        nop
        sethi   %hi(Errno+4),%o2
        ba      .LNoThread
        or      %o2,%lo(Errno+4),%o2
.LThread:
        sethi   %hi(Errno),%o0
        ld      [%o3],%o1
        or      %o0,%lo(Errno),%o0
        call    %o1
.LNoThread:
        st      %o0,[%o2]
        mov     -1,%o0
.LSyscOK:
        mov     %o0,%i0
end;


{
  $Log$
  Revision 1.13  2004-07-02 19:29:19  peter
    * fixed errno setting

  Revision 1.12  2004/06/30 21:15:51  florian
    * first syscall fix

  Revision 1.11  2004/02/06 23:06:16  florian
    - killed tsyscallregs

  Revision 1.10  2004/01/05 17:22:03  peter
    * removed asmmode direct

  Revision 1.9  2003/08/11 13:19:08  mazen
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
