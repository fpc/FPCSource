{
  This file is part of the Free Pascal run time library.

  Perform syscall with 0..6 arguments.
  If syscall return value is negative, negate it, set errno, and return -1.

  Written by Edmund Grimley Evans in 2015 and released into the public domain.
}

function FpSysCall(sysnr:TSysParam):TSysResult;
assembler; nostackframe; [public,alias:'FPC_SYSCALL0'];
asm
  ori $a7, $a0, 0
  syscall 0
  bge $a0, $zero, .Ldone
  addi.d $sp, $sp, -16
  st.d $ra, $sp, 8
  sub.d $a0, $zero, $a0
  bl seterrno
  addi.d $a0, $zero, -1
  ld.d $ra, $sp, 8
  addi.d $sp, $sp, 16
.Ldone:
end;

function FpSysCall(sysnr,param1:TSysParam):TSysResult;
assembler; nostackframe; [public,alias:'FPC_SYSCALL1'];
asm
  ori $a7, $a0, 0
  ori $a0, $a1, 0
  syscall 0
  bge $a0, $zero, .Ldone
  addi.d $sp, $sp, -16
  st.d $ra, $sp, 8
  sub.d $a0, $zero, $a0
  bl seterrno
  addi.d $a0, $zero, -1
  ld.d $ra, $sp, 8
  addi.d $sp, $sp, 16
.Ldone:
end;

function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult;
assembler; nostackframe; [public,alias:'FPC_SYSCALL2'];
asm
  ori $a7, $a0, 0
  ori $a0, $a1, 0
  ori $a1, $a2, 0
  syscall 0
  bge $a0, $zero, .Ldone
  addi.d $sp, $sp, -16
  st.d $ra, $sp, 8
  sub.d $a0, $zero, $a0
  bl seterrno
  addi.d $a0, $zero, -1
  ld.d $ra, $sp, 8
  addi.d $sp, $sp, 16
.Ldone:
end;

function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult;
assembler; nostackframe; [public,alias:'FPC_SYSCALL3'];
asm
  ori $a7, $a0, 0
  ori $a0, $a1, 0
  ori $a1, $a2, 0
  ori $a2, $a3, 0
  syscall 0
  bge $a0, $zero, .Ldone
  addi.d $sp, $sp, -16
  st.d $ra, $sp, 8
  sub.d $a0, $zero, $a0
  bl seterrno
  addi.d $a0, $zero, -1
  ld.d $ra, $sp, 8
  addi.d $sp, $sp, 16
.Ldone:
end;

function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult;
assembler; nostackframe; [public,alias:'FPC_SYSCALL4'];
asm
  ori $a7, $a0, 0
  ori $a0, $a1, 0
  ori $a1, $a2, 0
  ori $a2, $a3, 0
  ori $a3, $a4, 0
  syscall 0
  bge $a0, $zero, .Ldone
  addi.d $sp, $sp, -16
  st.d $ra, $sp, 8
  sub.d $a0, $zero, $a0
  bl seterrno
  addi.d $a0, $zero, -1
  ld.d $ra, $sp, 8
  addi.d $sp, $sp, 16
.Ldone:
end;

function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult;
assembler; nostackframe; [public,alias:'FPC_SYSCALL5'];
asm
  ori $a7, $a0, 0
  ori $a0, $a1, 0
  ori $a1, $a2, 0
  ori $a2, $a3, 0
  ori $a3, $a4, 0
  ori $a4, $a5, 0
  syscall 0
  bge $a0, $zero, .Ldone
  addi.d $sp, $sp, -16
  st.d $ra, $sp, 8
  sub.d $a0, $zero, $a0
  bl seterrno
  addi.d $a0, $zero, -1
  ld.d $ra, $sp, 8
  addi.d $sp, $sp, 16
.Ldone:
end;

function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult;
assembler; nostackframe; [public,alias:'FPC_SYSCALL6'];
asm
  ori $a7, $a0, 0
  ori $a0, $a1, 0
  ori $a1, $a2, 0
  ori $a2, $a3, 0
  ori $a3, $a4, 0
  ori $a4, $a5, 0
  ori $a5, $a6, 0
  syscall 0
  bge $a0, $zero, .Ldone
  addi.d $sp, $sp, -16
  st.d $ra, $sp, 8
  sub.d $a0, $zero, $a0
  bl seterrno
  addi.d $a0, $zero, -1
  ld.d $ra, $sp, 8
  addi.d $sp, $sp, 16
.Ldone:
end;
