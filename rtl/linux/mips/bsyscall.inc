{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005-2009 by Michael Van Canneyt and David Zhang
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$define FPC_BASEUNIX_HAS_FPPIPE}
Function fppipe(var fildes : tfildes):cint;assembler;
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
  Extracted from linux/source/arch/mips/kernel/syscall.c:
  * For historic reasons the pipe(2) syscall on MIPS has an unusual calling
  * convention.  It returns results in registers $v0 / $v1 which means there
  * is no need for it to do verify the validity of a userspace pointer
  * argument.  Historically that used to be expensive in Linux.  These days
  * the performance advantage is negligible.
}
asm
  li $v0,syscall_nr_pipe
  syscall
  { the two files descriptors are now in v0 and v1 registers 
    copying them back into fildes variable }
  lw $t1,fildes
  sw $v0,($t1)
  sw $v1,4($t1)
end;
