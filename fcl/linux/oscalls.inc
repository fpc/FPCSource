{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  Implementation of some system calls.
}

Type SysCallRegs = Record
       reg1,reg2,reg3,reg4,reg5,reg6 : Longint;
     end;


Const
  { Needed call numbers }
  syscall_nr_read      = 3; 
  syscall_nr_write     = 4;
  syscall_nr_open      = 5;
  syscall_nr_close     = 6;
  syscall_nr_lseek     = 19;
  syscall_nr_ftruncate = 93;


  { Things for OPEN call - after linux/fcntl.h }
  Open_Accmode  = 3;
  Open_RdOnly   = 0;
  Open_WrOnly   = 1;
  Open_RdWr     = 2;
  Open_Creat    = 1 shl 6;
  Open_Excl     = 2 shl 6;
  Open_NoCtty   = 4 shl 6;
  Open_Trunc    = 1 shl 9;
  Open_Append   = 2 shl 9;
  Open_NonBlock = 4 shl 9;
  Open_NDelay   = Open_NonBlock;
  Open_Sync     = 1 shl 12;


Procedure Do_SysCall( callnr:longint;var regs : SysCallregs );assembler;
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
{$ASMMODE ATT}
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
