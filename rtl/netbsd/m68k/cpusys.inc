{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    CPU dependent part of syscall for NetBSD

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


procedure actualsyscall; assembler;{$ifdef TEST_INLINE}inline;{$endif TEST_INLINE}
    asm
         trap  #0
         bcs   @LErrorcode
         clr.l d1
         rts
@LErrorcode:
         move.l d0,d1
         move.l #-1,d0
   end;


function Do_SysCall(sysnr:LONGINT):longint; assembler;

asm
  move.l  sysnr,d0
  bsr     actualsyscall
  move.l  d1,Errno
end;

function Do_SysCall(sysnr,param1:longint):longint; assembler;

 asm
  move.l Param1,-(sp)
  move.l sysnr,d0
  bsr    actualsyscall
  addq.l #4,sp
  move.l d1,Errno
 end;

{ Can this happen, where ?? PM }
function Do_SysCall(sysnr,param1:integer):longint;
(*assembler;
 asm
  movzwl sysnr,%eax
  pushw Param1
  move.l sysnr,-(sp)
  bsr   actualsyscall
  addl  $2,sp
  move.l d1,Errno
 end; *)
begin
  RunError(218);
end;


function Do_SysCall(sysnr,param1,param2:LONGINT):longint; assembler;

 asm
   move.l Param2,-(sp)
   move.l Param1,-(sp)
   move.l sysnr,d0
   bsr    actualsyscall
   add.l  #8,sp
   move.l d1,Errno
 end;

function Do_SysCall(sysnr,param1,param2,param3:LONGINT):longint; assembler;

 asm
   move.l Param3,-(sp)
   move.l Param2,-(sp)
   move.l Param1,-(sp)
   move.l sysnr,d0
   bsr    actualsyscall
   add.l  #12,sp
   move.l d1,Errno
 end;

function Do_SysCall(sysnr,param1,param2,param3,param4:LONGINT):longint; assembler;

asm
   move.l Param4,-(sp)
   move.l Param3,-(sp)
   move.l Param2,-(sp)
   move.l Param1,-(sp)
   move.l sysnr,d0
   bsr    actualsyscall
   add.l  #16,sp
   move.l d1,Errno
end;


function Do_SysCall(sysnr,param1,param2,param3,param4,param5:LONGINT):longint;  assembler;

 asm
   move.l Param5,-(sp)
   move.l Param4,-(sp)
   move.l Param3,-(sp)
   move.l Param2,-(sp)
   move.l Param1,-(sp)
   move.l sysnr,d0
   bsr    actualsyscall
   add.l  #20,sp
   move.l d1,Errno
 end;

function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6:LONGINT):longint;  assembler;

asm
   move.l Param6,-(sp)
   move.l Param5,-(sp)
   move.l Param4,-(sp)
   move.l Param3,-(sp)
   move.l Param2,-(sp)
   move.l Param1,-(sp)
   move.l sysnr,d0
   bsr    actualsyscall
   add.l  #24,sp
   move.l d1,Errno
end;


function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6,param7:LONGINT):longint;  assembler;

asm
   move.l Param7,-(sp)
   move.l Param6,-(sp)
   move.l Param5,-(sp)
   move.l Param4,-(sp)
   move.l Param3,-(sp)
   move.l Param2,-(sp)
   move.l Param1,-(sp)
   move.l sysnr,d0
   bsr    actualsyscall
   add.l  #28,sp
   move.l d1,Errno
end;

procedure minimalsyscall; assembler;{$ifdef TEST_INLINE}inline;{$endif TEST_INLINE}
   asm
         trap  #0
   end;

Function Sys_Lseek(F:longint;Off:longint;Whence:longint): int64; assembler;
{this one is special for the return value being 64-bit..}
{ syscall: "lseek" ret: "off_t" args: "int" "int" "off_t" "int"  }
 asm
  move.l Whence,-(sp)
  move.l Off,-(sp)   //low word of offset
  pea    (0)         // high word of offset
  pea    (0)
  move.l F,-(sp)
  move.l #syscall_nr_lseek,d0
  bsr    minimalsyscall
  add.l  #20,sp
  bcs   @LLseekErrorcode
  bsr   @LLseekEnd
@LLseekErrorcode:
  move.l d0,Errno
  move.l #-1,d0  // set d0 and d1 to -1
  move.l #-1,d1
@LLseekend:
 end;

{
 $Log$
 Revision 1.1.2.2  2001-09-12 23:26:14  pierre
  * lseek code hopefully fixed

 Revision 1.1.2.1  2001/08/10 11:00:59  pierre
  first m68k netbsd files

}
