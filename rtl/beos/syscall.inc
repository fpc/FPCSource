{
    $Id: syscall.inc,v 1.2 2005/02/14 17:13:21 peter Exp $
    Copyright (c) 1998-2000 by Florian Klaempfl

    This include implements the actual system call for the
    intel BeOS 80x86 platform.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

type
     SysCallArgs = packed record
       param: array[1..8] of cint;
     End;


procedure sys_call; external name 'sys_call';



function Do_SysCall( callnr:longint;var regs : SysCallArgs ): longint;assembler;
{
  This routine sets up the parameters on the stack, all the parameters
  are in reverse order on the stack (like C parameter passing).
}
asm
  { load the parameters... }
  movl  regs,%eax
  movl  24(%eax),%ebx
  pushl %ebx
  movl  20(%eax),%ebx
  pushl %ebx
  movl  16(%eax),%ebx
  pushl %ebx
  movl  12(%eax),%ebx
  pushl %ebx
  movl  8(%eax),%ebx
  pushl %ebx
  movl  4(%eax),%ebx
  pushl %ebx
  movl  0(%eax),%ebx
  pushl %ebx
  { set the call number }
  movl  callnr,%eax
  call  sys_call
  addl  $28,%esp
end;


Function SysCall( callnr:longint;var args : SysCallArgs ):longint;
{
  This function serves as an interface to do_SysCall.
  If the SysCall returned a negative number, it returns -1, and puts the
  SysCall result in errno. Otherwise, it returns the SysCall return value
}
var
 funcresult : longint;
begin
  funcresult:=do_SysCall(callnr,args);
  if funcresult<0 then
   begin
     ErrNo:=funcresult;
     SysCall:=-1;
   end
  else
   begin
     SysCall:=funcresult;
     errno:=0
   end;
end;


{
  $Log: syscall.inc,v $
  Revision 1.2  2005/02/14 17:13:21  peter
    * truncate log

}
