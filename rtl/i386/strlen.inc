{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Processor specific implementation of strlen

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

var
  saveedi : longint;
asm
	movl	%edi,saveedi
        movl    p,%edi
        movl    $0xffffffff,%ecx
        xorl    %eax,%eax
        cld
        repne
        scasb
        movl    $0xfffffffe,%eax
        subl    %ecx,%eax
	movl	saveedi,%edi
end;


{
  $Log$
  Revision 1.4  2003-09-08 18:21:37  peter
    * save edi,esi,ebx

  Revision 1.3  2002/09/07 16:01:19  peter
    * old logs removed and tabs fixed

}
