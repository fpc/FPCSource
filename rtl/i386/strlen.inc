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

asm
        movl    p,%edi
        movl    $0xffffffff,%ecx
        xorl    %eax,%eax
        cld
        repne
        scasb
        movl    $0xfffffffe,%eax
        subl    %ecx,%eax
end ['EDI','ECX','EAX']; 


{
  $Log$
  Revision 1.2  2001-03-05 17:10:04  jonas
    * moved implementations of strlen and strpas to separate include files
      (they were duplicated in i386.inc and strings.inc/stringss.inc)
    * strpas supports 'nil' pchars again (returns an empty string)
      (both merged)

  Revision 1.1.2.1  2001/03/05 16:56:01  jonas
    * moved implementations of strlen and strpas to separate include files
      (they were duplicated in i386.inc and strings.inc/stringss.inc)
    * strpas supports 'nil' pchars again (returns an empty string)

}
