{
    $Id$
    This file is part of the Free Pascal run time library.
    and implements some stuff for protected mode programming
    Copyright (c) 1998-2000 by the Free Pascal development team.

    These files adds support for TP styled port accesses
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit ports;

{ this unit uses classes so
  ObjFpc mode is required PM }
{$Mode ObjFpc}

interface
    
type
   tport = class
      procedure writeport(p : word;data : byte);
      function  readport(p : word) : byte;
      property pp[w : word] : byte read readport write writeport;default;
   end;

   tportw = class
      procedure writeport(p : word;data : word);
      function  readport(p : word) : word;
      property pp[w : word] : word read readport write writeport;default;
   end;

   tportl = class
      procedure writeport(p : word;data : longint);
      function  readport(p : word) : longint;
      property pp[w : word] : longint read readport write writeport;default;
   end;
var
{ we don't need to initialize port, because neither member
  variables nor virtual methods are accessed }
   port,
   portb : tport;
   portw : tportw;
   portl : tportl;

  implementation

{$asmmode ATT}

{ to give easy port access like tp with port[] }

procedure tport.writeport(p : word;data : byte);assembler;
asm
        movw    p,%dx
        movb    data,%al
        outb    %al,%dx
end ['EAX','EDX'];


function tport.readport(p : word) : byte;assembler;
asm
        movw    p,%dx
        inb     %dx,%al
end ['EAX','EDX'];


procedure tportw.writeport(p : word;data : word);assembler;
asm
        movw    p,%dx
        movw    data,%ax
        outw    %ax,%dx
end ['EAX','EDX'];


function tportw.readport(p : word) : word;assembler;
asm
        movw    p,%dx
        inw     %dx,%ax
end ['EAX','EDX'];


procedure tportl.writeport(p : word;data : longint);assembler;
asm
        movw    p,%dx
        movl    data,%eax
        outl    %eax,%dx
end ['EAX','EDX'];


function tportl.readport(p : word) : longint;assembler;
asm
        movw    p,%dx
        inl     %dx,%eax
end ['EAX','EDX'];

end.

{
  $Log$
  Revision 1.2  2000-01-07 16:32:23  daniel
    * copyright 2000 added

  Revision 1.1  1999/09/01 14:47:31  pierre
   TP port construction separated into this unit

  Revision 1.4  1999/05/13 21:54:27  peter
    * objpas fixes

  Revision 1.3  1999/03/26 00:01:52  peter
    * fixed rounding in global_dos_alloc

  Revision 1.2  1999/03/01 15:40:51  peter
    * use external names
    * removed all direct assembler modes

  Revision 1.1  1998/12/21 13:07:03  peter
    * use -FE

  Revision 1.12  1998/08/27 10:30:50  pierre
    * go32v1 RTL did not compile (LFNsupport outside go32v2 defines !)
      I renamed tb_selector to tb_segment because
        it is a real mode segment as opposed to
        a protected mode selector
      Fixed it for go32v1 (remove the $E0000000 offset !)

  Revision 1.11  1998/08/26 10:04:02  peter
    * new lfn check from mailinglist
    * renamed win95 -> LFNSupport
    + tb_selector, tb_offset for easier access to transferbuffer

  Revision 1.10  1998/08/11 00:07:17  peter
    * $ifdef ver0_99_5 instead of has_property

  Revision 1.9  1998/07/21 12:06:03  carl
    * restored working version
}
