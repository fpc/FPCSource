{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 by the Free Pascal development team.

    These files adds support for TP styled port accesses (port[],
    portw[] and portl[] constructs) using Delphi classes.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Ports;

{ This unit uses classes so ObjFpc mode is required. }
{$Mode ObjFpc}

interface
    
type
 TPort = class
  protected
   procedure WritePort (P: word; Data: byte);
   function ReadPort (P: word): byte;
  public
   property PP [W: word]: byte read readport write writeport; default;
 end;

 TPortW = class
  protected
   procedure WritePort (P: word; Data: word);
   function ReadPort (P: word): word;
  public
   property PP [W: word]: word read readport write writeport; default;
 end;

 TPortL = class
  protected
   procedure WritePort (P: word; Data: longint);
   function ReadPort (P: word): longint;
  public
   property PP [W: word]: longint read readport write writeport; default;
 end;

 { Non-instantiated vars. As yet, they don't have to be instantiated,
   because neither member variables nor virtual methods are accessed }

var
 Port, PortB: TPort;
 PortW: TPortW;
 PortL: TPortL;

implementation

{Import syscall to call it nicely from assembler procedures.}

procedure syscall; external name '___SYSCALL';

{$AsmMode ATT}

procedure TPort.WritePort (P: word; Data: byte); assembler;
asm
 xorl %ecx, %ecx
 movw P, %cx
 movl %ecx, %edx
 movw $0x7F12, %ax
 call syscall
 movw P, %dx
 movb Data, %al
 outb %al, %dx
end;

function TPort.ReadPort (P: word): byte; assembler;
asm
 xorl %ecx, %ecx
 movw P, %cx
 movl %ecx, %edx
 movw $0x7F12, %ax
 call syscall
 movw P, %dx
 inb %dx, %al
end;

procedure TPortW.WritePort (P: word; Data : word); assembler;
asm
 xorl %ecx, %ecx
 movw P, %cx
 movl %ecx, %edx
 movw $0x7F12, %ax
 call syscall
 movw P, %dx
 movw Data, %ax
 outw %ax, %dx
end;

function TPortW.ReadPort (P: word): word; assembler;
asm
 xorl %ecx, %ecx
 movw P, %cx
 movl %ecx, %edx
 movw $0x7F12, %ax
 call syscall
 movw P, %dx
 inw %dx, %ax
end;

procedure TPortL.WritePort (P: word; Data: longint); assembler;
asm
 xorl %ecx, %ecx
 movw P, %cx
 movl %ecx, %edx
 movw $0x7F12, %ax
 call syscall
 movw P, %dx
 movl Data, %eax
 outl %eax, %dx
end;

function TPortL.ReadPort (P: word): longint; assembler;
asm
 xorl %ecx, %ecx
 movw P, %cx
 movl %ecx, %edx
 movw $0x7F12, %ax
 call syscall
 movw P, %dx
 inl %dx, %eax
end;

end.

{
  $Log$
  Revision 1.2  2000-01-02 17:13:12  hajny
    * it might even work now ;-)

  Revision 1.1  2000/01/02 16:05:36  hajny
    + initial OS/2 release

}
