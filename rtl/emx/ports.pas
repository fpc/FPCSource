{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    These files adds support for TP styled port accesses (port[],
    portw[] and portl[] constructs) using Delphi classes.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

(*
  Warning:
  1) You have to enable port access in your CONFIG.SYS (IOPL directive),
     either globally (IOPL=YES), or just for particular application/-s with
     a need for port access (IOPL=app_name1, appname2, ...).
  2) Once you access some port, access to this port is enabled all the time
     for all EMX applications until EMX.DLL is unloaded from memory (i.e.
     all applications using this library finish).
*)
    
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
  Revision 1.1  2002-11-17 16:22:54  hajny
    + RTL for emx target

  Revision 1.2  2002/09/07 16:01:25  peter
    * old logs removed and tabs fixed

}
