{
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
{$WARNING Still using EMX - has to be updated once native linking available!}


{$AsmMode ATT}

procedure TPort.WritePort (P: word; Data: byte); assembler;
asm
 xorl %ecx, %ecx
{$IFDEF REGCALL}
 movw %ax, %cx
 pushl %edx
 pushl %ecx
{$ELSE REGCALL}
 movw P, %cx
{$ENDIF REGCALL}
 movl %ecx, %edx
 movw $0x7F12, %ax
 call syscall
{$IFDEF REGCALL}
 popl %edx
 popl %eax
{$ELSE REGCALL}
 movw P, %dx
 movb Data, %al
{$ENDIF REGCALL}
 outb %al, %dx
end {['eax', 'ecx', 'edx']};

function TPort.ReadPort (P: word): byte; assembler;
asm
 xorl %ecx, %ecx
{$IFDEF REGCALL}
 movw %ax, %cx
{$ELSE REGCALL}
 movw P, %cx
 pushl %ecx
{$ENDIF REGCALL}
 movl %ecx, %edx
 movw $0x7F12, %ax
 call syscall
{$IFDEF REGCALL}
 popl %edx
{$ELSE REGCALL}
 movw P, %dx
{$ENDIF REGCALL}
 inb %dx, %al
end {['eax', 'ecx', 'edx']};

procedure TPortW.WritePort (P: word; Data : word); assembler;
asm
 xorl %ecx, %ecx
{$IFDEF REGCALL}
 movw %ax, %cx
 pushl %edx
 pushl %ecx
{$ELSE REGCALL}
 movw P, %cx
{$ENDIF REGCALL}
 movl %ecx, %edx
 movw $0x7F12, %ax
 call syscall
{$IFDEF REGCALL}
 popl %edx
 popl %eax
{$ELSE REGCALL}
 movw P, %dx
 movw Data, %ax
{$ENDIF REGCALL}
 outw %ax, %dx
end {['eax', 'ecx', 'edx']};

function TPortW.ReadPort (P: word): word; assembler;
asm
 xorl %ecx, %ecx
{$IFDEF REGCALL}
 movw %ax, %cx
 pushl %ecx
{$ELSE REGCALL}
 movw P, %cx
{$ENDIF REGCALL}
 movl %ecx, %edx
 movw $0x7F12, %ax
 call syscall
{$IFDEF REGCALL}
 popl %edx
{$ELSE REGCALL}
 movw P, %dx
{$ENDIF REGCALL}
 inw %dx, %ax
end {['eax', 'ecx', 'edx']};

procedure TPortL.WritePort (P: word; Data: longint); assembler;
asm
 xorl %ecx, %ecx
{$IFDEF REGCALL}
 movw %ax, %cx
 pushl %edx
 pushl %ecx
{$ELSE REGCALL}
 movw P, %cx
{$ENDIF REGCALL}
 movl %ecx, %edx
 movw $0x7F12, %ax
 call syscall
{$IFDEF REGCALL}
 popl %edx
 popl %eax
{$ELSE REGCALL}
 movw P, %dx
 movl Data, %eax
{$ENDIF REGCALL}
 outl %eax, %dx
end {['eax', 'ecx', 'edx']};

function TPortL.ReadPort (P: word): longint; assembler;
asm
 xorl %ecx, %ecx
{$IFDEF REGCALL}
 movw %ax, %cx
 pushl %ecx
{$ELSE REGCALL}
 movw P, %cx
{$ENDIF REGCALL}
 movl %ecx, %edx
 movw $0x7F12, %ax
 call syscall
{$IFDEF REGCALL}
 popl %edx
{$ELSE REGCALL}
 movw P, %dx
{$ENDIF REGCALL}
 inl %dx, %eax
end {['eax', 'ecx', 'edx']};

end.
