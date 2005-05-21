{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1997-2004 by the Free Pascal development team

    Some x86 specific stuff. Has to be fixed still for *BSD

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY;without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit x86;

interface

Uses BaseUnix;

function ReadPortB (Port : Longint): Byte;
function ReadPortW (Port : Longint): Word;
function ReadPortL (Port : Longint): Longint;
Procedure ReadPort (Port : Longint; Var Value : Byte);
Procedure ReadPort (Port : Longint; Var Value : Longint);
Procedure ReadPort (Port : Longint; Var Value : Word);
Procedure ReadPortB (Port : Longint; Var Buf; Count: longint);
Procedure ReadPortL (Port : Longint; Var Buf; Count: longint);
Procedure ReadPortW (Port : Longint; Var Buf; Count: longint);
Procedure WritePort (Port : Longint; Value : Byte);
Procedure WritePort (Port : Longint; Value : Longint);
Procedure WritePort (Port : Longint; Value : Word);
Procedure WritePortB (Port : Longint; Value : Byte);
Procedure WritePortB (Port : Longint; Var Buf; Count: longint);
Procedure WritePortL (Port : Longint; Value : Longint);
Procedure WritePortW (Port : Longint; Value : Word);
Procedure WritePortW (Port : Longint; Var Buf; Count: longint);
Procedure WritePortl (Port : Longint; Var Buf; Count: longint);

Function  fpIOperm (From,Num : Cardinal; Value : cint) : cint;
Function  fpIoPL(Level : cint) : cint;

implementation
{$ASMMODE ATT}

Uses Syscall;

Procedure WritePort (Port : Longint; Value : Byte);
{
  Writes 'Value' to port 'Port'
}
begin
        asm
        movl port,%edx
        movb value,%al
        outb %al,%dx
        end ['EAX','EDX'];
end;

Procedure WritePort (Port : Longint; Value : Word);
{
  Writes 'Value' to port 'Port'
}

begin
        asm
        movl port,%edx
        movw value,%ax
        outw %ax,%dx
        end ['EAX','EDX'];
end;



Procedure WritePort (Port : Longint; Value : Longint);
{
  Writes 'Value' to port 'Port'
}

begin
        asm
        movl port,%edx
        movl value,%eax
        outl %eax,%dx
        end ['EAX','EDX'];
end;


Procedure WritePortB (Port : Longint; Value : Byte);
{
  Writes 'Value' to port 'Port'
}
begin
        asm
        movl port,%edx
        movb value,%al
        outb %al,%dx
        end ['EAX','EDX'];
end;

Procedure WritePortW (Port : Longint; Value : Word);
{
  Writes 'Value' to port 'Port'
}

begin
        asm
        movl port,%edx
        movw value,%ax
        outw %ax,%dx
        end ['EAX','EDX'];
end;



Procedure WritePortL (Port : Longint; Value : Longint);
{
  Writes 'Value' to port 'Port'
}

begin
        asm
        movl port,%edx
        movl value,%eax
        outl %eax,%dx
        end ['EAX','EDX'];
end;



Procedure WritePortl (Port : Longint; Var Buf; Count: longint);
{
  Writes 'Count' longints from 'Buf' to Port
}
begin
  asm
        movl count,%ecx
        movl buf,%esi
        movl port,%edx
        cld
        rep
        outsl
  end ['ECX','ESI','EDX'];
end;



Procedure WritePortW (Port : Longint; Var Buf; Count: longint);
{
  Writes 'Count' words from 'Buf' to Port
}
begin
  asm
        movl count,%ecx
        movl buf,%esi
        movl port,%edx
        cld
        rep
        outsw
  end ['ECX','ESI','EDX'];
end;



Procedure WritePortB (Port : Longint; Var Buf; Count: longint);
{
  Writes 'Count' bytes from 'Buf' to Port
}
begin
  asm
        movl count,%ecx
        movl buf,%esi
        movl port,%edx
        cld
        rep
        outsb
  end ['ECX','ESI','EDX'];
end;



Procedure ReadPort (Port : Longint; Var Value : Byte);
{
  Reads 'Value' from port 'Port'
}
begin
        asm
        movl port,%edx
        inb %dx,%al
        movl value,%edx
        movb %al,(%edx)
        end ['EAX','EDX'];
end;



Procedure ReadPort (Port : Longint; Var Value : Word);
{
  Reads 'Value' from port 'Port'
}
begin
        asm
        movl port,%edx
        inw %dx,%ax
        movl value,%edx
        movw %ax,(%edx)
        end ['EAX','EDX'];
end;



Procedure ReadPort (Port : Longint; Var Value : Longint);
{
  Reads 'Value' from port 'Port'
}
begin
        asm
        movl port,%edx
        inl %dx,%eax
        movl value,%edx
        movl %eax,(%edx)
        end ['EAX','EDX'];
end;



function ReadPortB (Port : Longint): Byte; assembler;
{
  Reads a byte from port 'Port'
}

asm
  movl port,%edx
  xorl %eax,%eax
  inb %dx,%al
end ['EAX','EDX'];

function ReadPortW (Port : Longint): Word; assembler;
{
  Reads a word from port 'Port'
}
asm
  movl port,%edx
  xorl %eax,%eax
  inw %dx,%ax
end ['EAX','EDX'];

function ReadPortL (Port : Longint): LongInt; assembler;
{
  Reads a LongInt from port 'Port'
}
asm
  movl port,%edx
  inl %dx,%eax
end ['EAX','EDX'];

Procedure ReadPortL (Port : Longint; Var Buf; Count: longint);
{
  Reads 'Count' longints from port 'Port' to 'Buf'.
}
begin
  asm
        movl count,%ecx
        movl buf,%edi
        movl port,%edx
        cld
        rep
        insl
  end ['ECX','EDI','EDX'];
end;



Procedure ReadPortW (Port : Longint; Var Buf; Count: longint);
{
  Reads 'Count' words from port 'Port' to 'Buf'.
}
begin
  asm
        movl count,%ecx
        movl buf,%edi
        movl port,%edx
        cld
        rep
        insw
  end ['ECX','EDI','EDX'];
end;

Procedure ReadPortB (Port : Longint; Var Buf; Count: longint);
{
  Reads 'Count' bytes from port 'Port' to 'Buf'.
}
begin
  asm
        movl count,%ecx
        movl buf,%edi
        movl port,%edx
        cld
        rep
        insb
  end ['ECX','EDI','EDX'];
end;

{$ifdef linux}
Function  fpIOperm (From,Num : Cardinal; Value : cint) : cint;
{
  Set permissions on NUM ports starting with port FROM to VALUE
  this works ONLY as root.
}

begin
  fpIOPerm:=do_Syscall(Syscall_nr_ioperm,TSysParam(From),TSysParam(Num),TSysParam(Value));
end;
{$else}


{$packrecords C}

TYPE uint=CARDINAL;

CONST
        I386_GET_LDT    =0;
        I386_SET_LDT    =1;
                                { I386_IOPL }
        I386_GET_IOPERM =3;
        I386_SET_IOPERM =4;
                                { xxxxx }
        I386_VM86       =6;


type

{ i386_ldt_args = record
        int     start : longint;
        union   descriptor *descs;
        int     num;
        end;
}

 i386_ioperm_args = record
        start    : cuint;
        length   : cuint;
        enable   : cint;
        end;


    i386_vm86_args = record
        sub_op   : cint;             { sub-operation to perform }
        sub_args : pchar;               { args }
        end;

   sysarch_args     = record
                        op    : longint;
                        parms : pchar;
                       end;

Function fpIOPerm(From,Num:CARDINAL;Value:cint):cint;

var sg : i386_ioperm_args;
    sa : sysarch_args;

begin
  sg.start:=From;
  sg.length:=Num;
  sg.enable:=value;
  sa.op:=i386_SET_IOPERM;
  sa.parms:=@sg;
  fpIOPerm:=do_syscall(syscall_nr_sysarch,TSysParam(@sa));
end;
{$endif}

Function fpIoPL(Level : cint) : cint;

begin
 {$ifdef Linux}
  fpIOPL:=do_Syscall(Syscall_nr_iopl,TSysParam(Level));
 {$endif}
end;


end.
