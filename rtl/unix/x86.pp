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

{$IFNDEF FPC_DOTTEDUNITS}
unit x86;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$inline on}

{$IFDEF FPC_DOTTEDUNITS}
Uses UnixApi.Base;
{$ELSE FPC_DOTTEDUNITS}
Uses BaseUnix;
{$ENDIF FPC_DOTTEDUNITS}

function ReadPortB (Port : Longint): Byte;inline;
function ReadPortW (Port : Longint): Word;inline;
function ReadPortL (Port : Longint): Longint;inline;
Procedure ReadPort (Port : Longint; Var Value : Byte);inline;
Procedure ReadPort (Port : Longint; Var Value : Longint);inline;
Procedure ReadPort (Port : Longint; Var Value : Word);inline;
Procedure ReadPortB (Port : Longint; Var Buf; Count: longint);
Procedure ReadPortL (Port : Longint; Var Buf; Count: longint);
Procedure ReadPortW (Port : Longint; Var Buf; Count: longint);
Procedure WritePort (Port : Longint; Value : Byte);inline;
Procedure WritePort (Port : Longint; Value : Longint);inline;
Procedure WritePort (Port : Longint; Value : Word);inline;
Procedure WritePortB (Port : Longint; Value : Byte);inline;
Procedure WritePortB (Port : Longint; Var Buf; Count: longint);
Procedure WritePortL (Port : Longint; Value : Longint);inline;
Procedure WritePortW (Port : Longint; Value : Word);inline;
Procedure WritePortW (Port : Longint; Var Buf; Count: longint);
Procedure WritePortl (Port : Longint; Var Buf; Count: longint);

Function  fpIOperm (From,Num : Cardinal; Value : cint) : cint;
Function  fpIoPL(Level : cint) : cint;

implementation
{$ASMMODE ATT}

{$IFDEF FPC_DOTTEDUNITS}
Uses UnixApi.SysCall;
{$ELSE FPC_DOTTEDUNITS}
Uses Syscall;
{$ENDIF FPC_DOTTEDUNITS}

{$IFDEF cpullvm}
procedure fpc_x86_outportb(p:longint;v:byte);
  begin
    asm
      movl %edx, p
      movb v, %al
      outb %al, %dx
    end ['eax','edx'];
  end;

procedure fpc_x86_outportw(p:longint;v:word);
  begin
    asm
      movl %edx, p
      movw v, %ax
      outw %ax, %dx
    end ['eax','edx'];
  end;

procedure fpc_x86_outportl(p:longint;v:longint);
  begin
    asm
      movl %edx, p
      movl v, %eax
      outl %eax, %dx
    end ['eax','edx'];
  end;

function fpc_x86_inportb(p:word):byte; assembler; nostackframe;
  asm
    xorl %eax, %eax
    movw %dx, p
    inb %dx, %al
  end;

function fpc_x86_inportw(p:word):word; assembler; nostackframe;
  asm
    xorl %eax, %eax
    movw %dx, p
    inw %dx, %ax
  end;

function fpc_x86_inportl(p:word):longint; assembler; nostackframe;
  asm
    movw %dx, p
    inl %dx, %eax
  end;
{$ENDIF ndef cpullvm}

Procedure WritePort (Port : Longint; Value : Byte);inline;
{
  Writes 'Value' to port 'Port'
}
begin
  fpc_x86_outportb(Port,Value);
end;

Procedure WritePort (Port : Longint; Value : Word);inline;
{
  Writes 'Value' to port 'Port'
}

begin
  fpc_x86_outportw(Port,Value);
end;



Procedure WritePort (Port : Longint; Value : Longint);inline;
{
  Writes 'Value' to port 'Port'
}
begin
  fpc_x86_outportl(Port,Value);
end;


Procedure WritePortB (Port : Longint; Value : Byte);inline;
{
  Writes 'Value' to port 'Port'
}
begin
  fpc_x86_outportb(Port,Value);
end;

Procedure WritePortW (Port : Longint; Value : Word);inline;
{
  Writes 'Value' to port 'Port'
}

begin
  fpc_x86_outportw(Port,Value);
end;



Procedure WritePortL (Port : Longint; Value : Longint);inline;
{
  Writes 'Value' to port 'Port'
}

begin
  fpc_x86_outportl(Port,Value);
end;



Procedure WritePortl (Port : Longint; Var Buf; Count: longint);
{
  Writes 'Count' longints from 'Buf' to Port
}
begin
  asm
{$ifdef CPU386}        
        movl count,%ecx
        movl buf,%esi
        movl port,%edx
        cld
        rep
        outsl
{$endif CPU386}        
{$ifdef CPUX86_64}        
        movl count,%ecx
        movq buf,%rsi
        movl port,%edx
        cld
        rep
        outsl
{$endif CPUX86_64}        
  end;
end;



Procedure WritePortW (Port : Longint; Var Buf; Count: longint);
{
  Writes 'Count' words from 'Buf' to Port
}
begin
  asm
{$ifdef CPU386}        
        movl count,%ecx
        movl buf,%esi
        movl port,%edx
        cld
        rep
        outsw
{$endif CPU386}        
{$ifdef CPUX86_64}        
        movl count,%ecx
        movq buf,%rsi
        movl port,%edx
        cld
        rep
        outsw
{$endif CPUX86_64}        
  end;
end;



Procedure WritePortB (Port : Longint; Var Buf; Count: longint);
{
  Writes 'Count' bytes from 'Buf' to Port
}
begin
  asm
{$ifdef CPU386}        
        movl count,%ecx
        movl buf,%esi
        movl port,%edx
        cld
        rep
        outsb
{$endif CPU386}        
{$ifdef CPUX86_64}        
        movl count,%ecx
        movq buf,%rsi
        movl port,%edx
        cld
        rep
        outsb
{$endif CPUX86_64}        
  end;
end;



Procedure ReadPort (Port : Longint; Var Value : Byte);inline;
{
  Reads 'Value' from port 'Port'
}
begin
  Value:=fpc_x86_inportb(Port);
end;



Procedure ReadPort (Port : Longint; Var Value : Word);inline;
{
  Reads 'Value' from port 'Port'
}
begin
  Value:=fpc_x86_inportw(Port);
end;



Procedure ReadPort (Port : Longint; Var Value : Longint);inline;
{
  Reads 'Value' from port 'Port'
}
begin
  Value:=fpc_x86_inportl(Port);
end;



function ReadPortB (Port : Longint): Byte;inline;
{
  Reads a byte from port 'Port'
}
begin
  ReadPortB:=fpc_x86_inportb(Port);
end;

function ReadPortW (Port : Longint): Word;inline;
{
  Reads a word from port 'Port'
}
begin
  ReadPortW:=fpc_x86_inportw(Port);
end;

function ReadPortL (Port : Longint): LongInt;inline;
{
  Reads a LongInt from port 'Port'
}
begin
  ReadPortL:=fpc_x86_inportl(Port);
end;

Procedure ReadPortL (Port : Longint; Var Buf; Count: longint);
{
  Reads 'Count' longints from port 'Port' to 'Buf'.
}
begin
  asm
{$ifdef CPU386}        
        movl count,%ecx
        movl buf,%edi
        movl port,%edx
        cld
        rep
        insl
{$endif CPU386}        
{$ifdef CPUX86_64}        
        movl count,%ecx
        movq buf,%rdi
        movl port,%edx
        cld
        rep
        insl
{$endif CPUX86_64}        
  end;
end;



Procedure ReadPortW (Port : Longint; Var Buf; Count: longint);
{
  Reads 'Count' words from port 'Port' to 'Buf'.
}
begin
  asm
{$ifdef CPU386}        
        movl count,%ecx
        movl buf,%edi
        movl port,%edx
        cld
        rep
        insw
{$endif CPU386}        
{$ifdef CPUX86_64}        
        movl count,%ecx
        movq buf,%rdi
        movl port,%edx
        cld
        rep
        insw
{$endif CPUX86_64}        
  end;
end;

Procedure ReadPortB (Port : Longint; Var Buf; Count: longint);
{
  Reads 'Count' bytes from port 'Port' to 'Buf'.
}
begin
  asm
{$ifdef CPU386}        
        movl count,%ecx
        movl buf,%edi
        movl port,%edx
        cld
        rep
        insb
{$endif CPU386}        
{$ifdef CPUX86_64}        
        movl count,%ecx
        movq buf,%rdi
        movl port,%edx
        cld
        rep
        insb
{$endif CPUX86_64}        
  end;
end;

{$if defined(linux) or defined(android)}
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
        sub_args : PAnsiChar;               { args }
        end;

   sysarch_args     = record
                        op    : longint;
                        parms : PAnsiChar;
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
 {$else}
  fpIOPL:=-1;
  FpSetErrNo(ESysENoSys);
 {$endif}
end;


end.
