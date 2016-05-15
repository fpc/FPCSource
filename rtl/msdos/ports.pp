{
    This file is part of the Free Pascal run time library.
    and implements some stuff for protected mode programming
    Copyright (c) 1999-2000 by the Free Pascal development team.

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

{$if defined(CPU80386)
  or defined(CPUPENTIUM)
  or defined(CPUPENTIUM2)
  or defined(CPUPENTIUM3)
  or defined(CPUPENTIUM4)
  or defined(CPUPENTIUMM)}
  {$define CPU_IS_386_OR_LATER}
{$endif}

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

{$ifdef CPU_IS_386_OR_LATER}
   tportl = class
      procedure writeport(p : word;data : longint);
      function  readport(p : word) : longint;
      property pp[w : word] : longint read readport write writeport;default;
   end;
{$endif CPU_IS_386_OR_LATER}
var
{ we don't need to initialize port, because neither member
  variables nor virtual methods are accessed }
   port,
   portb : tport;
   portw : tportw;
{$ifdef CPU_IS_386_OR_LATER}
   portl : tportl;
{$endif CPU_IS_386_OR_LATER}

  implementation

{ to give easy port access like tp with port[] }

procedure tport.writeport(p : word;data : byte);assembler;
asm
  mov dx, p
  mov al, data
  out dx, al
end;


function tport.readport(p : word) : byte;assembler;
asm
  mov dx, p
  in al, dx
end;


procedure tportw.writeport(p : word;data : word);assembler;
asm
  mov dx, p
  mov ax, data
  out dx, ax
end;


function tportw.readport(p : word) : word;assembler;
asm
  mov dx, p
  in ax, dx
end;


{$ifdef CPU_IS_386_OR_LATER}
procedure tportl.writeport(p : word;data : longint);assembler;
asm
  mov dx, p
  mov eax, data
  out dx, eax
end;


function tportl.readport(p : word) : longint;assembler;
asm
  mov dx, p
  in eax, dx
  mov edx, eax
  shr edx, 16
end;
{$endif CPU_IS_386_OR_LATER}

end.
