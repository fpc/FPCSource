{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2025 by Free Pascal development team

    This file contains startup code for the Oric

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit si_prc;

{$SMARTLINK OFF}

interface

implementation

{$GOTO ON}

procedure PascalMain; external name 'PASCALMAIN';

{ this *must* always remain the first procedure with code in this unit }
procedure _start; assembler; nostackframe; public name 'start';
asm
  // TODO...
end;

end.
