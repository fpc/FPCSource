{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2020 by Free Pascal development team

    This file contains startup code for the ZX Spectrum

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

var
  FPC_SAVE_IY: word; external name 'FPC_SAVE_IY';
  fpc_stackarea_start: word; external name '__fpc_stackarea_start';
  fpc_stackarea_end: word; external name '__fpc_stackarea_end';

procedure PascalMain; external name 'PASCALMAIN';

{ this *must* always remain the first procedure with code in this unit }
procedure _start; assembler; nostackframe; public name 'start';
asm
    ld sp, fpc_stackarea_end
    ld (FPC_SAVE_IY), iy
    jp PASCALMAIN
end;

end.
