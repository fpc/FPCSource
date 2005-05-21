{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2004 by Olle Raab

    This unit contain procedures specific for mode MacPas.
    It should be platform independant.

    See the file COPYING.FPC, included in this distribution,
    For details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit MacPas;

interface

{ Using inlining for small system functions/wrappers }
{$ifdef HASINLINE}
  {$inline on}
  {$define SYSTEMINLINE}
{$endif}

type
  LongDouble = ValReal;

{FourCharCode coercion
This routine coreces string literals to a FourCharCode.}
function FCC(literal: string): LongWord; {$ifdef systeminline}inline;{$endif}

{Same as FCC, to be compatible with GPC}
function FOUR_CHAR_CODE(literal: string): LongWord; {$ifdef systeminline}inline;{$endif}

{This makes casts from ShortString to FourCharCode automatically,
 to emulate the behaviour of mac pascal compilers}
operator := (s: ShortString) res: LongWord; {$ifdef systeminline}inline;{$endif}


implementation


function FCC(literal: string): LongWord; {$ifdef systeminline}inline;{$endif}
begin
  FCC := PLongWord(@literal[1])^;
end;

function FOUR_CHAR_CODE(literal: string): LongWord; {$ifdef systeminline}inline;{$endif}
begin
  FOUR_CHAR_CODE := PLongWord(@literal[1])^;
end;

operator := (s: ShortString) res: LongWord; {$ifdef systeminline}inline;{$endif}
begin
  res := PLongWord(@s[1])^;
end;

end.
