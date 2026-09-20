{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the FPC development time

    Implements overloaded operators and misc. functions to
    provide the int128 and uint128 types

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$inline on}
{$IFNDEF FPC_DOTTEDUNITS}
unit uinteger128;
{$ENDIF FPC_DOTTEDUNITS}

  interface

    type
      Int128 = record
        QWords: array [0..1] of QWord;
      end;

      UInt128 = record
        QWords: array [0..1] of QWord;
      end;

    operator := (const source : UInt64) dest : UInt128;inline;

  implementation

    operator := (const source : UInt64) dest : UInt128;inline;
      begin
        dest.QWords[0] := source;
        dest.QWords[1] := 0;
      end;

end.
