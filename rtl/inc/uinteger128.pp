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

    operator+ (const i1,i2: UInt128) result : UInt128;inline;

    operator := (const source : UInt64) dest : UInt128;inline;

  implementation

    procedure qword_add(const i1, i2: QWord; carry_in: Boolean; var o: QWord; var carry_out: Boolean);
      begin
        o := i1 + i2;
        carry_out := o < i1;
        if carry_in then
          begin
            if o = High(QWord) then
              carry_out := true;
            Inc(o);
          end;
      end;

    operator+ (const i1,i2: UInt128) result : UInt128;inline;
      var
        c: Boolean;
      begin
        qword_add(i1.QWords[0],i2.QWords[0],false,result.QWords[0],c);
        qword_add(i1.QWords[1],i2.QWords[1],c,result.QWords[1],c);
      end;

    operator := (const source : UInt64) dest : UInt128;inline;
      begin
        dest.QWords[0] := source;
        dest.QWords[1] := 0;
      end;

end.
