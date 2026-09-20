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
    operator- (const i1,i2: UInt128) result : UInt128;inline;
    operator* (f1,f2 : UInt128) result : UInt128;
    operator shl (value : UInt128;shift : ALUUInt) result : UInt128;
    operator shr(value : UInt128;shift : ALUUInt) result : UInt128;
    operator := (const source : UInt64) dest : UInt128;inline;

    procedure DumpUInt128(const f : UInt128);

  implementation

    const
{$ifdef FPC_LITTLE_ENDIAN}
      QWORD_LO = 0;
      QWORD_HI = 1;
{$else FPC_LITTLE_ENDIAN}
      QWORD_LO = 1;
      QWORD_HI = 0;
{$endif FPC_LITTLE_ENDIAN}

    procedure DumpUInt128(const f : UInt128);
      begin
        write(hexstr(f.QWords[QWORD_HI],16),hexstr(f.QWords[QWORD_LO],16));
      end;

{$push} {$q-,r-}
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
{$pop}

    operator+ (const i1,i2: UInt128) result : UInt128;inline;
      var
        c: Boolean;
      begin
        qword_add(i1.QWords[QWORD_LO],i2.QWords[QWORD_LO],false,result.QWords[QWORD_LO],c);
        qword_add(i1.QWords[QWORD_HI],i2.QWords[QWORD_HI],c,result.QWords[QWORD_HI],c);
      end;

    operator- (const i1,i2: UInt128) result : UInt128;inline;
      var
        ii2: UInt128;
      begin
        ii2.QWords[QWORD_LO]:=not i2.QWords[QWORD_LO];
        ii2.QWords[QWORD_HI]:=not i2.QWords[QWORD_HI];
        result:=i1+ii2+1;
      end;

    operator* (f1,f2 : UInt128) result : UInt128;
      var
        b : byte;
      begin
        result:=0;

        for b:=0 to 127 do
          begin
            if odd(f2.QWords[QWORD_LO]) then
              result:=result+f1;
            f1:=f1 shl 1;
            f2:=f2 shr 1;
          end;
      end;

    operator shl (value : UInt128;shift : ALUUInt) result : UInt128;
      begin
        shift:=shift and 127;
        if shift=0 then
          result:=value
        else if shift>63 then
          begin
            result.QWords[QWORD_LO]:=0;
            result.QWords[QWORD_HI]:=value.QWords[QWORD_LO] shl (shift-64);
          end
        else
          begin
            result.QWords[QWORD_LO]:=value.QWords[QWORD_LO] shl shift;
            result.QWords[QWORD_HI]:=(value.QWords[QWORD_HI] shl shift) or (value.QWords[QWORD_LO] shr (64-shift));
          end;
      end;

   operator shr (value : UInt128;shift : ALUUInt) result : UInt128;
      begin
        shift:=shift and 127;
        if shift=0 then
          result:=value
        else if shift>63 then
          begin
            result.QWords[QWORD_HI]:=0;
            result.QWords[QWORD_LO]:=value.QWords[QWORD_HI] shr (shift-64);
          end
        else
          begin
            result.QWords[QWORD_HI]:=value.QWords[QWORD_HI] shr shift;
            result.QWords[QWORD_LO]:=(value.QWords[QWORD_LO] shr shift) or (value.QWords[QWORD_HI] shl (64-shift));
          end;
      end;

    operator := (const source : UInt64) dest : UInt128;inline;
      begin
        dest.QWords[QWORD_LO] := source;
        dest.QWords[QWORD_HI] := 0;
      end;

end.
