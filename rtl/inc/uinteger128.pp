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

{$MODE objfpc}

  interface

    type
      Int128 = record
        QWords: array [0..1] of QWord;
      end;

      UInt128 = record
        QWords: array [0..1] of QWord;
      end;

    function BsrUInt128(Const AValue : UInt128): {$ifdef CPU16}byte{$else}cardinal{$endif};

    operator+ (const i1,i2: UInt128): UInt128;inline;
    operator- (const i1,i2: UInt128): UInt128;inline;
    operator- (const i: Int128): Int128;inline;
    operator* (f1,f2 : UInt128): UInt128;
    operator div (z,n : uint128): uint128;
    operator mod (z,n: uint128): uint128;
    operator shl (value : UInt128;shift : ALUUInt): UInt128;
    operator shr(value : UInt128;shift : ALUUInt): UInt128;
    operator and (const i1,i2: UInt128): UInt128;
    operator or (const i1,i2: UInt128): UInt128;
    operator xor (const i1,i2: UInt128): UInt128;
    operator not (const i: UInt128): UInt128;

    operator = (const i1,i2: UInt128): Boolean;inline;
    operator = (const i1,i2: Int128): Boolean;inline;
    operator < (const i1,i2: UInt128): Boolean;inline;
    operator <= (const i1,i2: UInt128): Boolean;inline;
    operator > (const i1,i2: UInt128): Boolean;inline;
    operator >= (const i1,i2: UInt128): Boolean;inline;

    operator := (const source : UInt64): UInt128;inline;
    operator := (const source : Int64): Int128;inline;
    operator := (const source : UInt64): Int128;inline;

    procedure val_int128(Const S: ShortString; out V: Int128; out Code: ValSInt);
    procedure val_uint128(Const S: ShortString; out V: UInt128; out Code: ValSInt);

    function BinStr(const v: UInt128; cnt: Byte): string; overload;
    function HexStr(const v: UInt128; cnt: Byte): string; overload;
    function IntToStr(Value: UInt128): string;
    function IntToStr(Value: Int128): string;

  const
    MaxUInt128: UInt128 = (QWords: (High(QWord), High(QWord)));
{$ifdef FPC_LITTLE_ENDIAN}
    MaxInt128: Int128 = (QWords: (High(QWord), QWord(High(Int64))));
    MinInt128: Int128 = (QWords: (0, QWord(Low(Int64))));
{$else FPC_LITTLE_ENDIAN}
    MaxInt128: Int128 = (QWords: (QWord(High(Int64)), High(QWord)));
    MinInt128: Int128 = (QWords: (QWord(Low(Int64)), 0));
{$endif FPC_LITTLE_ENDIAN}

  implementation

    const
{$ifdef FPC_LITTLE_ENDIAN}
      QWORD_LO = 0;
      QWORD_HI = 1;
{$else FPC_LITTLE_ENDIAN}
      QWORD_LO = 1;
      QWORD_HI = 0;
{$endif FPC_LITTLE_ENDIAN}

    function BinStr(const v: UInt128; cnt: Byte): string; overload;
      begin
        if cnt<=64 then
          BinStr:=System.BinStr(v.QWords[QWORD_LO],cnt)
        else
          BinStr:=System.BinStr(v.QWords[QWORD_HI],cnt-64)+System.BinStr(v.QWords[QWORD_LO],64);
      end;

    function HexStr(const v: UInt128; cnt: Byte): string; overload;
      begin
        if cnt<=16 then
          HexStr:=System.HexStr(v.QWords[QWORD_LO],cnt)
        else
          HexStr:=System.HexStr(v.QWords[QWORD_HI],cnt-16)+System.HexStr(v.QWords[QWORD_LO],16);
      end;

    function IntToStr(Value: UInt128): string;
      var
        I: Integer;
        tmpC: Char;
      begin
        if Value=0 then
          begin
            IntToStr:='0';
            exit;
          end;
        IntToStr:='';
        while Value<>0 do
          begin
            IntToStr:=IntToStr+Chr(Ord('0') + (Value mod 10).QWords[QWORD_LO]);
            Value:=Value div 10;
          end;
        for I:=1 to Length(IntToStr) div 2 do
          begin
            tmpC:=IntToStr[I];
            IntToStr[I]:=IntToStr[Length(IntToStr)-I+1];
            IntToStr[Length(IntToStr)-I+1]:=tmpC;
          end;
      end;

    function IntToStr(Value: Int128): string;
      var
        UValue: UInt128 absolute Value;
      begin
        if (UValue.QWords[QWORD_HI] and (qword(1) shl 63)) = 0 then
          IntToStr:=IntToStr(UValue)
        else
          begin
            UValue:=not UValue;
            UValue:=UValue+1;
            IntToStr:='-'+IntToStr(UValue);
          end;
      end;

    function BsrUInt128(Const AValue : UInt128): {$ifdef CPU16}byte{$else}cardinal{$endif};
      var
        tmp: QWord;
      begin
        BsrUInt128:=64;
        tmp:=AValue.QWords[QWORD_HI];
        if (tmp=0) then
          begin
            tmp:=AValue.QWords[QWORD_LO];
            BsrUInt128:=0;
          end;
        BsrUInt128:=BsrUInt128 or BsrQword(tmp);
      end;

{$push} {$q-,r-}
    procedure qword_add(const i1, i2: QWord; carry_in: Boolean; out o: QWord; out carry_out: Boolean);
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

    operator+ (const i1,i2: UInt128): UInt128;inline;
      var
        c: Boolean;
      begin
        qword_add(i1.QWords[QWORD_LO],i2.QWords[QWORD_LO],false,result.QWords[QWORD_LO],c);
        qword_add(i1.QWords[QWORD_HI],i2.QWords[QWORD_HI],c,result.QWords[QWORD_HI],c);
      end;

    operator- (const i1,i2: UInt128): UInt128;inline;
      var
        ii2: UInt128;
      begin
        ii2.QWords[QWORD_LO]:=not i2.QWords[QWORD_LO];
        ii2.QWords[QWORD_HI]:=not i2.QWords[QWORD_HI];
        result:=i1+ii2+1;
      end;

    operator- (const i: Int128): Int128;inline;
      var
        ii: UInt128;
      begin
        ii.QWords[QWORD_LO]:=not i.QWords[QWORD_LO];
        ii.QWords[QWORD_HI]:=not i.QWords[QWORD_HI];
        result:=Int128(ii+1);
      end;

    operator* (f1,f2 : UInt128): UInt128;
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

    operator div (z,n : uint128) fpc_div_uint128 : uint128;
      var
         shift,lzz,lzn : longint;
      begin
         { Use the usually faster 64-bit division if possible }
         if (z.QWords[QWORD_HI] = 0) and (n.QWords[QWORD_HI] = 0) then
           begin
             result := z.QWords[QWORD_LO] div n.QWords[QWORD_LO];
             exit;
           end;
         result:=0;
         if n=0 then
           RunError(200);
           //TODO:HandleErrorAddrFrameInd(200,get_pc_addr,get_frame);
         if z=0 then
           exit;
         lzz:=BsrUInt128(z);
         lzn:=BsrUInt128(n);
         { if the denominator contains less zeros }
         { than the numerator                     }
         { then d is greater than the n           }
         if lzn>lzz then
           exit;

         shift:=lzz-lzn;
         n:=n shl shift;
         for shift:=shift downto 0 do
           begin
             if z>=n then
               begin
                  z:=z-n;
                  result:=result+(uint128(1) shl shift);
               end;
             n:=n shr 1;
           end;
      end;

    operator mod (z,n: uint128) fpc_mod_uint128 : uint128;
      var
         shift,lzz,lzn : longint;
      begin
         { Use the usually faster 64-bit mod if possible }
         if (z.QWords[QWORD_HI] = 0) and (n.QWords[QWORD_HI] = 0) then
           begin
             result := z.QWords[QWORD_LO] mod n.QWords[QWORD_LO];
             exit;
           end;
         result:=0;
         if n=0 then
           RunError(200);
           //TODO:HandleErrorAddrFrameInd(200,get_pc_addr,get_frame);
         if z=0 then
           exit;
         lzz:=BsrUInt128(z);
         lzn:=BsrUInt128(n);
         { if the denominator contains less zeros }
         { then the numerator                     }
         { the d is greater than the n            }
         if lzn>lzz then
           begin
              result:=z;
              exit;
           end;
         shift:=lzz-lzn;
         n:=n shl shift;
         for shift:=shift downto 0 do
           begin
             if z>=n then
               z:=z-n;
             n:=n shr 1;
           end;
         result:=z;
      end;

    operator shl (value : UInt128;shift : ALUUInt): UInt128;
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

   operator shr (value : UInt128;shift : ALUUInt): UInt128;
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

    operator and (const i1,i2: UInt128): UInt128;
      begin
        result.QWords[QWORD_LO]:=i1.QWords[QWORD_LO] and i2.QWords[QWORD_LO];
        result.QWords[QWORD_HI]:=i1.QWords[QWORD_HI] and i2.QWords[QWORD_HI];
      end;

    operator or (const i1,i2: UInt128): UInt128;
      begin
        result.QWords[QWORD_LO]:=i1.QWords[QWORD_LO] or i2.QWords[QWORD_LO];
        result.QWords[QWORD_HI]:=i1.QWords[QWORD_HI] or i2.QWords[QWORD_HI];
      end;

    operator xor (const i1,i2: UInt128): UInt128;
      begin
        result.QWords[QWORD_LO]:=i1.QWords[QWORD_LO] xor i2.QWords[QWORD_LO];
        result.QWords[QWORD_HI]:=i1.QWords[QWORD_HI] xor i2.QWords[QWORD_HI];
      end;

    operator not (const i: UInt128): UInt128;
      begin
        result.QWords[QWORD_LO]:=not i.QWords[QWORD_LO];
        result.QWords[QWORD_HI]:=not i.QWords[QWORD_HI];
      end;

    operator = (const i1,i2: UInt128): Boolean;inline;
      begin
        result:=(i1.QWords[QWORD_LO]=i2.QWords[QWORD_LO]) and
                (i1.QWords[QWORD_HI]=i2.QWords[QWORD_HI]);
      end;

    operator = (const i1,i2: Int128): Boolean;inline;
      begin
        result:=(i1.QWords[QWORD_LO]=i2.QWords[QWORD_LO]) and
                (i1.QWords[QWORD_HI]=i2.QWords[QWORD_HI]);
      end;

    operator < (const i1,i2: UInt128): Boolean;inline;
      begin
        result:=(i1.QWords[QWORD_HI]<i2.QWords[QWORD_HI]) or
               ((i1.QWords[QWORD_HI]=i2.QWords[QWORD_HI]) and (i1.QWords[QWORD_LO]<i2.QWords[QWORD_LO]));
      end;

    operator <= (const i1,i2: UInt128): Boolean;inline;
      begin
        result:=(i1.QWords[QWORD_HI]<i2.QWords[QWORD_HI]) or
               ((i1.QWords[QWORD_HI]=i2.QWords[QWORD_HI]) and (i1.QWords[QWORD_LO]<=i2.QWords[QWORD_LO]));
      end;

    operator > (const i1,i2: UInt128): Boolean;inline;
      begin
        result:=(i1.QWords[QWORD_HI]>i2.QWords[QWORD_HI]) or
               ((i1.QWords[QWORD_HI]=i2.QWords[QWORD_HI]) and (i1.QWords[QWORD_LO]>i2.QWords[QWORD_LO]));
      end;

    operator >= (const i1,i2: UInt128): Boolean;inline;
      begin
        result:=(i1.QWords[QWORD_HI]>i2.QWords[QWORD_HI]) or
               ((i1.QWords[QWORD_HI]=i2.QWords[QWORD_HI]) and (i1.QWords[QWORD_LO]>=i2.QWords[QWORD_LO]));
      end;

    operator := (const source : UInt64) dest : UInt128;inline;
      begin
        result.QWords[QWORD_LO] := source;
        result.QWords[QWORD_HI] := 0;
      end;

    operator := (const source : Int64): Int128;inline;
      begin
        result.QWords[QWORD_LO] := QWord(source);
        if source>=0 then
          result.QWords[QWORD_HI] := 0
        else
          result.QWords[QWORD_HI] := High(QWord);
      end;

    operator := (const source : UInt64): Int128;inline;
      begin
        result.QWords[QWORD_LO] := source;
        result.QWords[QWORD_HI] := 0;
      end;

{$i sstrings_val_common.inc}
{$i sstrings_val_int128.inc}

    procedure val_int128(Const S: ShortString; out V: Int128; out Code: ValSInt);
      begin
        V:=fpc_val_int128_shortstr(S,Code);
      end;

    procedure val_uint128(Const S: ShortString; out V: UInt128; out Code: ValSInt);
      begin
        V:=fpc_val_uint128_shortstr(S,Code);
      end;

end.
