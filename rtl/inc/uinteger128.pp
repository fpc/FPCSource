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

    function BsrUInt128(Const AValue : UInt128): {$ifdef CPU16}byte{$else}cardinal{$endif};

    operator+ (const i1,i2: UInt128) result : UInt128;inline;
    operator- (const i1,i2: UInt128) result : UInt128;inline;
    operator* (f1,f2 : UInt128) result : UInt128;
    operator div (z,n : uint128) fpc_div_uint128 : uint128;
    operator mod (z,n: uint128) fpc_mod_uint128 : uint128;
    operator shl (value : UInt128;shift : ALUUInt) result : UInt128;
    operator shr(value : UInt128;shift : ALUUInt) result : UInt128;
    operator and (const i1,i2: UInt128) result: UInt128;
    operator or (const i1,i2: UInt128) result: UInt128;
    operator xor (const i1,i2: UInt128) result: UInt128;

    operator = (const i1,i2: UInt128) result: Boolean;inline;
    operator < (const i1,i2: UInt128) result: Boolean;inline;
    operator <= (const i1,i2: UInt128) result: Boolean;inline;
    operator > (const i1,i2: UInt128) result: Boolean;inline;
    operator >= (const i1,i2: UInt128) result: Boolean;inline;

    operator := (const source : UInt64) dest : UInt128;inline;

    function IntToStr(Value: UInt128): string;

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

    operator div (z,n : uint128) fpc_div_uint128 : uint128;
      var
         shift,lzz,lzn : longint;
      begin
         { Use the usually faster 64-bit division if possible }
         if (z.QWords[QWORD_HI] = 0) and (n.QWords[QWORD_HI] = 0) then
           begin
             fpc_div_uint128 := z.QWords[QWORD_LO] div n.QWords[QWORD_LO];
             exit;
           end;
         fpc_div_uint128:=0;
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
                  fpc_div_uint128:=fpc_div_uint128+(uint128(1) shl shift);
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
             fpc_mod_uint128 := z.QWords[QWORD_LO] mod n.QWords[QWORD_LO];
             exit;
           end;
         fpc_mod_uint128:=0;
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
              fpc_mod_uint128:=z;
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
         fpc_mod_uint128:=z;
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

    operator and (const i1,i2: UInt128) result: UInt128;
      begin
        result.QWords[QWORD_LO]:=i1.QWords[QWORD_LO] and i2.QWords[QWORD_LO];
        result.QWords[QWORD_HI]:=i1.QWords[QWORD_HI] and i2.QWords[QWORD_HI];
      end;

    operator or (const i1,i2: UInt128) result: UInt128;
      begin
        result.QWords[QWORD_LO]:=i1.QWords[QWORD_LO] or i2.QWords[QWORD_LO];
        result.QWords[QWORD_HI]:=i1.QWords[QWORD_HI] or i2.QWords[QWORD_HI];
      end;

    operator xor (const i1,i2: UInt128) result: UInt128;
      begin
        result.QWords[QWORD_LO]:=i1.QWords[QWORD_LO] xor i2.QWords[QWORD_LO];
        result.QWords[QWORD_HI]:=i1.QWords[QWORD_HI] xor i2.QWords[QWORD_HI];
      end;

    operator = (const i1,i2: UInt128) result: Boolean;inline;
      begin
        result:=(i1.QWords[QWORD_LO]=i2.QWords[QWORD_LO]) and
                (i1.QWords[QWORD_HI]=i2.QWords[QWORD_HI]);
      end;

    operator < (const i1,i2: UInt128) result: Boolean;inline;
      begin
        result:=(i1.QWords[QWORD_HI]<i2.QWords[QWORD_HI]) or
               ((i1.QWords[QWORD_HI]=i2.QWords[QWORD_HI]) and (i1.QWords[QWORD_LO]<i2.QWords[QWORD_LO]));
      end;

    operator <= (const i1,i2: UInt128) result: Boolean;inline;
      begin
        result:=(i1.QWords[QWORD_HI]<i2.QWords[QWORD_HI]) or
               ((i1.QWords[QWORD_HI]=i2.QWords[QWORD_HI]) and (i1.QWords[QWORD_LO]<=i2.QWords[QWORD_LO]));
      end;

    operator > (const i1,i2: UInt128) result: Boolean;inline;
      begin
        result:=(i1.QWords[QWORD_HI]>i2.QWords[QWORD_HI]) or
               ((i1.QWords[QWORD_HI]=i2.QWords[QWORD_HI]) and (i1.QWords[QWORD_LO]>i2.QWords[QWORD_LO]));
      end;

    operator >= (const i1,i2: UInt128) result: Boolean;inline;
      begin
        result:=(i1.QWords[QWORD_HI]>i2.QWords[QWORD_HI]) or
               ((i1.QWords[QWORD_HI]=i2.QWords[QWORD_HI]) and (i1.QWords[QWORD_LO]>=i2.QWords[QWORD_LO]));
      end;

    operator := (const source : UInt64) dest : UInt128;inline;
      begin
        dest.QWords[QWORD_LO] := source;
        dest.QWords[QWORD_HI] := 0;
      end;

end.
