{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                Int128/UInt128 support for the Val() Functions
*****************************************************************************}

  Function fpc_val_int128_shortstr(Const S: ShortString; out Code: ValSInt): Int128;{ [public, alias:'FPC_VAL_INT128_SHORTSTR']; compilerproc;}

  var  u : sizeuint;
       sp,ns : sizeint;
       digitsLeft : ALUSint;
       temp, prev, lim : UInt128;
       vc : ValCommon;

  begin
  {$ifdef EXCLUDE_COMPLEX_PROCS}
    runerror(219);
  {$else EXCLUDE_COMPLEX_PROCS}
    fpc_val_int128_shortstr := 0;
    sp:=InitVal(s,vc);
    ns:=length(s);
    if (sp>ns) or (s[sp]=#0) then
     begin
       Code:=sp;
       exit;
     end;

    digitsLeft:=ValData.MaxDigits[ord((vc.base=10) or vc.negative)+ord(vc.negative), vc.baseIndex, 4];
    Temp:=0;
    prev:=0;
    repeat
      u:=SizeUint(ord(s[sp])-ord('0'));
      if u>=length(ValData.ValueArray) then
        break;
      u:=ValData.ValueArray[u];
      If u>=vc.base then
        break;
      dec(digitsLeft);
      if digitsLeft<0 then
        break;
      prev:=Temp;
      Temp:=Temp*vc.base+u;
      inc(sp);
    until sp>ns;

    if digitsLeft<=0 then { Maybe an overflow. If so, step 1 digit back. }
     begin
       if sp<=ns then { If the loop was stopped not by sp>ns check... }
         u:=ValData.ValueArray[ord(s[sp-1])-ord('0')]; { Then recover previous u. }
       lim:=MaxUInt128;
       if (vc.base=10) or vc.negative then
        begin
          lim:=uint128(MaxInt128);
          if vc.negative then { lim:=uint64(High(int64))+ord(vc.negative) triggers #41148. }
            lim:=uint128(MinInt128);
        end;
       if prev>uint128(lim-u) div ValNonZeroBase(vc.base) then
         dec(sp); { Overflow. Step 1 digit back. }
     end;

    if (sp<=ns) and (s[sp]<>#0) then
     begin
       Code:=sp;
       exit;
     end;

    code:=0;
    fpc_val_int128_shortstr:=int128(Temp);
    if vc.negative then
      fpc_val_int128_shortstr:=-fpc_val_int128_shortstr;
  {$endif EXCLUDE_COMPLEX_PROCS}
  end;

  Function fpc_val_uint128_shortstr(Const S: ShortString; out Code: ValSInt): UInt128; {[public, alias:'FPC_VAL_UINT128_SHORTSTR']; compilerproc;}
  var
    sp,ns : SizeInt;
    u : SizeUInt;
    digitsLeft : ALUSint;
    prev: UInt128;
    vc: ValCommon;
  begin
    fpc_val_uint128_shortstr:=0;
    sp:=InitVal(s,vc);
    ns:=length(s);
    If vc.negative or (sp>ns) or (s[sp]=#0) Then
      begin
        if vc.negative then sp:=vc.minusPos;
        Code:=sp;
        Exit;
      end;

    prev:=0;
    digitsLeft:=ValData.MaxDigits[0, vc.baseIndex, 4];
    repeat
      u:=SizeUint(ord(s[sp])-ord('0'));
      if u>=length(ValData.ValueArray) then
        break;
      u:=ValData.ValueArray[u];
      If u>=vc.base then
        break;
      dec(digitsLeft);
      if digitsLeft<0 then
        break;
      prev:=fpc_val_uint128_shortstr;
      fpc_val_uint128_shortstr:=fpc_val_uint128_shortstr*vc.base+u;
      inc(sp);
    until sp>ns;

    if digitsLeft<=0 then { Maybe an overflow. If so, step 1 digit back. }
     begin
       if sp<=ns then { If the loop was stopped not by sp>ns check... }
         u:=ValData.ValueArray[ord(s[sp-1])-ord('0')]; { Then recover previous u. }
       if prev>UInt128(MaxUInt128-u) div ValNonZeroBase(vc.base) then
         dec(sp);
     end;

    code:=0;
    if (sp<=ns) and (s[sp]<>#0) then
     begin
       Code:=sp;
       fpc_val_uint128_shortstr:=0;
     end;
  end;
