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
                           Val() Functions
*****************************************************************************}

type
  ValCommon=record
    negative:boolean;
    base,baseIndex:byte; { baseIndex is flattened base for lookups: 0 — base 10, 1 — base 16, 2 — base 2, 3 — base 8. }
    minusPos:SizeInt;
  end;

Function InitVal(const s:shortstring;out vc:ValCommon):ValSInt;
var
  ns : SizeInt;
begin
  result:=1;
  vc.negative:=false;
  vc.base:=10;
  vc.baseIndex:=0;
  ns:=length(s);
{Skip Spaces and Tab}
  while (result<=ns) and (s[result] in [' ',#9]) do
   inc(result);
{Sign}
  if result<=ns then
   case s[result] of
    '-' : begin
            vc.negative:=true;
            vc.minusPos:=result;
            inc(result);
          end;
    '+' : inc(result);
   end;
{Base}
  if result<=ns then
   case s[result] of
    '$',
    'X',
    'x' : begin
            vc.base:=16;
            vc.baseIndex:=1;
            inc(result);
          end;
    '%' : begin
            vc.base:=2;
            vc.baseIndex:=2;
            inc(result);
          end;
    '&' : begin
            vc.base:=8;
            vc.baseIndex:=3;
            inc(result);
          end;
    '0' : if (result<ns) and (s[result+1] in ['x', 'X']) then
          begin
            vc.base:=16;
            vc.baseIndex:=1;
            inc(result, 2);
          end;
   end;
  { strip leading zeros }
  while (result<ns) and (s[result]='0') and (s[result+1]<>#0) do
    inc(result);
end;

const
  ValData: record
    ValueArray: array[0..ord('f')-ord('0')] of byte;
    MaxDigits: array[0 .. 2 { unsigned / signed positive / signed negative }, 0 .. 3 { base index }, 0 .. 4 { Bsr(DestSize) }] of byte;
  end =
  (
    ValueArray:
    (
      0,1,2,3,4,5,6,7,8,9,$FF,$FF,$FF,$FF,$FF,$FF,$FF,10,11,12,13,14,15,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
      10,11,12,13,14,15
    );

    { If VAL input has exactly this many digits (without leading zeros), then it may overflow.
      If it has more digits, it definitely overflows.
      If it has less, it definitely doesn’t overflow. }
    MaxDigits:
    (
      ( { unsigned }
        (3, 5, 10, 20, 39), { base 10: 255 / 65535 / 4_294_967_295 / 18_446_744_073_709_551_615 / 340_282_366_920_938_463_463_374_607_431_768_211_455 }
        (2, 4, 8, 16, 32), { base 16: $FF / $FFFF / $FFFF_FFFF / $FFFF_FFFF_FFFF_FFFF /  $FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF }
        (8, 16, 32, 64, 128), { base 2: %1111_1111 / %1111_1111_1111_1111 / %1111_1111_1111_1111_1111_1111_1111_1111 / %1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111 / %1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111 }
        (3, 6, 11, 22, 43) { base 8: &377 / &17_7777 / &377_7777_7777 / &17_7777_7777_7777_7777_7777 / &377_7777_7777_7777_7777_7777_7777_7777_7777_7777_7777 }
      ),
      ( { signed positive }
        (3, 5, 10, 19, 39), { base 10: 127 / 32767 / 2_147_483_647 / 9_223_372_036_854_775_807 / 170_141_183_460_469_231_731_687_303_715_884_105_727 }
        (2, 4, 8, 16, 32), { base 16: $7F / $7FFF / $7FFF_FFFF / $7FFF_FFFF_FFFF_FFFF / $7FFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF }
        (7, 15, 31, 63, 127), { base 2: %111_1111 / %111_1111_1111_1111 / %111_1111_1111_1111_1111_1111_1111_1111 / %111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111 / %111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111 }
        (3, 5, 11, 21, 43) { base 8: &177 / &7_7777 / &177_7777_7777 / &7_7777_7777_7777_7777_7777 / &177_7777_7777_7777_7777_7777_7777_7777_7777_7777_7777 }
      ),
      ( { signed negative }
        (3, 5, 10, 19, 39), { base 10: 128 / 32768 / 2_147_483_648 / 9_223_372_036_854_775_808 / 170_141_183_460_469_231_731_687_303_715_884_105_728 }
        (2, 4, 8, 16, 32), { base 16: $80 / $8000 / $8000_0000 / $8000_0000_0000_0000 / $8000_0000_0000_0000_0000_0000_0000_0000 }
        (8, 16, 32, 64, 128), { base 2: %1000_0000 / %1000_0000_0000_0000 / %1000_0000_0000_0000_0000_0000_0000_0000 / %1000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 / %1000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 }
        (3, 6, 11, 22, 43) { base 8: &200 / &10_0000 / &200_0000_0000 / &10_0000_0000_0000_0000_0000 / &200_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 }
      )
    )
  );

type
  ValNonZeroBase = 1 .. ord(High(ValCommon.base));
  ValNonZeroDestSize = 1 .. ord(High(uint32));

Function fpc_Val_SInt_ShortStr(DestSize: SizeInt; Const S: ShortString; out Code: ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_SHORTSTR']; compilerproc;
var
  sp,ns : SizeInt;
  u : SizeUInt;
  digitsLeft,sh : ALUSint;
  temp, prev, lim: ValUInt;
  vc: ValCommon;
begin
  fpc_Val_SInt_ShortStr:=0;
  sp:=InitVal(s,vc);
  ns:=length(s);
  if (sp>ns) or (s[sp]=#0) then
   begin
     Code:=sp;
     exit;
   end;

  digitsLeft:=ValData.MaxDigits[ord((vc.base=10) or vc.negative)+ord(vc.negative), vc.baseIndex, BsrDWord(ValNonZeroDestSize(DestSize))];
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
    Temp:=Temp*ValUInt(vc.base)+u;
    inc(sp);
  until sp>ns;

  if digitsLeft<=0 then { Maybe an overflow. If so, step 1 digit back. }
   begin
     if sp<=ns then { If the loop was stopped not by sp>ns check... }
       u:=ValData.ValueArray[ord(s[sp-1])-ord('0')]; { Then recover previous u. }
     lim:=High(lim) shr (bitsizeof(lim)-8*DestSize);
     if (vc.base=10) or vc.negative then
       lim:=lim shr 1+Ord(vc.negative); { Convert to signed limit. }
     if prev>ValUInt(lim-u) div ValNonZeroBase(vc.base) then
       dec(sp); { Overflow. Step 1 digit back. }
   end;

  if (sp<=ns) and (s[sp]<>#0) then
   begin
     Code:=sp;
     exit;
   end;

  Code:=0;
  fpc_Val_SInt_ShortStr := ValSInt(Temp);
  If vc.Negative Then
    fpc_Val_SInt_ShortStr := -fpc_Val_SInt_ShortStr;

  If Not(vc.Negative) and (vc.base<>10) and (DestSize<sizeof(fpc_Val_SInt_ShortStr)) Then
   begin
     {sign extend the result to allow proper range checking}
     sh:=bitsizeof(fpc_Val_SInt_ShortStr)-8*DestSize;
     fpc_Val_SInt_ShortStr:=
{$if sizeof(ValSint)=sizeof(int64)} SarInt64
{$elseif sizeof(ValSint)=sizeof(int32)} SarLongint
{$elseif sizeof(ValSint)=sizeof(int16)} SarSmallint
{$else} {$error unknown ValSint size}
{$endif} (fpc_Val_SInt_ShortStr shl sh, sh);
    End;
end;


{$ifndef FPC_HAS_INT_VAL_SINT_SHORTSTR}
{$define FPC_HAS_INT_VAL_SINT_SHORTSTR}
{ we need this for fpc_Val_SInt_Ansistr and fpc_Val_SInt_WideStr because }
{ we have to pass the DestSize parameter on (JM)                         }
Function int_Val_SInt_ShortStr(DestSize: SizeInt; Const S: ShortString; out Code: ValSInt): ValSInt; [external name 'FPC_VAL_SINT_SHORTSTR'];
{$endif FPC_HAS_INT_VAL_SINT_SHORTSTR}


Function fpc_Val_UInt_Shortstr({$ifndef VER3_2}DestSize: SizeInt;{$endif VER3_2} Const S: ShortString; out Code: ValSInt): ValUInt; [public, alias:'FPC_VAL_UINT_SHORTSTR']; compilerproc;
var
  sp,ns : SizeInt;
  u : SizeUInt;
  digitsLeft : ALUSint;
  prev: ValUInt;
  vc: ValCommon;
begin
  fpc_Val_UInt_Shortstr:=0;
  sp:=InitVal(s,vc);
  ns:=length(s);
  If vc.negative or (sp>ns) or (s[sp]=#0) Then
    begin
      if vc.negative then sp:=vc.minusPos;
      Code:=sp;
      Exit;
    end;

  digitsLeft:=ValData.MaxDigits[0, vc.baseIndex, BsrDWord(ValNonZeroDestSize({$ifndef VER3_2}DestSize{$else}sizeof(fpc_Val_UInt_Shortstr){$endif}))];
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
    prev:=fpc_Val_UInt_Shortstr;
    fpc_Val_UInt_Shortstr:=fpc_Val_UInt_Shortstr*ValUInt(vc.base)+u;
    inc(sp);
  until sp>ns;

  if digitsLeft<=0 then { Maybe an overflow. If so, step 1 digit back. }
   begin
     if sp<=ns then { If the loop was stopped not by sp>ns check... }
       u:=ValData.ValueArray[ord(s[sp-1])-ord('0')]; { Then recover previous u. }
     if prev>ValUInt(High(result) {$ifndef VER3_2} shr (bitsizeof(result)-8*DestSize) {$endif}-u) div ValNonZeroBase(vc.base) then
       dec(sp);
   end;

  code:=0;
  if (sp<=ns) and (s[sp]<>#0) then
   begin
     Code:=sp;
     fpc_Val_UInt_Shortstr:=0;
   end;
end;


{$ifndef CPU64}

  Function fpc_val_int64_shortstr(Const S: ShortString; out Code: ValSInt): Int64; [public, alias:'FPC_VAL_INT64_SHORTSTR']; compilerproc;

  var  u : sizeuint;
       sp,ns : sizeint;
       digitsLeft : ALUSint;
       temp, prev, lim : qword;
       vc : ValCommon;

  begin
  {$ifdef EXCLUDE_COMPLEX_PROCS}
    runerror(219);
  {$else EXCLUDE_COMPLEX_PROCS}
    fpc_val_int64_shortstr := 0;
    sp:=InitVal(s,vc);
    ns:=length(s);
    if (sp>ns) or (s[sp]=#0) then
     begin
       Code:=sp;
       exit;
     end;

    digitsLeft:=ValData.MaxDigits[ord((vc.base=10) or vc.negative)+ord(vc.negative), vc.baseIndex, 3];
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
       lim:=High(lim);
       if (vc.base=10) or vc.negative then
        begin
          lim:=uint64(High(int64));
          if vc.negative then { lim:=uint64(High(int64))+ord(vc.negative) triggers #41148. }
            lim:=uint64(Low(int64));
        end;
       if prev>uint64(lim-u) div ValNonZeroBase(vc.base) then
         dec(sp); { Overflow. Step 1 digit back. }
     end;

    if (sp<=ns) and (s[sp]<>#0) then
     begin
       Code:=sp;
       exit;
     end;

    code:=0;
    fpc_val_int64_shortstr:=int64(Temp);
    if vc.negative then
      fpc_val_int64_shortstr:=-fpc_val_int64_shortstr;
  {$endif EXCLUDE_COMPLEX_PROCS}
  end;

  Function fpc_val_qword_shortstr(Const S: ShortString; out Code: ValSInt): QWord; [public, alias:'FPC_VAL_QWORD_SHORTSTR']; compilerproc;
  var
    sp,ns : SizeInt;
    u : SizeUInt;
    digitsLeft : ALUSint;
    prev: qword;
    vc: ValCommon;
  begin
    fpc_val_qword_shortstr:=0;
    sp:=InitVal(s,vc);
    ns:=length(s);
    If vc.negative or (sp>ns) or (s[sp]=#0) Then
      begin
        if vc.negative then sp:=vc.minusPos;
        Code:=sp;
        Exit;
      end;

    prev:=0;
    digitsLeft:=ValData.MaxDigits[0, vc.baseIndex, 3];
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
      prev:=fpc_val_qword_shortstr;
      fpc_val_qword_shortstr:=fpc_val_qword_shortstr*vc.base+u;
      inc(sp);
    until sp>ns;

    if digitsLeft<=0 then { Maybe an overflow. If so, step 1 digit back. }
     begin
       if sp<=ns then { If the loop was stopped not by sp>ns check... }
         u:=ValData.ValueArray[ord(s[sp-1])-ord('0')]; { Then recover previous u. }
       if prev>qword(High(qword)-u) div ValNonZeroBase(vc.base) then
         dec(sp);
     end;

    code:=0;
    if (sp<=ns) and (s[sp]<>#0) then
     begin
       Code:=sp;
       fpc_val_qword_shortstr:=0;
     end;
  end;
{$endif CPU64}

{$if defined(CPU16) or defined(CPU8)}
  Function fpc_val_longint_shortstr(Const S: ShortString; out Code: ValSInt): LongInt; [public, alias:'FPC_VAL_LONGINT_SHORTSTR']; compilerproc;

  var  u, temp, prev, maxprevvalue, maxnewvalue : longword;
       vc : ValCommon;

  const maxlongint=longword($7fffffff);
        maxlongword=longword($ffffffff);

  begin
    fpc_val_longint_shortstr := 0;
    Temp:=0;
    Code:=InitVal(s,vc);
    if (Code>length(s)) or (s[Code]=#0) then
      exit;
    maxprevvalue := maxlongword div vc.base;
    if (vc.base = 10) then
      maxnewvalue := maxlongint + ord(vc.negative)
    else
      maxnewvalue := maxlongword;

    while Code<=Length(s) do
     begin
       u:=16;
       case s[code] of
         '0'..'f' : u:=ValData.ValueArray[ord(S[Code])-ord('0')];
         #0 : break;
       else
         ;
       end;

       Prev:=Temp;
       Temp:=Temp*longword(vc.base);
       If (u >= vc.base) or
         (longword(maxnewvalue-u) < temp) or
         (prev > maxprevvalue) Then
         Begin
           fpc_val_longint_shortstr := 0;
           Exit;
         End;
       Temp:=Temp+u;
       inc(code);
     end;
    code:=0;
    fpc_val_longint_shortstr:=longint(Temp);
    If vc.Negative Then
      fpc_val_longint_shortstr:=-fpc_val_longint_shortstr;
  end;


  Function fpc_val_longword_shortstr(Const S: ShortString; out Code: ValSInt): LongWord; [public, alias:'FPC_VAL_LONGWORD_SHORTSTR']; compilerproc;

  var  u, prev: LongWord;
       vc: ValCommon;

  const UpperLimit=High(longword);

  begin
    fpc_val_longword_shortstr:=0;
    Code:=InitVal(s,vc);
    If vc.Negative or (Code>length(s)) or (s[Code]=#0) Then
      Exit;
    while Code<=Length(s) do
     begin
       u:=16;
       case s[code] of
         '0'..'f' : u:=ValData.ValueArray[ord(S[Code])-ord('0')];
         #0 : break;
       else
         ;
       end;

       If (u>=vc.base) or
          (LongWord(UpperLimit-u) div LongWord(vc.Base)<fpc_val_longword_shortstr) then
        begin
          fpc_val_longword_shortstr:=0;
          exit;
        end;
       fpc_val_longword_shortstr:=fpc_val_longword_shortstr*vc.base + u;
       inc(code);
     end;
    code := 0;
  end;


  Function fpc_val_smallint_shortstr(Const S: ShortString; out Code: ValSInt): SmallInt; [public, alias:'FPC_VAL_SMALLINT_SHORTSTR']; compilerproc;

  var  u, temp, prev, maxprevvalue : word;
       UnsignedUpperLimit: ValUInt;
       vc: ValCommon;
  begin
    fpc_val_smallint_shortstr := 0;
    Temp:=0;
    Code:=InitVal(s,vc);
    if (vc.base=10) or vc.negative then
      UnsignedUpperLimit := Word(High(SmallInt))+Ord(vc.negative)
    else
      UnsignedUpperLimit := High(Word);
    if (Code>length(s)) or (s[Code]=#0) then
     exit;
    maxprevvalue := High(Word) div vc.base;

    while Code<=Length(s) do
     begin
       u:=16;
       case s[code] of
         '0'..'f' : u:=ValData.ValueArray[ord(S[Code])-ord('0')];
         #0 : break;
       else
         ;
       end;

       Prev:=Temp;
       Temp:=Temp*longword(vc.base);
       If (u >= vc.base) or
        (prev > maxPrevValue) or
        ((Temp)>(UnsignedUpperLimit-u)) Then
       Begin
         fpc_val_smallint_shortstr := 0;
         Exit
       End;
       Temp:=Temp+u;
       inc(code);
     end;
    code:=0;
    fpc_val_smallint_shortstr:=SmallInt(Temp);
    If vc.Negative Then
      fpc_val_smallint_shortstr:=-fpc_val_smallint_shortstr;
  end;


  Function fpc_val_word_shortstr(Const S: ShortString; out Code: ValSInt): Word; [public, alias:'FPC_VAL_WORD_SHORTSTR']; compilerproc;

  var  u, prev: word;
       vc: ValCommon;

  const UpperLimit=High(Word);  //this preserves 3.2 (and earlier) behaviour

  begin
    fpc_val_word_shortstr:=0;
    Code:=InitVal(s,vc);
    If vc.Negative or (Code>length(s)) or (s[Code]=#0) Then
      begin
        if vc.Negative then Code:=vc.minusPos;
        Exit;
      end;
    while Code<=Length(s) do
     begin
       u:=16;
       case s[code] of
         '0'..'f' : u:=ValData.ValueArray[ord(S[Code])-ord('0')];
         #0 : break;
       else
         ;
       end;

       If (u>=vc.base) or
          (Word(UpperLimit-u) div Word(vc.Base)<fpc_val_word_shortstr) then
        begin
          fpc_val_word_shortstr:=0;
          exit;
        end;
       fpc_val_word_shortstr:=fpc_val_word_shortstr*vc.base + u;
       inc(code);
     end;
    code := 0;
  end;
{$endif CPU16 or CPU8}

{$ifndef FPUNONE}
Function fpc_Val_Real_ShortStr(const s : shortstring; out Code : ValSInt): ValReal; [public, alias:'FPC_VAL_REAL_SHORTSTR']; compilerproc;
begin
    fpc_Val_Real_ShortStr := val_real( s, code );
end;
{$endif FPUNONE}

{$ifndef FPC_STR_ENUM_INTERN}
function fpc_val_enum_shortstr(str2ordindex:pointer;const s:shortstring;out code:valsint):longint; [public, alias:'FPC_VAL_ENUM_SHORTSTR']; compilerproc;

type  Psorted_array=^Tsorted_array;
      Tsorted_array={$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif}record
        o:longint;
        s:Pstring;
      end;

      Pstring_to_ord=^Tstring_to_ord;
      Tstring_to_ord={$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif}record
        count:longint;
        data:array[0..0] of Tsorted_array;
      end;

var l,r,l2,r2,m,sp,isp:SizeInt;
    c:char;

begin
  {Val for numbers accepts spaces at the start, so lets do the same
   for enums. Skip spaces at the start of the string.}
  sp:=1;
  while (sp<=length(s)) and (s[sp]=' ')  do
    inc(sp);

  { Let input be “abd” and sorted names be: _hm a aa ab aba abb abc abd ac ad b c
    Start:                                  L                                    ┘R (R points PAST the last item in the range.)
    After iteration 0 (“a” analyzed):           L                            ┘R
    After iteration 1 (“ab” analyzed):               L                 ┘R
    After iteration 2 (“abd” analyzed):                             L  ┘R }
  l:=0;
  r:=Pstring_to_ord(str2ordindex)^.count;
  dec(sp); { sp/isp are incremented at the beginning of the loop so that 'continue's advance sp/isp. }
  isp:=0; { isp is the position without spaces. }
  repeat
    inc(sp);
    if sp>length(s) then
      break;
    inc(isp);
    c:=UpCase(s[sp]);
    { Among all strings beginning with, say, ‘ab’, the ‘ab’ itself will be the first.
      So after this check, “isp ≤ length(any string in the range)” is guaranteed. }
    if isp>length(Psorted_array(Pstring_to_ord(str2ordindex)^.data)[l].s^) then
      begin
        inc(l);
        if l=r then
          break;
      end;
    if UpCase(Psorted_array(Pstring_to_ord(str2ordindex)^.data)[l].s^[isp])=c then { Shortcut: L may be already correct (enums often have common prefixes). }
      begin
        if l+1=r then { Shortcut: the only string left (enums often have different suffixes). }
          continue;
      end
    else
      begin
        r2:=r; { Search for new L. }
        repeat
          m:=SizeUint(l+r2) div 2;
          if UpCase(Psorted_array(Pstring_to_ord(str2ordindex)^.data)[m].s^[isp])<c then
            l:=m+1
          else
            r2:=m;
        until l=r2;
        if l=r then
          break;
      end;
    if UpCase(Psorted_array(Pstring_to_ord(str2ordindex)^.data)[r-1].s^[isp])=c then { Shortcut: R−1 may be already correct. }
      continue;
    l2:=l; { Search for new R. }
    repeat
      m:=SizeUint(l2+r) div 2;
      if UpCase(Psorted_array(Pstring_to_ord(str2ordindex)^.data)[m].s^[isp])<=c then
        l2:=m+1
      else
        r:=m;
    until l2=r;
    if l=r then { Better not to make it the loop condition, or ‘continue’s may jump to it instead of the beginning. }
      break;
  until false;
  if (l<r) and (isp=length(Psorted_array(Pstring_to_ord(str2ordindex)^.data)[l].s^)) then
    begin
      code:=0;
      exit(Psorted_array(Pstring_to_ord(str2ordindex)^.data)[l].o);
    end;
  code:=sp;
  result:=-1; { Formally undefined, but −1 is very likely the invalid value prone to crashing, which is better than accidentally working. }
end;

{Redeclare fpc_val_enum_shortstr for internal use in the system unit.}
function fpc_val_enum_shortstr(str2ordindex:pointer;const s:shortstring;out code:valsint):longint;external name 'FPC_VAL_ENUM_SHORTSTR';
{$endif FPC_STR_ENUM_INTERN}

function fpc_Val_Currency_ShortStr(const s : shortstring; out Code : ValSInt): currency; [public, alias:'FPC_VAL_CURRENCY_SHORTSTR']; compilerproc;
{$ifdef EXCLUDE_COMPLEX_PROCS}
begin
  runerror(217);
end;
{$else EXCLUDE_COMPLEX_PROCS}
const
  MinInt64 : Int64  =-$8000000000000000;
  MinInt64Edge : Int64 = (-$8000000000000000 + 10) div 10;
var
  { to enable taking the address on the JVM target }
  res : array[0..0] of Int64;
  i,j,power,sign,len : longint;
  FracOverflow : boolean;
begin
  fpc_Val_Currency_ShortStr:=0;
  res[0]:=0;
  len:=Length(s);
  Code:=1;
  sign:=-1;
  power:=0;
  while True do
    if Code > len then
      exit
    else
      if s[Code] in [' ', #9] then
        Inc(Code)
      else
        break;
  { Read sign }
  case s[Code] of
   '+' : begin
           Inc(Code);
         end;
   '-' : begin
           sign:=+1;
           Inc(Code);
         end;
  end;
  { Read digits }
  FracOverflow:=False;
  i:=0;
  while Code <= len do
    begin
      case s[Code] of
        '0'..'9':
          begin
            j:=Ord(s[code])-Ord('0');
            { check overflow }
            if (res[0] >= MinInt64Edge) or (res[0] >= (MinInt64 + j) div 10) then
              begin
                res[0]:=res[0]*10 - j;
                Inc(i);
              end
            else
              if power = 0 then
                { exit if integer part overflow }
                exit
              else
                begin
                  if not FracOverflow and (j >= 5) and (res[0] > MinInt64) then
                    { round if first digit of fractional part overflow }
                    Dec(res[0]);
                  FracOverflow:=True;
                end;
          end;
        '.':
          begin
            if power = 0 then
              begin
                power:=1;
                i:=0;
              end
            else
              exit;
          end;
        else
          break;
      end;
      Inc(Code);
    end;
  if (i = 0) and (power = 0) then
    exit;
  if power <> 0 then
    power:=i;
  power:=4 - power;
  { Exponent? }
  if Code <= len then
    if s[Code] in ['E', 'e'] then
      begin
        Inc(Code);
        if Code > len then
          exit;
        i:=1;
        case s[Code] of
          '+':
            Inc(Code);
          '-':
            begin
              i:=-1;
              Inc(Code);
            end;
        end;
        { read exponent }
        j:=0;
        while Code <= len do
          if s[Code] in ['0'..'9'] then
            begin
              if j > 4951 then
                exit;
              j:=j*10 + (Ord(s[code])-Ord('0'));
              Inc(Code);
            end
          else
            exit;
        power:=power + j*i;
      end
    else
      exit;

  if power > 0 then
    begin
      for i:=1 to power do
        if res[0] >= MinInt64 div 10 then
          res[0]:=res[0]*10
        else
          exit;
    end
  else
    for i:=1 to -power do
      begin
        if res[0] >= MinInt64 + 5 then
          Dec(res[0], 5);
        res[0]:=res[0] div 10;
      end;

  if sign <> 1 then
    if res[0] > MinInt64 then
      res[0]:=res[0]*sign
    else
      exit;

  fpc_Val_Currency_ShortStr:=PCurrency(@res[0])^;
  Code:=0;
end;
{$endif EXCLUDE_COMPLEX_PROCS}
