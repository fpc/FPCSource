{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  { See symdefh.inc tfloattyp }
  treal_type = (rt_s32real,rt_s64real,rt_s80real,rt_c64bit,rt_f16bit,rt_f32bit);
  { corresponding to single   double   extended   fixed      comp for i386 }

Procedure str_real (len,f : longint; d : ValReal; real_type :treal_type; var s : string);
{$ifdef SUPPORT_EXTENDED}
type
  TSplitExtended = packed record
    case byte of
      0: (bytes: Array[0..9] of byte);
      1: (words: Array[0..4] of word);
      2: (cards: Array[0..1] of cardinal; w: word);
  end;
const
  maxDigits = 17;
{$else}
{$ifdef SUPPORT_DOUBLE}
type
  TSplitDouble = packed record
    case byte of
      0: (bytes: Array[0..7] of byte);
      1: (words: Array[0..3] of word);
      2: (cards: Array[0..1] of cardinal);
  end;
const
  maxDigits = 14;
{$else}
{$ifdef SUPPORT_SINGLE}
type
  TSplitSingle = packed record
    case byte of
      0: (bytes: Array[0..3] of byte);
      1: (words: Array[0..1] of word);
      2: (cards: Array[0..0] of cardinal);
  end;
const
  maxDigits = 9;
{$endif SUPPORT_SINGLE}
{$endif SUPPORT_DOUBLE}
{$endif SUPPORT_EXTENDED}

type
  { the value in the last position is used for rounding }
  TIntPartStack = array[1..maxDigits+1] of valReal;

var
  roundCorr, corrVal: valReal;
  intPart, spos, endpos, fracCount: longint;
  correct, currprec: longint;
  temp : string;
  power : string[10];
  sign : boolean;
  dot : byte;
  mantZero, expMaximal: boolean;

  procedure RoundStr(var s: string; lastPos: byte);
  var carry: longint;
  begin
    carry := 1;
    repeat
      s[lastPos] := chr(ord(s[lastPos])+carry);
      carry := 0;
      if s[lastPos] > '9' then
        begin
          s[lastPos] := '0';
          carry := 1;
        end;
      dec(lastPos);
    until carry = 0;
  end;

  procedure getIntPart(d: extended);
  var
    intPartStack: TIntPartStack;
    stackPtr, endStackPtr, digits: longint;
    overflow: boolean;
  begin
    { position in the stack (gets increased before first write) }
    stackPtr := 0;
    { number of digits processed }
    digits := 0;
    { did we wrap around in the stack? Necessary to know whether we should round }
    overflow :=false;
    { generate a list consisting of d, d/10, d/100, ... until d < 1.0 }
    while d > 1.0-roundCorr do
      begin
        inc(stackPtr);
        inc(digits);
        if stackPtr > maxDigits+1 then
          begin
            stackPtr := 1;
            overflow := true;
          end;
        intPartStack[stackPtr] := d;
        d := d / 10.0;
      end;
    { if no integer part, exit }
    if digits = 0 then
      exit;
    endStackPtr := stackPtr+1;
    if endStackPtr > maxDigits + 1 then
      endStackPtr := 1;
 { now, all digits are calculated using trunc(d*10^(-n)-int(d*10^(-n-1))*10) }
    corrVal := 0.0;
 { the power of 10 with which the resulting string has to be "multiplied" }
 { if the decimal point is placed after the first significant digit       }
    correct := digits-1;
    repeat
      if (currprec > 0) then
        begin
          intPart:= trunc(intPartStack[stackPtr]-corrVal);
          dec(currPrec);
          inc(spos);
          temp[spos] := chr(intPart+ord('0'));
          if temp[spos] > '9' then
            begin
              temp[spos] := chr(ord(temp[spos])-10);
              roundStr(temp,spos-1);
            end;
        end;
      corrVal := int(intPartStack[stackPtr]) * 10.0;
      dec(stackPtr);
      if stackPtr = 0 then
        stackPtr := maxDigits+1;
    until (overflow and (stackPtr = endStackPtr)) or
          (not overflow and (stackPtr = maxDigits+1)) or (currPrec = 0);
    { round if we didn't use all available digits yet and if the }
    { remainder is > 5                                           }
    if overflow  and
       (trunc(intPartStack[stackPtr]-corrVal) > 5.0 - roundCorr) then
      roundStr(temp,spos);
  end;

var  maxlen : longint;   { Maximal length of string for float }
     minlen : longint;   { Minimal length of string for float }
     explen : longint;   { Length of exponent, including E and sign.
                           Must be strictly larger than 2 }
const
      maxexp = 1e+35;   { Maximum value for decimal expressions }
      minexp = 1e-35;   { Minimum value for decimal expressions }
      zero   = '0000000000000000000000000000000000000000';

begin
  case real_type of
    rt_s32real :
      begin
         maxlen:=16;
         minlen:=8;
         explen:=4;
         { correction used with comparing to avoid rounding/precision errors }
         roundCorr := (1/exp((16-4-3)*ln(10)));
      end;
    rt_s64real :
      begin
{ if the maximum suppported type is double, we can print out one digit }
{ less, because otherwise we can't round properly and 1e-400 becomes   }
{ 0.99999999999e-400 (JM)                                              }
{$ifdef support_extended}
         maxlen:=23;
         { correction used with comparing to avoid rounding/precision errors }
         roundCorr := (1/exp((23-5-3)*ln(10)));
{$else support_extended}
{$ifdef support_double}
         maxlen := 22;
         { correction used with comparing to avoid rounding/precision errors }
         roundCorr := (1/exp((22-4-3)*ln(10)));
{$endif support_double}
{$endif support_extended}
         minlen:=9;
         explen:=5;
      end;
    rt_s80real :
      begin
         maxlen:=26;
         minlen:=10;
         explen:=6;
         { correction used with comparing to avoid rounding/precision errors }
         roundCorr := (1/exp((26-6-3)*ln(10)));
      end;
    rt_c64bit  :
      begin
         maxlen:=22;
         minlen:=9;
         { according to TP (was 5) (FK) }
         explen:=6;
         { correction used with comparing to avoid rounding/precision errors }
         roundCorr := (1/exp((22-6-3)*ln(10)));
      end;
    rt_f16bit  :
      begin
         maxlen:=16;
         minlen:=8;
         explen:=4;
         { correction used with comparing to avoid rounding/precision errors }
         roundCorr := (1/exp((16-4-3)*ln(10)));
      end;
    rt_f32bit  :
      begin
         maxlen:=16;
         minlen:=8;
         explen:=4;
         { correction used with comparing to avoid rounding/precision errors }
         roundCorr := (1/exp((16-4-3)*ln(10)));
      end;
    end;
  { check parameters }
  { default value for length is -32767 }
  if len=-32767 then
    len:=maxlen;
  { determine sign. before precision, needs 2 less calls to abs() }
{$ifndef big_endian}
{$ifdef SUPPORT_EXTENDED}
  { extended, format (MSB): 1 Sign bit, 15 bit exponent, 64 bit mantissa }
  sign := (TSplitExtended(d).w and $8000) <> 0;
  expMaximal := (TSplitExtended(d).w and $7fff) = 32767;
  mantZero := (TSplitExtended(d).cards[0] = 0) and
                  (TSplitExtended(d).cards[1] = 0);
{$else SUPPORT_EXTENDED}
{$ifdef SUPPORT_DOUBLE}
  { double, format (MSB): 1 Sign bit, 11 bit exponent, 52 bit mantissa }
  sign := ((TSplitDouble(d).cards[1] shr 20) and $800) <> 0;
  expMaximal := ((TSplitDouble(d).cards[1] shr 20) and $7ff) = 2047;
  mantZero := (TSplitDouble(d).cards[1] and $fffff = 0) and
              (TSplitDouble(d).cards[0] = 0);
{$else SUPPORT_DOUBLE}
{$ifdef SUPPORT_SINGLE}
  { single, format (MSB): 1 Sign bit, 8 bit exponent, 23 bit mantissa }
  sign := ((TSplitSingle(d).words[1] shr 7) and $100) <> 0;
  expMaximal := ((TSplitSingle(d).words[1] shr 7) and $ff) = 255;
  mantZero := (TSplitSingle(d).cards[0] and $7fffff = 0);
{$else SUPPORT_SINGLE}
  {$error No big endian floating type supported yet in real2str}
{$endif SUPPORT_SINGLE}
{$endif SUPPORT_DOUBLE}
{$endif SUPPORT_EXTENDED}
{$else big_endian}
  {$error sign/NaN/Inf not yet supported for big endian CPU's in str_real}
{$endif big_endian}
  if expMaximal then
    if mantZero then
      if sign then
        temp := '-Inf'
      else temp := 'Inf'
    else temp := 'NaN'
  else
    begin
      {  d:=abs(d); this converts d to double so we loose precision }
      { for the same reason I converted d:=frac(d) to d:=d-int(d); (PM) }
      if sign then
        d:=-d;
      { determine precision : maximal precision is : }
      currPrec := maxlen-explen-2;
      { this is also the maximal number of decimals !!}
      if f>currprec then
        f:=currprec;
      { when doing a fixed-point, we need less characters.}
      if (f<0) {or ((d<>0) and ((d>maxexp) and (d>minexp)))} then
        begin
        { determine maximal number of decimals }
          if (len>=0) and (len<minlen) then
            len:=minlen;
          if (len>0) and (len<maxlen) then
            currprec:=len-explen-2;
        end;

      { leading zero, may be necessary for things like str(9.999:0:2) to }
      { be able to insert an extra character at the start of the string  }
      temp := ' 0';
      { position in the temporary output string }
      spos := 2;
      { get the integer part }
      correct := 0;
      GetIntPart(d);
      { now process the fractional part }
      d := frac(d);
      { if we have to round earlier than the amount of available precision, }
      { only calculate digits up to that point                              }
      if (f >= 0) and (currPrec > f) then
        currPrec := f;
      { if integer part was zero, go to the first significant digit of the }
      { fractional part                                                    }
      { make sure we don't get an endless loop if d = 0                    }
      if (spos = 2) and (d <> 0.0) then
        begin
         { take rounding errors into account }
          while d < 0.1-roundCorr do
            begin
              d := d * 10.0;
              dec(correct);
              { adjust the precision depending on how many digits we  }
              { already "processed" by multiplying by 10, but only if }
              { the amount of precision is specified                  }
              if f >= 0 then
                dec(currPrec);
            end;
          dec(correct);
        end;
      { current length of the output string in endPos }
      endPos := spos;
      { always calculate at least 1 fractional digit for rounding }
      if (currPrec >= 0) then
        begin
          corrVal := 0.5;
          for fracCount := 1 to currPrec do
            corrVal := corrVal / 10.0;
          if d >= corrVal then
            d := d + corrVal;
          if int(d) = 1 then
            begin
              roundStr(temp,spos);
              d := frac(d);
            end;
          { calculate the necessary fractional digits }
          for fracCount := 1 to currPrec do
            begin
              d := frac(d) * 10.0;
              inc(spos);
              temp[spos] := chr(trunc(d)+ord('0'));
              if temp[spos] > '9' then
                { possible because trunc and the "*10.0" aren't exact :( }
                begin
                  temp[spos] := chr(ord(temp[spos]) - 10);
                  roundStr(temp,spos-1);
                end;
            end;
          { new length of string }
          endPos := spos;
        end;
      setLength(temp,endPos);
      { delete leading zero if we didn't need it while rounding at the }
      { string level                                                   }
      if temp[2] = '0' then
        delete(temp,2,1)
      { the rounding caused an overflow to the next power of 10 }
      else inc(correct);
      if sign then
        temp[1] := '-';
      if (f<0) or (correct>(round(ln(maxexp)/ln(10)))) then
        begin
          insert ('.',temp,3);
          str(abs(correct),power);
          if length(power)<explen-2 then
            power:=copy(zero,1,explen-2-length(power))+power;
          if correct<0 then
            power:='-'+power
          else
            power:='+'+power;
          temp:=temp+'E'+power;
        end
      else
        begin
          if not sign then
            begin
              delete(temp,1,1);
              dot := 2
            end
          else
            dot := 3;
          { set zeroes and dot }
          if correct>=0 then
            begin
              if length(temp)<correct+dot+f-1 then
                temp:=temp+copy(zero,1,correct+dot+f-length(temp));
              insert ('.',temp,correct+dot);
            end
          else
            begin
              correct:=abs(correct);
              insert(copy(zero,1,correct),temp,dot-1);
              insert ('.',temp,dot);
            end;
          { correct length to fit precision }
          if f>0 then
            setlength(temp,pos('.',temp)+f)
          else
            setLength(temp,pos('.',temp)-1);
        end;
    end;
    if length(temp)<len then
      s:=space(len-length(temp))+temp
    else s:=temp;
end;

{
  $Log$
  Revision 1.30  2000-03-26 11:36:28  jonas
    + $maxfpuregisters 0 for i386 in systemh (to avoid requiring too much
      empty FPU registers for sysstem routines
    * fixed bug in str_real when using :x:0
    * str_real now doesn't call exp() anymore at runtime, so it should
      require less free FPU registers now (and be slightly faster)

  Revision 1.29  2000/03/21 12:00:30  jonas
    * fixed more bugs due to inexact nature of FPU

  Revision 1.28  2000/03/17 20:20:33  jonas
    * fixed rounding bugs with certain formatting parameters in str_real
    * fixed tbs0218 so it compares both results only until max precision

  Revision 1.27  2000/03/05 09:41:05  jonas
    * fixed rounding problem when writing out single/double type vars

  Revision 1.26  2000/03/02 07:35:57  jonas
    * sign was not written in some cases

  Revision 1.25  2000/02/27 14:41:25  peter
    * removed warnings/notes

  Revision 1.24  2000/02/26 18:53:11  jonas
    * fix for lost precision because sometimes the correction value was
      larger than the number to be corrected
    * incompatibility with TP's output fixed

  Revision 1.23  2000/02/26 15:49:40  jonas
    + new str_real which is completely TP compatible regarding output
      format and which should have no rounding errors anymore

  Revision 1.22  2000/02/09 16:59:31  peter
    * truncated log

  Revision 1.21  2000/02/09 12:17:51  peter
    * moved halt to system.inc
    * syslinux doesn't use direct asm anymore

  Revision 1.20  2000/01/17 13:00:51  jonas
    + support for NaN's, cleaner support for Inf

  Revision 1.19  2000/01/07 16:41:36  daniel
    * copyright 2000

  Revision 1.18  1999/11/28 23:57:23  pierre
   * Infinite loop for infinite value problem fixed

  Revision 1.17  1999/11/03 09:54:24  peter
    * another fix for precision

  Revision 1.16  1999/11/03 00:55:09  pierre
   * problem of last commit for large d values corrected

  Revision 1.15  1999/11/02 15:05:53  peter
    * better precisio by dividing only once with a calculated longint
      instead of multiple times by 10

  Revision 1.14  1999/08/03 21:58:44  peter
    * small speed improvements

}