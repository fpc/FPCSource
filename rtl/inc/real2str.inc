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

const
   { do not use real constants else you get rouding errors }
   i10 : longint = 10;
   i2  : longint = 2;
   i1  : longint = 1;
(*
   { we can use this conditional if the Inf const is defined
     in processor specific code PM }
{$ifdef FPC_HAS_INFINITY_CONST}
   {$define FPC_INFINITY_FOR_REAL2STR}
{$else not  FPC_HAS_INFINITY_CONST}
   { To avoid problems with infinity just
     issue it in byte representation to be endianness independant PM }
{$ifndef FPC_INFINITY_FOR_REAL2STR}
{$ifdef SUPPORT_EXTENDED}
   { extended is not IEEE so its processor specific
     so I only allow it for i386 PM }
{$ifdef i386}
   {$define FPC_INFINITY_FOR_REAL2STR}
   InfArray : {extended} array[0..9] of byte = ($0,$0,$0,$0,$0,$0,$0,$80,$ff,$7f);
{$endif i386}
{$endif SUPPORT_EXTENDED}
{$endif not FPC_INFINITY_FOR_REAL2STR}

{$ifndef FPC_INFINITY_FOR_REAL2STR}
{$ifdef SUPPORT_DOUBLE}
   {$define FPC_INFINITY_FOR_REAL2STR}
   InfArray : {double} array[0..9] of byte = ($0,$0,$0,$0,$0,$0,$f0,$7f);
{$endif SUPPORT_DOUBLE}
{$endif not FPC_INFINITY_FOR_REAL2STR}

{$ifndef FPC_INFINITY_FOR_REAL2STR}
{$ifdef SUPPORT_SINGLE}
   {$define FPC_INFINITY_FOR_REAL2STR}
   InfArray : {single} array[0..3] of byte = ($0,$0,$80,$7f);
{$endif SUPPORT_SINGLE}
{$endif not FPC_INFINITY_FOR_REAL2STR}

{$ifndef FPC_INFINITY_FOR_REAL2STR}
    {$warning don't know Infinity values }
{$endif not FPC_INFINITY_FOR_REAL2STR}
{$endif not FPC_HAS_INFINITY_CONST}
*)
Procedure str_real (len,f : longint; d : ValReal; real_type :treal_type; var s : string);
{
  These numbers are for the double type...
  At the moment these are mapped onto a double but this may change
  in the future !
}
var  maxlen : longint;   { Maximal length of string for float }
     minlen : longint;   { Minimal length of string for float }
     explen : longint;   { Length of exponent, including E and sign.
                           Must be strictly larger than 2 }
const
      maxexp = 1e+35;   { Maximum value for decimal expressions }
      minexp = 1e-35;   { Minimum value for decimal expressions }
      zero   = '0000000000000000000000000000000000000000';

type
{$ifdef SUPPORT_EXTENDED}
  TSplitExtended = packed record
    case byte of
      0: (bytes: Array[0..9] of byte);
      1: (words: Array[0..4] of word);
      2: (cards: Array[0..1] of cardinal; w: word);
  end;
{$else}
{$ifdef SUPPORT_DOUBLE}
  TSplitDouble = packed record
    case byte of
      0: (bytes: Array[0..7] of byte);
      1: (words: Array[0..3] of word);
      2: (cards: Array[0..1] of cardinal);
  end;
{$else}
{$ifdef SUPPORT_SINGLE}
  TSplitSingle = packed record
    case byte of
      0: (bytes: Array[0..3] of byte);
      1: (words: Array[0..1] of word);
      2: (cards: Array[0..0] of cardinal);
  end;
{$endif SUPPORT_SINGLE}
{$endif SUPPORT_DOUBLE}
{$endif SUPPORT_EXTENDED}

var correct : longint;  { Power correction }
    currprec : longint;
    il,il2,roundcorr : Valreal;
    temp : string;
    power : string[10];
    sign : boolean;
    i : integer;
    dot : byte;
    currp : pchar;
    mantZero, expMaximal: boolean;
begin
  case real_type of
    rt_s32real :
      begin
         maxlen:=16;
         minlen:=8;
         explen:=4;
      end;
    rt_s64real :
      begin
         maxlen:=23;
         minlen:=9;
         explen:=5;
      end;
    rt_s80real :
      begin
         maxlen:=26;
         minlen:=10;
         explen:=6;
      end;
    rt_c64bit  :
      begin
         maxlen:=22;
         minlen:=9;
         { according to TP (was 5) (FK) }
         explen:=6;
      end;
    rt_f16bit  :
      begin
         maxlen:=16;
         minlen:=8;
         explen:=4;
      end;
    rt_f32bit  :
      begin
         maxlen:=16;
         minlen:=8;
         explen:=4;
      end;
    end;
  { check parameters }
  { default value for length is -32767 }
  if len=-32767 then
    len:=maxlen;
  { determine sign. before precision, needs 2 less calls to abs() }
{  sign:=d<0;}
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
  {$error No floating type supported for real2str}
{$endif SUPPORT_SINGLE}
{$endif SUPPORT_DOUBLE}
{$endif SUPPORT_EXTENDED}
{$else big_endian}
  {$error NaN/Inf not yet supported for big endian machines in str_real}
{$endif big_endian}
  if expMaximal then
    if mantZero then
      if sign then
        temp := '-Inf'
      else temp := 'Inf'
    else temp := 'NaN'
  else
    begin
      { the creates a cannot determine which overloaded function to call
      if d is extended !!!
      we should prefer real_to_real on real_to_longint !!
      corrected in compiler }

      {  d:=abs(d); this converts d to double so we loose precision }
      { for the same reason I converted d:=frac(d) to d:=d-int(d); (PM) }
      if sign then
        d:=-d;
(*
    {$ifdef FPC_INFINITY_FOR_REAL2STR}
    {$ifndef FPC_HAS_INFINITY_CONST}
        if d=ValReal(InfArray) then
    {$else FPC_HAS_INFINITY_CONST}
        if d=Inf then
    {$endif FPC_HAS_INFINITY_CONST}
          begin
            if sign then
              s:='-Inf'
            else
              s:='Inf';
            exit;
          end;
    {$endif FPC_INFINITY_FOR_REAL2STR}
*)      
      { determine precision : maximal precision is : }
      currprec:=maxlen-explen-3;
      { this is also the maximal number of decimals !!}
      if f>currprec then
        f:=currprec;
      { when doing a fixed-point, we need less characters.}
      if (f<0) or ( (d<>0) and ((d>maxexp) or (d<minexp))) then
        begin
        { determine maximal number of decimals }
          if (len>=0) and (len<minlen) then
            len:=minlen;
          if (len>0) and (len<maxlen) then
            currprec:=len-explen-3;
        end;
      { convert to standard form. }
      correct:=0;
      if d>=i10 then
       begin
         il:=i1;
         il2:=i10;
         repeat
           il:=il2;
           il2:=il*i10;
           inc(correct);
         until (d<il2);
         d:=d/il;
       end
      else
       if (d<1) and (d<>0) then
        begin
          while d<1 do
           begin
             d:=d*i10;
             dec(correct);
           end;
        end;
      { RoundOff }
      roundcorr:=extended(i1)/extended(i2);
      if f<0 then
        for i:=1 to currprec do roundcorr:=roundcorr/i10
      else
        begin
          if correct+f<0 then
           begin
             for i:=1 to abs(correct+f) do
              roundcorr:=roundcorr*i10;
           end
          else
           begin
             for i:=1 to correct+f do
              roundcorr:=roundcorr/i10;
           end;
        end;
      d:=d+roundcorr;
      { 0.99 + 0.05 > 1.0 ! Fix this by dividing the results >=10 first (PV) }
      while (d>=10.0) do
       begin
         d:=d/i10;
         inc(correct);
       end;
      { Now we have a standard expression : sign d *10^correct
        where  1<d<10 or d=0 ... }
      { get first character }
      currp:=pchar(@temp[1]);
      if sign then
        currp^:='-'
      else
        currp^:=' ';
      inc(currp);
      currp^:=chr(ord('0')+trunc(d));
      inc(currp);
      d:=d-int(d);
      { Start making the string }
      for i:=1 to currprec do
       begin
         d:=d*i10;
         currp^:=chr(ord('0')+trunc(d));
         inc(currp);
         d:=d-int(d);
       end;
      temp[0]:=chr(currp-pchar(@temp[1]));
      { Now we need two different schemes for the different
        representations. }
      if (f<0) or (correct>maxexp) then
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
             delete (temp,1,1);
             dot:=2;
           end
          else
           dot:=3;
          { set zeroes and dot }
          if correct>=0 then
           begin
             if length(temp)<correct+dot+f then
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
           temp[0]:=chr(pos('.',temp)+f)
          else
           temp[0]:=chr(pos('.',temp)-1);
        end;
    end;
  if length(temp)<len then
    s:=space(len-length(temp))+temp
  else
    s:=temp;
end;

{
  $Log$
  Revision 1.21  2000-02-09 12:17:51  peter
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

  Revision 1.13  1999/05/06 09:05:12  peter
    * generic write_float str_float

  Revision 1.12  1999/03/10 21:49:02  florian
    * str and val for extended use now int constants to minimize
      rounding error

  Revision 1.11  1999/02/16 00:49:20  peter
    * fixed rounding when correct+f < 0

  Revision 1.10  1998/08/11 21:39:06  peter
    * splitted default_extended from support_extended

  Revision 1.9  1998/08/11 00:05:25  peter
    * $ifdef ver0_99_5 updates

  Revision 1.8  1998/08/10 15:56:30  peter
    * fixed 0_9_5 typo

  Revision 1.7  1998/08/08 12:28:12  florian
    * a lot small fixes to the extended data type work

  Revision 1.6  1998/07/18 17:14:22  florian
    * strlenint type implemented

  Revision 1.5  1998/07/13 21:19:10  florian
    * some problems with ansi string support fixed

  Revision 1.4  1998/06/18 08:15:33  michael
  + Fixed error when printing zero. len was calculated wron.

  Revision 1.3  1998/05/12 10:42:45  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

  Revision 1.2  1998/04/07 22:40:46  florian
    * final fix of comp writing
}
