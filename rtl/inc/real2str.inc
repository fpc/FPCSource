{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1997 by Michael Van Canneyt,
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type

  treal_type = (rt_s64real,rt_s32real,rt_f32bit,rt_s80real,rt_s64bit);
  { corresponding to real    single     fixed   extended and comp for i386 }

{$ifdef i386}
  {$ifdef DEFAULT_EXTENDED}
    bestreal = extended;
  {$else}
    bestreal = double;
  {$endif DEFAULT_EXTENDED}
{$else i386}
  bestreal = single;
{$endif i386}

Procedure str_real (len,f : longint; d : bestreal; real_type :treal_type; var s : string);
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

var correct : longint;  { Power correction }
    currprec : longint;
    roundcorr : bestreal;
    temp : string;
    power : string[10];
    sign : boolean;
    i : integer;
    dot : byte;

begin
  case real_type of
    rt_s64real :
      begin
         maxlen:=23;
         minlen:=9;
         explen:=5;
      end;
    rt_s32real :
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
    rt_s80real :
      begin
         maxlen:=26;
         minlen:=10;
         explen:=6;
      end;
    rt_s64bit  :
      begin
         maxlen:=22;
         minlen:=9;
         { according to TP (was 5) (FK) }
         explen:=6;
      end;
    end;
  { check parameters }
  { default value for length is -32767 }
  if len=-32767 then len:=maxlen;
  { determine sign. before precision, needs 2 less calls to abs() }
  sign:=d<0;
  { the creates a cannot determine which overloaded function to call
  if d is extended !!!
  we should prefer real_to_real on real_to_longint !!
  corrected in compiler }

  {  d:=abs(d); this converts d to double so we loose precision }
  { for the same reason I converted d:=frac(d) to d:=d-int(d); (PM) }
  if sign then d:=-d;
  { determine precision : maximal precision is : }
  currprec:=maxlen-explen-3;
  { this is also the maximal number of decimals !!}
  if f>currprec then f:=currprec;
  { when doing a fixed-point, we need less characters.}
  if (f<0) or ( (d<>0) and ((d>maxexp) or (d<minexp))) then
    begin
    { determine maximal number of decimals }
    if (len>=0) and (len<minlen) then len:=minlen;
    if (len>0) and (len<maxlen) then
      currprec:=len-explen-3;
    end;
  { convert to standard form. }
  correct:=0;
  if d>=10.0 then
    while d>=10.0 do
      begin
      d:=d/10.0;
      inc(correct);
      end
  else if (d<1) and (d<>0) then
    while d<1 do
      begin
      d:=d*10.0;
      dec(correct);
      end;
  { RoundOff }
  roundcorr:=0.5;
  if f<0 then
    for i:=1 to currprec do roundcorr:=roundcorr/10
  else
    begin
      if correct+f<0 then
       begin
         for i:=1 to abs(correct+f) do
          roundcorr:=roundcorr*10;
       end
      else
       begin
         for i:=1 to correct+f do
          roundcorr:=roundcorr/10;
       end;
    end;
  d:=d+roundcorr;
  { 0.99 + 0.05 > 1.0 ! Fix this by dividing the results >=10 first (PV) }
  while (d>=10.0) do
   begin
     d:=d/10.0;
     inc(correct);
   end;
  { Now we have a standard expression : sign d *10^correct
    where  1<d<10 or d=0 ... }
  { get first character }
  if sign then
    temp:='-'
  else
    temp:=' ';
  temp:=temp+chr(ord('0')+trunc(d));
  d:=d-int(d);
  { Start making the string }
  for i:=1 to currprec do
    begin
    d:=d*10.0;
    temp:=temp+chr(ord('0')+trunc(d));
    d:=d-int(d);
    end;
  { Now we need two different schemes for the different
    representations. }
  if (f<0) or (correct>maxexp) then
    begin
    insert ('.',temp,3);
    str(abs(correct),power);
    if length(power)<explen-2 then
      power:=copy(zero,1,explen-2-length(power))+power;
    if correct<0 then power:='-'+power else power:='+'+power;
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
    {correct length to fit precision.}
      if f>0 then
       temp[0]:=chr(pos('.',temp)+f)
      else
       temp[0]:=chr(pos('.',temp)-1);
    end;
  if length(temp)<len then
    s:=space(len-length(temp))+temp
  else
    s:=temp;
end;

{
  $Log$
  Revision 1.11  1999-02-16 00:49:20  peter
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
