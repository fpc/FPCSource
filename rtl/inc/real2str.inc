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
{$ifdef ver_above0_9_ still not ok }
  bestreal  = extended; { still gives problems }
{$else ver_above0_9_8}
  bestreal = double;
{$endif ver_above0_9_8}
{$else not i386}
  bestreal = single;
{$endif not i386}

Procedure str_real (len,f : longint; d : bestreal; real_type :treal_type; var s : string);

{ These numbers are for the double type...
  At the moment these are mapped onto a double but this may change
  in the future ! }

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
         maxlen:=24;
         minlen:=9;
         explen:=5;
      end;
    end;
  { check parameters }
  { default value for length is -32767 }
{$ifdef ver_above0_9_7}
  if len=-32767 then len:=maxlen;
{$else }
  if (len=-1) and (f=-1) then len:=maxlen;
{$endif }
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
  if (f<0) or ((d>maxexp) or (d<minexp)) then
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
    for i:=1 to correct+f do roundcorr:=roundcorr/10;
  d:=d+roundcorr;
  { 0.99 + 0.05 > 10.0 ! Fix this by dividing the results >=10 first (PV) }
  if d>=10.0 then
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
  Revision 1.1  1998-03-25 11:18:43  root
  Initial revision

  Revision 1.7  1998/03/16 23:38:17  peter
    * fixed 0.997:0:2 bugs

  Revision 1.6  1998/01/26 11:59:47  michael
  + Added log at the end


  revision 1.5
  date: 1998/01/05 00:48:24;  author: carl;  state: Exp;  lines: +2 -2
  + Now compatible with m68k floating point types
  ----------------------------
  revision 1.4
  date: 1997/12/02 17:44:45;  author: pierre;  state: Exp;  lines: +2 -2
    * use of extended type in function str_real still buggy
  ----------------------------
  revision 1.3
  date: 1997/12/01 12:08:04;  author: michael;  state: Exp;  lines: +12 -6
  + added copyright reference header.
  ----------------------------
  revision 1.2
  date: 1997/11/28 19:45:21;  author: pierre;  state: Exp;  lines: +6 -3
    * one more bug fix with namelength
    + fixed math in fixed_math define (does not compile yet)
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:47;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:47;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
}
