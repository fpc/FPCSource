Program easter;

{
    easter v1.0
    © 1995 by Andreas Tetzl
    FREEWARE


    This is a little program to calculate the date of
    easter for years between 1583 and 2299.

    Start it in a shell with the year as argument.

}

{
    Translated to fpc pascal.
    21 Mar 2001.

    nils.sjoholm@mailbox.swipnet.se
}

uses amigados;

const version : pchar = '$VER: easter v1.0 (3-Nov-95) by Andreas Tetzl';

VAR i,a,b,c,d,e,m,n : Integer;
    year, month, day : longint;


BEGIN

  if (ParamStr(1) = '?') or (ParamStr(1) = '') then
   BEGIN
    Writeln('YEAR/N');
    halt(20);
   END;

  i:=StrToLong(ParamStr(1),year);
  if (year<1583) or (year>2299) then
   BEGIN
    Writeln('only years between 1583 and 2299 allowed');
    halt(20);
   END;

  Case year of
    1583..1699 : BEGIN m:=22; n:=2; END;
    1700..1799 : BEGIN m:=23; n:=3; END;
    1800..1899 : BEGIN m:=23; n:=4; END;
    1900..2099 : BEGIN m:=24; n:=5; END;
    2100..2199 : BEGIN m:=24; n:=6; END;
    2200..2299 : BEGIN m:=25; n:=0; END;
  end;

  a:=year mod 19;
  b:=year mod 4;
  c:=year mod 7;
  d:=(19*a+m) mod 30;
  e:=(2*b+4*c+6*d+n) mod 7;

  day:=22+d+e;
  if day<=31 then
   month:=3
  else
   BEGIN
    month:=4;
    day:=d+e-9;
   END;

  if (month=4) and (day=26) then day:=19;
  if (month=4) and (day=25) and (d=28) and (e=6) and (a>10) then day:=18;

  Write(year,'-');
  if month=3 then Write('Mar') else Write('Apr');
  Writeln('-',day);
END.
