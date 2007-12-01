{
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             documentation by Michael van Canneyt (Michael@freepascal.org)

    Basic In and output of matrix and vector types. Maybe too simple for
    your application, but still handy for logging and debugging.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit iom;

interface
{$I direct.inc}

uses typ;

const
    npos  : ArbInt = 78;

{Read a n-dimensional vector v from textfile}
procedure iomrev(var inp: text; var v: ArbFloat; n: ArbInt);

{Read a m x n-dimensional matrix a from textfile}
procedure iomrem(var inp: text; var a: ArbFloat; m, n, rwidth: ArbInt);

{Write a n-dimensional vectorv v to textfile}
procedure iomwrv(var out: text; var v: ArbFloat; n, form: ArbInt);

{Write a m x n-dimensional matrix a to textfile}
procedure iomwrm(var out: text; var a: ArbFloat; m, n, rwidth, form: ArbInt);

{Read a m x n-dimensional matrix a from string}
procedure iomrems(inp: ArbString; var a: ArbFloat; var m, n: ArbInt; c: ArbInt);

{Write a m x n-dimensional matrix a to string}
procedure iomwrms(var out: ArbString; var a: ArbFloat; m, n, form, c: ArbInt);

implementation

procedure iomrev(var inp: text; var v: ArbFloat; n: ArbInt);

var pv : ^arfloat1;
     i : ArbInt;

BEGIN
  pv:=@v; for i:=1 to n do read(inp, pv^[i])
END {iomrev};

procedure iomrem(var inp: text; var a: ArbFloat; m, n, rwidth: ArbInt);

var    pa : ^arfloat1;
     i, k : ArbInt;

BEGIN
  pa:=@a; k:=1;
  for i:=1 to m do
    BEGIN
      iomrev(inp, pa^[k], n); Inc(k, rwidth)
    END
END {iomrem};

procedure iomwrv(var out: text; var v: ArbFloat; n, form: ArbInt);

var  pv     : arfloat1 absolute v;
     i, i1  : ArbInt;
BEGIN
  if form>maxform then form:=maxform  else
  if form<minform then form:=minform;
  i1:=npos div (form+2);
  for i:=1 to n do
  if ((i mod i1)=0) or (i=n) then writeln(out, pv[i]:form)
                             else write(out, pv[i]:form, '':2)
END {iomwrv};

procedure iomwrm(var out: text; var a: ArbFloat; m, n, rwidth, form: ArbInt);
var  pa                            : ^arfloat1;
     i, k, nb, i1, l, j, r, l1, kk : ArbInt;
BEGIN
  if (n<1) or (m<1) then exit;
  pa:=@a;
  if form>maxform then form:=maxform else
  if form<minform then form:=minform;
  i1:=npos div (form+2); l1:=0;
  nb:=n div i1; r:=n mod i1;
  if r>0 then Inc(nb);
  for l:=1 to nb do
    BEGIN
      k:=l1+1; if (r>0) and (l=nb) then i1:=r;
      for i:=1 to m do
        BEGIN
          kk:=k;
          for j:=1 to i1-1 do
            BEGIN
              write(out, pa^[kk]:form, '':2); Inc(kk)
            END;
          writeln(out, pa^[kk]:form); Inc(k, rwidth)
        END;
      Inc(l1, i1); if l<nb then writeln(out)
    END;
END {iomwrm};

procedure iomrems(inp: ArbString; var a: ArbFloat; var m, n: ArbInt; c: ArbInt);
var
  pa: ^arfloat1;
  i, k: ArbInt;
  err: ArbInt;
  s: ArbString;
  ni: ArbInt;
  ci: ArbInt;
begin
  pa:=@a;
  
  k:=1;
  m:=0;
  n:=0;
  
  //parse the text
  i:= 1;
  while i < Length(inp) do
  begin
    ni := 1;
    ci := 1;

    //parse row
    while not (inp[i] in ['}']) do
    begin

      //go to beginning of row values
      while inp[i] in ['{',' '] do
      begin
        //increase row counter
        if inp[i] = '{' then
          Inc(m);
        Inc(i);
      end;

      //get value from string
      s := '';
      while inp[i] in ['0'..'9','E','e','+','-'] do
      begin
        s := s + inp[i];
        Inc(i);
      end;

      //assign value to element
      val(s, pa^[k], err);
      Inc(k);
      if err <> 0 then
        writeln('Val(',s,') failed at position ', err);
        
      Inc(ci);
    end;

    k := ((k div c) + 1) * c + 1;
    
    Inc(ni);
    if ni > n then n := ni;

    Inc(i);
  end;

end;

procedure iomwrms(var out: ArbString; var a: ArbFloat; m, n, form, c: ArbInt);
var
  pa: ^arfloat1;
  i, l, kk: ArbInt;
  s: string;
BEGIN
  if (n<1) or (m<1) then
    exit;

  pa:=@a;

  if form>maxform then
    form:=maxform
  else
    if form<minform then
      form:=minform;

  kk := 1;
  for l:=1 to m do
  BEGIN
    out := out + '{';

    for i:=1 to n do
    BEGIN
      str(pa^[kk]:form, s);
      Inc(kk);
      
      if i <> n then
        out := out + s + ' '
      else
        out := out + s;
    END;
    kk := ((kk div c) + 1) * c + 1;
    out := out + ' }';
  end;
end;

END.
