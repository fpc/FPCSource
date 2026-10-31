{ unit for tb0732: implementation-section variables and typed constants
  get hidden visibility, on aarch64 they are accessed directly instead of
  through the GOT }
unit ub0732;

{$mode objfpc}

interface

type
  trec = record
    a, b, c, d: longint;
  end;

procedure setvalues(x: longint);
function sumvalues: int64;
function getaddrsl: plongint;
function gettab(i: longint): longint;
function getsl_inline: longint; inline;
function gettab_inline(i: longint): longint; inline;

implementation

var
  sl: longint;
  sq: int64;
  sb: byte;
  sw: word;
  sd: double;
  sa: array[0..15] of int64;
  sr: trec;

const
  tab: array[0..7] of longint = (1,2,3,4,5,6,7,8);
  tq: int64 = $123456789;

procedure setvalues(x: longint);
  var
    i: longint;
  begin
    sl:=x;
    sq:=int64(x)*tq;
    sb:=x;
    sw:=x+1;
    sd:=x*0.5;
    for i:=low(sa) to high(sa) do
      sa[i]:=i*x;
    sr.a:=x;
    sr.b:=x+1;
    sr.c:=sr.a+sr.b;
    sr.d:=tab[x and 7];
  end;

function sumvalues: int64;
  var
    i: longint;
  begin
    result:=sl+sq+sb+sw+trunc(sd)+sr.a+sr.b+sr.c+sr.d;
    for i:=low(sa) to high(sa) do
      inc(result,sa[i]);
  end;

function getaddrsl: plongint;
  begin
    result:=@sl;
  end;

function gettab(i: longint): longint;
  begin
    result:=tab[i and 7];
  end;

function getsl_inline: longint; inline;
  begin
    result:=sl;
  end;

function gettab_inline(i: longint): longint; inline;
  begin
    result:=tab[i and 7];
  end;

end.
