{ %norun }

{ Note: these things *are* supported now, they just weren't when the test was
  written (the purpose was to make sure the compiler didn't crash when trying
  to compile these things, even though it generated invalid code for them)
}

{$mode delphi}
{$t+}

unit unsupported;

interface

type
  tmyfunc = function(a: longint): longint;
  tmyfuncobj = function(a: longint): longint of object;

type
  tc = class
    function methfunc(a: longint): longint;
    class procedure methproc; static;
  end;
  tcclass = class of tc;

procedure test;

implementation


function tc.methfunc(a: longint): longint;
begin
end;

class procedure tc.methproc;
begin
end;

function func(a: longint): longint;
begin
  result:=a;
end;

procedure test;
var
  m: tmyfunc;
  l: longint;
  c: tc;
  m2,m2a: tmyfuncobj;
begin
  m:=func;
  l:=m(6);
  m2:=c.methfunc;
  l:=m2(60);
  if assigned(m) then ;
  if assigned(m2) then ;
  if @m=nil then ;
  if @m2=nil then ;
  m2a:=m2;
end;

procedure testset;
var
  a,b: set of byte;
begin
  a:=[1..127];
  b:=[4..129];
  include(a,6);
  a:=a*b+b-b><a;
  if 3 in a then ;
end;

procedure testnest;
var
  a: longint;

  procedure nest;
    begin
      a:=5;
    end;

begin
  nest;
end;


procedure testclassref;
var
  cr: tcclass;
begin
  cr:=tc;
end;  


procedure callarrconst(a: array of const);
begin
  if a[0].vtype = vtInteger then ;
  if a[0].vinteger=4 then ;
end;

procedure testarrconst;
begin
  callarrconst([32,1.0]);
end;

end.
