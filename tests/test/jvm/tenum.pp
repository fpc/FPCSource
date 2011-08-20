program tenum;

{$mode delphi}

uses
  uenum;

const
  cenum = mea;

type
  tenumclass = class
    e: myenum;
    constructor create;
  end;

constructor tenumclass.create;
  begin
    if e<>mea then
      raise JLException.create('error create');
  end;

function func: myenum;
begin
  result:=cenum;
end;

var
  a: myenum;
  b1,b2: myenumjumps;
  l: longint;
  arr: array[myenum] of byte;
  c: tenumclass;
  earr: array[1..4] of myenum;
  dearr: array of myenum;
begin
  c:=tenumclass.create;
  if earr[1]<>mea then
    raise JLException.create('error 0');
  setlength(dearr,1);
  if dearr[0]<>mea then
    raise JLException.create('error 0a');
  a:=cenum;
  inc(a);
  if ord(a)<>1 then
    raise JLException.create('error 1');
  a:=succ(a);
  if a<>mec then
    raise JLException.create('error 2');

  arr[a]:=123;
  if arr[mec]<>123 then
    raise JLException.create('error 2a');
  l:=0;
  for a:=func to mec do
    inc(l,ord(a));
  if l<>3 then
    raise JLException.create('error 2b');
  if JLObject(mea).toString<>'mea' then
    raise JLException.create('expected mea, got '+unicodestring(JLObject(mea).toString));

  a:=mec;
  case a of
    mea..meb:
     raise JLException.create('error 2c');
    mec:
      ;
    else
     raise JLException.create('error 2d');
  end;
   

  b1:=meja;
  b2:=mejb;
  if b1<=b2 then
    raise JLException.create('error 3');
  b2:=mejc;
  if b1>=b2 then
    raise JLException.create('error 4');
  l:=-5;
  b2:=myenumjumps(l);
  if b2<>mejb then
    raise JLException.create('error 5');
end.
