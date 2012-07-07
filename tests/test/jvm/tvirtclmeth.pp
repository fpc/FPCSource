program tvirtclmeth;

{$mode delphi}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

type
  tvirtclmethbase = class
    constructor create(l: longint); virtual; overload;
    class function test(l: longint): ansistring; virtual;
  end;

  tvirtclmethchild = class(tvirtclmethbase)
    constructor create(l: longint); override; overload;
    class function test(l: longint): ansistring; override;
    procedure docreate;
  end;

  tvirtclmethchild2 = class(tvirtclmethchild)
  end;

  tcc = class of tvirtclmethbase;


  constructor tvirtclmethbase.create(l: longint);
    begin
      if l<>1 then
        raise jlexception.create('base class constructor but child expected');
    end;

  class function tvirtclmethbase.test(l: longint): ansistring;
    begin
      if l<>1 then
        raise jlexception.create('base class but child expected');
      result:='base';
    end;

  constructor tvirtclmethchild.create(l: longint);
    begin
      if l<>2 then
        raise jlexception.create('child class constructor but base expected');
    end;

  class function tvirtclmethchild.test(l: longint): ansistring;
    begin
      if l<>2 then
        raise jlexception.create('child class but base expected');
      result:='child';
    end;


  procedure tvirtclmethchild.docreate;
    var
      c: tvirtclmethchild;
    begin
      c:=self.create(2);
    end;

var
  cc: tcc;
  c: tvirtclmethbase;
begin
  c:=tvirtclmethbase.create;
  if c.test(1)<>'base' then
    raise JLException.create('base 1 res');
  c:=tvirtclmethchild.create;
  if c.test(2)<>'child' then
    raise JLException.create('child 1 res');
  tvirtclmethchild(c).docreate;
  cc:=tvirtclmethbase;
  if cc.test(1)<>'base' then
    raise JLException.create('base 2 res');
  cc:=tvirtclmethchild;
  if cc.test(2)<>'child' then
    raise JLException.create('child 2 res');
  cc:=tvirtclmethchild2;
  if cc.test(2)<>'child' then
    raise JLException.create('child2 1 res');

  c:=tvirtclmethbase.create(1);
  if not(c is tvirtclmethbase) then
    raise JLException.create('base 4 res');
  c:=tvirtclmethchild.create(2);
  if not(c is tvirtclmethchild) then
    raise JLException.create('child 4 res');
  c:=tvirtclmethchild2.create(2);
  if not(c is tvirtclmethchild2) then
    raise JLException.create('child2 2 res');
  cc:=tvirtclmethbase;
  c:=cc.create(1);
  if not(c is tvirtclmethbase) then
    raise JLException.create('base 4 res');
  cc:=tvirtclmethchild;
  c:=cc.create(2);
  if not(c is tvirtclmethchild) then
    raise JLException.create('child 4 res');
  cc:=tvirtclmethchild2;
  c:=cc.create(2);
  if not(c is tvirtclmethchild2) then
    raise JLException.create('child2 3 res');
end.
