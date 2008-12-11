{ %wpoparas=devirtcalls,optvmts }
{ %wpopasses=1 }

{$mode objfpc}

{ check to make sure that classes created via classrefdefs are properly
  registered
}

type
  ta = class
    constructor mycreate;
    procedure test; virtual;
    class procedure test2; virtual;
  end;

  tb = class(ta)
    procedure test; override;
    class procedure test2; override;
  end;

constructor ta.mycreate;
begin
end;

procedure ta.test;
begin
  writeln('ta.test');
  halt(1);
end;


class procedure ta.test2;
begin
  writeln('ta.test2');
end;


var
 cc: class of ta;


procedure tb.test;
begin
  writeln('tb.test');
end;

class procedure tb.test2;
begin
  cc:=self;
  writeln('tb.test2');
end;

var
  a: ta;
  ca: class of ta;
begin
  tb.test2;
  a:=cc.create;
  a.test;
  a.free
end.
