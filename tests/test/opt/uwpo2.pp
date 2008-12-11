{$mode objfpc}
unit uwpo2;

interface

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

var
 cc: class of ta;

implementation

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



procedure tb.test;
begin
  writeln('tb.test');
end;

class procedure tb.test2;
begin
  cc:=self;
  writeln('tb.test2');
end;

end.
