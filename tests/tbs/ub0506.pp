{$ifdef fpc}{$mode objfpc}{$endif}
unit ub0506;
interface
type
  c1=class
    procedure SetValue(i:integer);
    function GetValue:integer;
    property Value:integer read GetValue write SetValue;
  end;

  c2=class(c1)
    procedure SetValue(i:integer);
    function GetValue:integer;
    property Value read GetValue;
  end;

implementation

procedure c1.SetValue(i:integer);
begin
  writeln('c1.SetValue');
end;

function c1.GetValue:integer;
begin
  writeln('c1.getValue');
  result:=1;
end;

procedure c2.SetValue(i:integer);
begin
  writeln('c2.SetValue');
end;

function c2.GetValue:integer;
begin
  writeln('c2.getValue');
  result:=2;
end;

end.