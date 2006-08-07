{ %fail }
{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

uses uw6922;

type

  { TC }

  TC=class(TA)
  public
    procedure Test;
  end;

{ TC }

procedure TC.Test;
var
  B: TB;
begin
  T := 'Test1'; // allowed, because it is a descendant
  B := TB.Create;
  B.T := 'Test2'; // should not be allowed
  writeln(B.T);
  B.Free;
end;

var
  c: TC;
begin
  c := TC.Create;
  c.T := 'Test3'; // allowed, because it is in the same 'unit'
  c.Test;
  c.Free;
end.
