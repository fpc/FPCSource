{$mode objfpc}
unit uw21808a;

interface

type
  TC = class
    procedure P;
  end;

implementation

procedure TC.P;
begin
  Writeln(1);
end;

end.

