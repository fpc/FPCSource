{$mode objfpc}
unit uw21808b;

interface

uses
  uw21808a;

type
  TH = class helper for TC
    procedure Q;
  end;

implementation

procedure TH.Q;
begin
  Writeln(2);
end;

end.

