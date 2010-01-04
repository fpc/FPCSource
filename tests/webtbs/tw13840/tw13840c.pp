{ %interactive }

{ see tw13480d.pp for test instructions }

{$mode objfpc}

unit tw13840c;

interface

uses
  tw13840b;

type
  tc = class(tb)
    procedure mymy(var a);override;
  end;

implementation

procedure tc.mymy(var a);
begin
  writeln('tc.mymy');
  inherited;
end;

end.
