{ %opt=-Sew }
{ %fail }

{$ifdef fpc}
{$mode delphi}
{$endif fpc}

type
  tc = class
    constructor create1;
    constructor create2;
    procedure t; virtual; abstract;
  end;

  td = class(tc)
    procedure t; override;
  end;

constructor tc.create1;
begin
  inherited create;
end;

constructor tc.create2;
begin
  self.create1;
end;

procedure td.t;
begin
end;

var
  d: tc;
begin
  d := tc.create2;
  d.free;
end.
