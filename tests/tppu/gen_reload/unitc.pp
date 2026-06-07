unit unitc;

{$mode delphi}

interface

uses unitb;

procedure doit;
procedure doit2;

implementation

procedure doit;
var
  t: TThing;
begin
  t := TThing.Create;
  t.DoStuff;
  t.Free;
end;

procedure doit2;
begin
end;

end.
