{$MODE DELPHI}

uses sysutils;

procedure tt (params : array of const);
begin
// this call generate Access violation
  writeln (Format ('Params test %d', params));
end;

begin
  writeln (Format ('First test %d', [1]));
  tt ([1]);
end.
