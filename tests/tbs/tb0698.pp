{ %NORUN }

program tb0698;

{ Open Strings is by default enabled in mode Delphi }

{$mode delphi}

procedure Test(var aArg: ShortString);
begin
end;

var
  s: String[5];
begin
  s := 'Test';
  Test(s);
end.
