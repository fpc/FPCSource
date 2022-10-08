{ %NORUN }
{ %OPT=-Sew }

program tb0697;

{ $P is now a local switch; compatible to Delphi }

{$V+}
{$P-}

procedure Test(var aArg: ShortString);
begin
end;

{$P+}

procedure Test2(var aArg: ShortString);
begin
end;

var
  s: String[5];
begin
  s := 'Test';
  Test2(s);
end.
