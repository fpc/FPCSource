program tmultilinestring12;

{$modeswitch MultiLineStrings}
{$MultiLineStringTrimLeft 4}

procedure TakesAString(const S: String);
begin
  Write(S);
end;

begin
  TakesAString(`
    This
    works
    just
    fine!
  `);
end.
