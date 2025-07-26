program tmultilinestring19;

{$modeswitch MultiLineStrings}
{$MultiLineStringTrimLeft 2}

{ This is extremely unlikely, but it needs to work properly... }

procedure Bloop; external name `
  Actually
  Called
  Bloop
`;

begin
end.
