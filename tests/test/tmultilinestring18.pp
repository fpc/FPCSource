program tmultilinestring18;

{$modeswitch MultiLineStrings}
{$MultiLineStringTrimLeft 2}
{$Warnings On}

procedure IsDeprecated; deprecated
`
  Multi
  line
  deprecation
  message!
`;
begin
end;

begin
  IsDeprecated;
end.
