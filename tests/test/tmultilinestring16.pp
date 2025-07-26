program tmultilinestring16;

{$modeswitch MultiLineStrings}
{$MultiLineStringTrimLeft 6}

procedure TakesAnArray(constref A: array of String);
var S: String;
begin
  for S in A do Write(S);
end;

begin
  TakesAnArray([
    ` Multi
      line
      one!`,
    `
      Multi
      line
      two!`,
    `
      Multi
      line
      three!
    `,
    'Single line one!' + sLineBreak +
    'Single line two!' + sLineBreak +
    'Single line three!'
  ]);
end.
