program tmultilinestring22;

{$modeswitch MultiLineStrings}
{$MultiLineStringTrimLeft 5}

{ ↓↓↓ the point here being that it no longer disappears }

const Test = ` ThisSpace>>>>     <<<<disappeared`;

begin
  Write(Test);
end.
