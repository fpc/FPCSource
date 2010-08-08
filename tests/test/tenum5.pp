{ %fail}
{ %norun}

// test checks that enumeration searches symbols only in the own symbol list 
// if they are prefixed by the enumeration name

program tenum5;
type
{$SCOPEDENUMS ON}
  TEnum1 = (first, second, third);
{$SCOPEDENUMS OFF}
  TEnum2 = (zero, first, second, third);
var
  En1: TEnum1;
begin
  En1 := TEnum1.zero;
end.
