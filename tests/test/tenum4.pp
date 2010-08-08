{ %fail}
{ %norun}

// test checks that enumeration memebers are not present in the global/local symtables 
// if enumeration is defined with scopedenums directive

program tenum4;
type
{$SCOPEDENUMS ON}
  TEnum1 = (first, second, third);
{$SCOPEDENUMS OFF}
  TEnum2 = (zero, first, second, third);
var
  En1: TEnum1;
begin
  // this is not possible since first belongs to TEnum2
  En1 := first;
end.