{ %NORUN }

program tw39876b;

{$mode objfpc}
{$scopedenums on}

type
 {$minenumsize 1}
 TEnum1 = (teOne, teTwo, teThree);

 {$minenumsize 2}
 TEnum2 = (teOne, teTwo, teThree);

 {$minenumsize 4}
 TEnum4 = (teOne, teTwo, teThree);

var
 te1: TEnum1;
 te2: TEnum2;
 te4: TEnum4;
 s: String;

begin
  s := 'teTwo';

  ReadStr(s, te1);
  ReadStr(s, te2);
  ReadStr(s, te4);
end.

