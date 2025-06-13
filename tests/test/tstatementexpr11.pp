{$ModeSwitch StatementExpressions}

type
  TS3 = String[3];
  TS12 = String[12];

var
  s: String;
begin
  s := case 5 of
    0..4: TS3('Foo');
    5: TS12('FooBar');
    otherwise TS3('Bar');
  WriteLn(s);
  if (s<>'FooBar') then Halt(1);
end.
