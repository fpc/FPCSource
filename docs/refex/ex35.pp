Program Example35;

{ Program to demonstrate the IOResult function. }

Var F : text;

begin
  Assign (f,paramstr(1));
  {$i-}
  Reset (f);
  {$i+}
  If IOresult<>0 then
    writeln ('File ',paramstr(1),' doesn''t exist')
  else
    writeln ('File ',paramstr(1),' exists');
end.
