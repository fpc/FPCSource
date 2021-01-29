{ %FAIL }
program tgenconst30;

{$mode objfpc}

type
  TRange = 3..4;

  generic TTest<const U: TRange> = record end;

var
  t: specialize TTest<2>;
begin
end.
