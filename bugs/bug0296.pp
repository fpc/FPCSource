
function test : string;

  begin
    test:='This should not be printed';
    exit('this should be printed');
  end;

begin
  writeln(test);
end.
