
function test : string;

  begin
    test:='This should not be printed';
    exit('this should be printed');
  end;

begin
  writeln(test);
  if test<>'this should be printed' then
    Halt(1);
end.
