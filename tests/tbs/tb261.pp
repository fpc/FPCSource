{ Old file: tbs0296.pp }
{ exit(string) does not work (web form bugs 613)        OK 0.99.13 (PM) }


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
