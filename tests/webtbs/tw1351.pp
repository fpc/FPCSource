{$mode objfpc}

procedure Test(ParArr :array of const);
begin
   writeln(ParArr[0].vtype,' ',vtObject,' ',vtclass);
   if ParArr[0].vtype<>vtObject then
    halt(1);
end;

begin
        Test([TObject.Create]);
end.
