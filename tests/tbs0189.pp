var m: procedure;

procedure test;
begin
end;

procedure test2;
begin
end;

begin
 if @test <> @test2 then
   writeln('different!')
 else
   writeln('error');
 m:=@test;

 { here also the syntax was wrong !! }
 {  @m <> @test have different types !! }
 if m <> @test then
   writeln('error');
end.
