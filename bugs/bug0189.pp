var m: procedure;

procedure test;
begin
end;

procedure test2;
begin
end;

begin
 if @test <> @test2 then
   writeln('different!');

 if @m <> @test then
   writeln('hi!');
end.
