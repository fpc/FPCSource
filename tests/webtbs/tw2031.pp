program settest;
const
  size = 31;
var
  testset : set of 0..size;
  i : integer;
begin
  testset := [];
  testset := testset + [0,1,2,3,4];
  if testset <> [0,1,2,3,4] then
    begin
      writeln('add wrong');
      halt(1);
    end;
  testset := testset - [2];
  if testset <> [0,1,3,4] then
    begin
      writeln('sub wrong');
      halt(1);
    end;
end.
