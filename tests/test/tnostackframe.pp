{ %fail }
{ %opt=-Sew }

{ This is a border case:
  the result could be considered to reside only in
  the function result register,
  in which case no stack space would be required... }

  { This is the reason of the -Sew option,
    this test must fail because a warning sould be issued
    about nostackframe without assembler.
    Please do not remove -Sew option. PM 2012-10-17 }

function test : longint; nostackframe;

begin
  test:=5;
end;

begin
  if test<>5 then
    begin
      writeln('Wrong result in  nostackframe non-assembler function');
      halt(1);
    end
  else
    writeln('Pascal function nostackframe works OK');
end.
