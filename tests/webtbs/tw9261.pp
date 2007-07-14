program tw9261;

{$mode objfpc}

type methodprocvar = function(): Boolean of object;

procedure test_procedure(a1, a2, a3, a4, a5, a6: integer; mv: methodprocvar);
begin
  with Tmethod(mv) do
    if (code<>pointer($11111111)) or (data<>pointer($22222222)) then
       begin
         writeln('test failed');
         halt(1);
       end;
end;

var a:methodprocvar;

begin
  with Tmethod(a) do
    begin
      code:=pointer($11111111);
      data:=pointer($22222222);
    end;
  test_procedure(1, 2, 3, 4, 5, 6, a);
end.
