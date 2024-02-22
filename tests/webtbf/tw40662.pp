{ %fail }
program test;

procedure proc(var values: array of string);
begin
  values := ['hi']; // project1.lpr(5,10) Error: Assignments to formal parameters and open arrays are not possible
                    // project1.lpr(5,13) Error: Internal error 99080501
end;

begin
end.
