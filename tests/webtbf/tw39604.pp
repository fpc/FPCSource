{ %fail }

program Project1;

var
  Buf: integer absolute Buf;

begin
  writeln(Buf);    //uncomment and get: test.pas(12,14) Error: Internal error 200104143
  Buf := 1;        //uncomment and get: test.pas(14,1) Error: Undefined symbol: U_$P$PROJECT1
end.  //line 14
