{ %norun }

{ if the compiler is still buggy and consumed the "calling stdcall" while
  parsing the forward definition of test, it will complain about a calling
  convention mismatch when parsing the actual definition
}

{$calling register}
function test(l1,l2: longint): longint; forward;
{$calling stdcall}

function test(l1,l2: longint): longint; register;
begin
end;

begin
end.
