{ %FAIL }
{ Old file: tbf0008.pp }
{  tests the crash when decrementing constants         OK 0.9.2 }

const
   compilerconst=1;

begin
   dec(compilerconst);
end.
