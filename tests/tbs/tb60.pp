{ Old file: tbs0066.pp }
{  shows that Round doesn't work correctly. (NOT A bugs) OK 0.99.1 }

Program Example54;

{ Program to demonstrate the Round function. }

begin
  Writeln (Round(123.456));  { Prints 124  }
  Writeln (Round(-123.456)); { Prints -124 }
  Writeln (Round(12.3456));  { Prints 12   }
  Writeln (Round(-12.3456)); { Prints -12  }
end.
