{ %opt=-Sh }
{$MODE OBJFPC}
{$HINTS ON}
{$OPTIMIZATION DFA}
program test;

type
   TTest1 = class
      FArray: array of record end; // or AnsiString
      procedure TestMethod();
   end;

procedure TTest1.TestMethod();
begin
   SetLength(FArray, 0); // Hint: Local variable "$self" does not seem to be initialized
end;

type
   TTest2 = class
      FString: AnsiString; // or dynamic array
      procedure TestMethod();
   end;

procedure TTest2.TestMethod();
begin
   FString := Default(AnsiString); // Hint: Local variable "$self" does not seem to be initialized
end;

type
   TTest3 = class
      FValue: Integer;
      procedure TestMethod(var Value: Integer);
   end;

procedure TTest3.TestMethod(var Value: Integer);
begin
   TestMethod(FValue); // Hint: Local variable "$self" does not seem to be initialized
   Value:=FValue;
end;

begin
end.
