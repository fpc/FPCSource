{ %NORUN }

program tw31120;

{$mode objfpc}

type
  TTest = class
    generic class procedure PrintDefault<T>();
  end;

generic Function GetDefault<T>(): T;
  Begin
    result := default(T);
  End;

generic Procedure PrintDefault<T>();
  procedure print();
    begin
      writeln(specialize GetDefault<T>())
    end;

  Begin
    print()
  End;

generic class procedure TTest.PrintDefault<T>();
  procedure print();
    begin
      writeln(specialize GetDefault<T>())
    end;

begin
  print()
end;

Begin
  specialize PrintDefault<LongInt>();
  specialize PrintDefault<String>();
  TTest.specialize PrintDefault<Boolean>();
  TTest.specialize PrintDefault<Char>();
End.
