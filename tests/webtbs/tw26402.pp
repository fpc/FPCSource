{ %opt=-vh -Seh }
{ %norun }

{$MODESWITCH ADVANCEDRECORDS+}
program test;

type
   TestInternals = record
      private
         type
            TTest = record
               Bar: AnsiString;
            end;
   end;

var
   Foo: TestInternals.TTest;
begin
   Foo.Bar := '';
end.
