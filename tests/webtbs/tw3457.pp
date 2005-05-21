{ Source provided for Free Pascal Bug Report 3457 }
{ Submitted by "Ales Katona (Almindor)" on  2004-12-19 }
{ e-mail: ales@chello.sk }

{$mode objfpc}
{$inline on}

type TTest = class
      private
       procedure testit; inline;
      public // error is reported here
       constructor Create;
     end;

       procedure TTest.testit; inline;
       begin
       end;

       constructor TTest.Create;
       begin
       end;

begin
end.
