{ %FAIL }

{ derived from source for Bug Report 1203 }
{ in FPC mode this shouldn't compile      }
{$mode objfpc}
type
     someprocedureofobjectype=procedure (sender:tobject) OF OBJECT;
     a=class
     protected
        fondisplay : someprocedureofobjectype;
     public
        a:longint;
     end;

     b=class(A)
     protected
        fondisplay : someprocedureofobjectype;
     public
        a:longint;
     end;

begin
end.
