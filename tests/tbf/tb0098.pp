{ %FAIL }

{ derived from Web Bug Report 1203 }
{$mode objfpc}
type
     someprocedureofobjectype=procedure (sender:tobject) OF OBJECT;
     a=object
     protected
        fondisplay : someprocedureofobjectype;
     public
        a:longint;
     end;

     b=object(A)
     protected
        fondisplay : someprocedureofobjectype;
     public
        a:longint;
     end;

begin
end.
