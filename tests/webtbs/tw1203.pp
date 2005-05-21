{ Source provided for Free Pascal Bug Report 1203 }
{ Submitted by "Marco van de Voort" on  2000-10-29 }
{ e-mail: marco@freepascal.org }
{$mode Delphi}
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
