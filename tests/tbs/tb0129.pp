{ Old file: tbs0149b.pp }
{  }

{there is no crash when tset or c from unit a are used in OuterProcedure,
 it's only a problem when using them in a nested procedure/function}

unit tb0129;

interface

uses ub0129;

implementation

Procedure OuterProcedure;

  function t(a: byte): byte;
  begin
     if a = c then t := a else t := 0;
     if a in tset   {probably same bug}
       then t := a
       else t := 0
  end;

Begin
End;

end.
