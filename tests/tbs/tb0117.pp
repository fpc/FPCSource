{ Old file: tbs0137.pp }
{ Cannot assign child object variable to parent objcet type variable OK 0.99.6 }

program OO_Test;

Type TVater = Object
                Constructor Init;
                Procedure Gehen; Virtual;
                Procedure Laufen; Virtual;
              End;

     TSohn = Object(TVater)
               Procedure Gehen; Virtual;
             End;

Var V : TVater;
    S : TSohn;

Constructor TVater.Init;
Begin
End;

Procedure TVater.Gehen;
Begin
  Writeln('langsam gehen');
End;

Procedure TVater.Laufen;
Begin
  Gehen;
  Gehen;
End;

Procedure TSohn.Gehen;
Begin
  Writeln('schnell gehen');
End;

Begin
  V.Init;
  S.Init;
  V.Laufen;
  Writeln;
  S.Laufen;
  Writeln;
  V := S;
  V.Gehen;
End.
