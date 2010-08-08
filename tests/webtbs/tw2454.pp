{ Source provided for Free Pascal Bug Report 2454 }
{ Submitted by "Nikolay Nikolov" on  2003-04-06 }
{ e-mail: nickysn1983@netscape.net }
{$MODE objfpc}
Program Test;

Type
  TFunClass = Class(TObject)
    data : Integer;
    Class Procedure FunProc(q : TFunClass);
  End;

Class Procedure TFunClass.FunProc(q : TFunClass);

Begin
  Writeln(q.data);
  With q Do
  Begin
    Writeln(q.data);

    Writeln(data); { fpc 1.1 says: Error: Only class methods can be accessed in class methods

    this is a bug, because 'data' actually means 'q.data' due to the 'with' statement,
    (this can be seen if you make this a normal method by removing the 'Class' keyword
    and running the program, it will writeln q.data, not self.data)
    so it shouldn't cause an error
    }
  End;
End;

Var
  c1, c2 : TFunClass;

Begin
  c1 := TFunClass.Create;
  c2 := TFunClass.Create;
  c1.data := 5;
  c2.data := 7;
  c1.FunProc(c2);
  c1.Destroy;
  c2.Destroy;
End.
