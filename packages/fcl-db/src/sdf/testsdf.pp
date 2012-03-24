{$mode objfpc}
{$h+}
program testsdf;

uses DB,sdfdata,sysutils;

Procedure Dotest;

Var
  I,Count : Integer;

begin
  With TSdfDataSet.Create(Nil) do
    try
      Delimiter := #9;
      FileName := 'fpc.ssy';
      FirstLineAsSchema := False;
      Schema.Add('First Name');
      Schema.Add('Last Name');
      Schema.Add('Email');
      Open;
      Count:=0;
      Try
        While Not EOF do
          begin
          Inc(Count);
          Writeln('Record : ',Count);
          For I:=0 to FieldCount-1 do
            Writeln(Fields[i].FieldName,' : ',Fields[i].AsString);
          Writeln('-------------------------------')  ;
          Next;
          end;
      Finally
        Close;
      end;
    finally
      free;
    end;
end;

Procedure DoTest2;
begin
  // if file does not exists, then create it with schema at first line
  With TSdfDataSet.Create(Nil) do
    try
      Delimiter := #9;
      FileName := 'fpc2.ssy';
      FileMustExist := False;
      FirstLineAsSchema := True;
      Schema.Add('First Name');
      Schema.Add('Last Name');
      Schema.Add('Email');
      Open;
      AppendRecord(['FName', 'LName', 'name@freepascal.org']);
      Close;
    finally
      Free;
    end;
end;

begin
  DoTest;
  DoTest2;
end.
