{$mode objfpc}
{$h+}
program testfix;

uses DB,sdfdata,sysutils;

Procedure Dotest;

Var
  I,Count : Integer;

begin
  With TFixedFormatDataSet.Create(Nil) do
    try
      FileName := 'fpc.ssx';
      Schema.Add('First Name=20');
      Schema.Add('Last Name=20');
      Schema.Add('Email=30');
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

begin
  DoTest;
end.
