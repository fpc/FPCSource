program dumpdb;

{$i+}

uses DB,Dbf,SysUtils;

Procedure DumpTable (Const TN,FN : String);

Var
  I,Count : longint;
  F : Text;
  Buf : Array[1..1024*4] of byte;

begin
  Assign(F,FN);
  Rewrite(F);
//  SetTextBuf(F,Buf);
  Count := 0;
  With TDBF.Create(Nil) do
    begin
    TableName:=TN;
    Open;
    While not EOF do
      begin
      Inc(Count);
      For I:=0 to FieldCount-1 do
        With Fields[i] do
          Writeln(F,FieldName:20,' : ',AsString);
      Writeln(F,StringOfChar('=',72));
      Next;
      end;
    end;
  Writeln(F,'Dumped total of ',Count,' records.');
  Close(F);
end;

Var i : longint;

begin
  If ParamCount<2 then
    begin
    Writeln('Usage: dumpdb tablename filename');
    Halt(1);
    end;
  DumpTable(Paramstr(1),Paramstr(2));
end.
