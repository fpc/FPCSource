program createds;

uses ddg_rec,sysutils;

Type IndexFile = File Of Longint;

Var F : TDDGDataFile;
    I : Integer;
    S : String;
    L : IndexFile;
    TableName : String;
    IndexName : String;
    ARec : TDDGData;
    
begin
  If ParamCount<>1 then
    begin
    Writeln('Usage: createds tablename');
    Halt(1);
    end;
  TableName:=ChangeFileExt(paramstr(1),'.ddg');
  IndexName:=ChangeFileExt(TableName,'.ddx');
  Assign(F,TableName);
  Rewrite(F);
  For I:=1 to 100 do
    begin
    S:=Format('This is person %d.',[i]);
    ARec.Name:=S; 
    ARec.ShoeSize:=I;
    ARec.height:=I*0.001;
    Write(F,ARec);
    end;
  Close(F);
  Assign(L,IndexName);
  Rewrite(L);
  For I:=0 to 100-1 do
    Write(L,I);
  Close(L);  
end.
