program showcsv;

{$mode objfpc}
{$H+}

uses db,csvdataset, classes, sysutils;

Procedure usage;

begin
  Writeln('Usage: ',ExtractFIleName(Paramstr(0)),' [-h] [-c col] filename');
  halt(1);
end;

Var
  FN,Column : String;
  F : TField;

begin
  if (paramcount=0) or (paramstr(1)='-h') then 
    usage;
  if (Paramstr(1)='-c') then
    begin
    Column:=Paramstr(2);
    FN:=Paramstr(3);
    end
  else if ParamCount<>1 then
    Usage
  else   
    FN:=Paramstr(1);
  With TCSVDataset.Create(Nil) do
    try
      CSVOptions.FirstLineAsFieldNames:=True;
      LoadFromFile(FN);
      Active:=True;
      While not EOF do
        begin
        Write(RecNo,' : ');
        For F in FIelds do
          if (Column='') or (Column=F.FIeldName) then
            Write(' ',F.AsString);
        Writeln;
        Next;  
        end;      
    finally
      Free;
    end;    
end.