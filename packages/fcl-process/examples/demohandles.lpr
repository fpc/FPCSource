program demohandles;

uses classes, process;

var
  P : TProcess;
  F : TFileStream;
  
begin
  P:=TProcess.Create(Nil);
  P.Executable:='/bin/ls';
  P.OutputDescriptor.FileName:='tmp.txt';
  P.Parameters.Add('-l');
  P.Execute;
end.

