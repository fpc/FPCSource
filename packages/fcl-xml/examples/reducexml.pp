program reducexml;

{$mode objfpc}
{$h+}

uses cwstring,SysUtils,classes,DOM,xmlutils,xmlread,xmlwrite;

Var
  D : TXMLDocument;
  S : TFileStream;
  W : TDOMWriter;
  FN : String;
 

begin
  if paramCount=0 then
    begin
    Writeln('Usage : reducexml infile [outfile]');
    halt(1);
    end;
  ReadXMLFile(D,ParamStr(1));
  FN:=ParamStr(2);
  if FN='' then
    FN:=ChangeFileExt(ParamStr(1),'-new.xml');
  W:=nil;  
  S:=TFileStream.Create(FN,fmCreate);
  try
    W:=TDOMWriter.Create(S,D);
    W.IndentSize:=1;
//    W.Canonical:=True;
    W.UseTab:=True;
    W.WriteNode(D);
  Finally
    W.Free;
    S.Free;
  end;
end.
