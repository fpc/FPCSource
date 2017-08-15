program ini2json;

{$mode objfpc}
{$h+1}

uses sysutils,jsonini;

var
  fin,fout : string;

begin
  if (ParamCount<1) then
    begin
    Writeln('Usage : ',ExtractFileName(ParamStr(0)),' infile [outfile]');
    Halt(1);
    end;
  Fin:=ParamStr(1);  
  FOut:=ParamStr(2);  
  If (Fout='') then
    Fout:=ChangeFileExt(Fin,'.json');
  TJSONIniFile.ConvertIni(Fin,Fout,False);  
end.