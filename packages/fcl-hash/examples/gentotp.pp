{$mode objfpc}
{$h+}
uses sysutils, onetimepass;

Var
  aCount : Longint;

begin
  If ParamCount=0 then
    Writeln('New key: ',TOTPSharedSecret)
  else If ParamCount=1 then 
    begin
    if (ParamStr(1)='-h') or (ParamStr(1)='--help') then
      begin
      Writeln('Usage : ',ExtractFileName(Paramstr(0)),' [key [code]]');
      Writeln('If no options are specified, generate key');
      Writeln('If only key is specified, print current code');
      Writeln('If both key and code are specified then check code'); 
      end
    else
      Writeln('Current token : ',TOTPGenerateToken(ParamStr(1)));
    end
  else  
    begin
    if TOTPValidate(Paramstr(1),StrToIntDef(ParamStr(2),-1),1,aCount) then
      Writeln('Code OK')
    else
      begin
      Writeln('Code wrong');
      ExitCode:=1;
      end;
    end;
end.