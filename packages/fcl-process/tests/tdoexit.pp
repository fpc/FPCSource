{%norun}
{%neededafter}

program doexit;

uses sysutils;

var
  WT,EC : Integer;

begin
  EC:=StrToIntDef(ParamStr(1),0);
  WT:=StrToIntDef(ParamStr(2),0);
  if WT>0 then
    Sleep(WT);
  Halt(EC);
end.

