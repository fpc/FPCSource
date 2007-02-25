{$mode objfpc}
{$h+ }

{ test reverse service resolve }

program testrsre;

uses sysutils,resolve;

Var
  I : integer;
  S : longint;
  P : String;


begin
  If (ParamCount>0) then
    S:=StrToIntDef(Paramstr(1),0)
  else
    S:=23;
  If (ParamCount>1) then
    P:=Paramstr(2)
  else
    P:='';
  With TServiceResolver.Create(Nil) do
    try
      If Not PortLookup(S,P) then
        Writeln('Lookup failed : ',LastError)
      else
        begin
        Writeln('Name          : ',ResolvedName);
        Writeln('Protocol      : ',Protocol);
        Writeln('Port          : ',port);
        Writeln('Aliases       : ',AliasCount);
        For I:=0 to AliasCount-1 do
          Writeln('Alias ',i:2,'  : ',Aliases[I]);
        end;
    finally
      Free;
    end;
end.
