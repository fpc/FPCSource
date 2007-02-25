{$mode objfpc}
{$h+ }

{ test service resolve }

program testsres;

uses resolve;

Var
  I : integer;
  S,P : String;


begin
  If (ParamCount>0) then
    S:=Paramstr(1)
  else
    S:='telnet';
  If (ParamCount>1) then
    P:=Paramstr(2)
  else
    P:='';
  With TServiceResolver.Create(Nil) do
    try
      If Not NameLookup(S,P) then
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
