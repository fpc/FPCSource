{$mode objfpc}
{$h+ }

{ Test reverse net resolve }

program testrnre;

uses resolve;

Var
  I : integer;
  S : String;


begin
  If (ParamCount>0) then
    S:=Paramstr(1)
  else
    S:='127.0.0.0';
  With TNetResolver.Create(Nil) do
    try
      If Not AddressLookup(S) then
        Writeln('Lookup failed : ',LastError)
      else
        begin
        Writeln('Name          : ',ResolvedName);
        Writeln('Addres        : ',AddressAsString);
        Writeln('Alias count   : ',AliasCount);
        For I:=0 to AliasCount-1 do
          Writeln('Alias ',i:2,'  : ',Aliases[I]);
        end;
    finally
      Free;
    end;
end.
