{$mode objfpc}
{$h+ }

{ test network resolve }

program testnres;

uses resolve;

Var
  I : integer;
  S : String;


begin
  If (ParamCount>0) then
    S:=Paramstr(1)
  else
    S:='loopback';
  With TNetResolver.Create(Nil) do
    try
      If Not NameLookup(S) then
        Writeln('Lookup failed : ',LastError)
      else
        begin
        Writeln('Name          : ',ResolvedName);
        Writeln('Addres        : ',AddressAsString);
{
        Writeln('Address count : ',);
        For I:=0 to AddressCount-1 do
          Writeln('Adress ',I:2,'     : ',HostAddrToStr(Addresses[I]));
}
        Writeln('Alias count   : ',AliasCount);
        For I:=0 to AliasCount-1 do
          Writeln('Alias ',i:2,'  : ',Aliases[I]);
        end;
    finally
      Free;
    end;
end.
