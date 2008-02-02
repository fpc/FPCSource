{$mode objfpc}
{$h+ }

{ test host resolve }

program testhostresolve;

uses resolve,sockets;

Var
  I : integer;
  S : String;


begin
  If (ParamCount>0) then
    S:=Paramstr(1)
  else
    S:='www.freepascal.org';
  With THostResolver.Create(Nil) do
    try
      If Not NameLookup(S) then
        Writeln('Lookup failed : ',LastError)
      else
        begin
        Writeln('Name          : ',ResolvedName);
        Writeln('Addres        : ',AddressAsString);
        Writeln('Address count : ',AddressCount);
        For I:=0 to AddressCount-1 do
          Writeln('Adress ',I:2,'     : ',NetAddrToStr(Addresses[I]));
        Writeln('Alias count   : ',AliasCount);
        For I:=0 to AliasCount-1 do
          Writeln('Alias ',i:2,'  : ',Aliases[I]);
        end;
    finally
      Free;
    end;
end.
