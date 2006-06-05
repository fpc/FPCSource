{$mode objfpc}
{$h+}
program testhosts;

uses sockets,netdb;

Const 
{$ifdef unix}
  hosts = '/etc/hosts';
{$else}
{$ifdef win32}
  hosts = 'c:\windows\system32\drivers\etc\hosts';
{$else}
  hosts = 'hosts';  { Fallback !! }
{$endif}
{$endif}  
  
var
  L,P : PHostListEntry;
  I : Integer;

begin
  L:=ProcessHosts(Hosts);
  Try
    P:=L;
    I:=0;
    While (P<>Nil) do 
      begin
      With P^ do
        begin
        Inc(I);
        Write(i:3,' Address : ',HostAddrToStr(NetToHost(P^.entry.addr)):15);
        Write(' hostname : ',P^.entry.Name);
        If (P^.entry.Aliases<>'') then
          Writeln(' Aliases : ',P^.entry.Aliases)
        else
          Writeln;   
        P:=P^.next;  
        end;
      end
  finally
    FreeHostslist(L);    
  end;   
end.  