program testinet;

{
   Program to test the inet unit.
  (C) 1997,1998 by Michael Van Canneyt
}

uses inet;

var p : PHostEnt;
    ph : Phostaddr;
    pn : PNetEnt;
    ps : PServEnt;
    pp : ppchar;
    host : THost;
    Net : TNet;
    service : TService;
    S : String;
    TheAddr : THostAddr;

const
    { Lily is my machine. This may not work of you're on a
      standalone machine. In that case, replace the address by
      an address known to your machine, or, as a last resort, 127.0.0.1 }

    lily : THostAddr = (134,58,81,164);
    {lily : THostAddr = (127,0,0,1);}

begin
  p:=gethostbyname ('LocalHost');
  if p=nil then
    begin
    Writeln ('GetHostByname : No entry');
    end
  else
    begin
    Writeln ('Data for localhost : ');
    with p^ do
      begin
      writeln ('Name   : ',name);
      writeln ('Length : ',Addrlen);
      pp:=aliases;
      while pp^<>nil do
        begin
        writeln ('Alias : ',pp^);
        inc(longint(pp),4);
        end;
      ph:=PHostAddr(addrlist^);
      writeln ('Addres : ',ph^[1],'.',ph^[2],'.',ph^[3],'.',ph^[4]);
      end;
    end;
  p:=gethostbyaddr (@lily,4,2);
  if p=nil then
    begin
    Writeln ('Gethostbyaddr : No entry');
    end
  else
    begin
    Writeln ('Data for ',lily[1],'.',lily[2],'.',lily[3],'.',lily[4] );
    with p^ do
      begin
      writeln ('Name   : ',name);
      writeln ('Length : ',Addrlen);
      pp:=aliases;
      while pp^<>nil do
        begin
        writeln ('Alias : ',pp^);
        inc(longint(pp),4);
        end;
      ph:=PHostAddr(addrlist^);
      writeln ('Addres : ',ph^[1],'.',ph^[2],'.',ph^[3],'.',ph^[4]);
      end;
    end;
  pn:=GetNetByName ('loopback');
  if pn=nil then
    begin
    writeln ('GetNetByName : No entry');
    end
  else
    begin
    Writeln ('Data for loopback : ');
    with pn^ do
      begin
      writeln ('Name   : ',name);
      writeln ('Type   : ',AddrType);
      pp:=aliases;
      while pp^<>nil do
        begin
        writeln ('Alias : ',pp^);
        inc(longint(pp),4);
        end;
      ph:=PHostAddr(@net);
      writeln ('Addres : ',ph^[4],'.',ph^[3],'.',ph^[2],'.',ph^[1]);
      end;
    end;
  pn:=GetNetByAddr ((127 shl 24),2);
  if pn=nil then
    begin
    writeln ('GetNetByAddr : No entry');
    end
  else
    begin
    Writeln ('Data for 127.0.0.0 : ');
    with pn^ do
      begin
      writeln ('Name   : ',name);
      writeln ('Type   : ',AddrType);
      pp:=aliases;
      while pp^<>nil do
        begin
        writeln ('Alias : ',pp^);
        inc(longint(pp),4);
        end;
      ph:=PHostAddr(@net);
      writeln ('Addres : ',ph^[4],'.',ph^[3],'.',ph^[2],'.',ph^[1]);
      end;
    end;
  ps:=GetServByName ('telnet','tcp');
  if ps=nil then
    begin
    writeln ('GetServByName : No entry ');
    end
  else
    with ps^ do
      begin
      writeln ('Name : ',name);
      writeln ('Protocol : ',proto);
      writeln ('Port ',port shr 8);
      pp:=aliases;
      while pp^<>nil do
        begin
        writeln ('Alias : ',pp^);
        inc(longint(pp),4);
        end;
      end;
  ps:=GetServByPort (23 shl 8 ,'tcp');
  if ps=nil then
    begin
    writeln ('GetServByPort : No entry ');
    end
  else
    with ps^ do
      begin
      writeln ('Name : ',name);
      writeln ('Protocol : ',proto);
      writeln ('Port ',port shr 8);
      pp:=aliases;
      while pp^<>nil do
        begin
        writeln ('Alias : ',pp^);
        inc(longint(pp),4);
        end;
      end;
  Writeln ('Creating Host Object with namelookup(tflily)');
  Host.NameLookup ('tflily');
  If Host.LastError=0 then
    begin
    Writeln   ('Name       : ',host.name);
    S:=Host.GetAlias (stfirst);
    While S<>'' do
      begin
      Writeln ('Host alias : ',S);
      S:=Host.GetAlias(stnext);
      end;
    S:=Host.GetAddress (stfirst);
    While S<>'' do
      begin
      Writeln ('Host address : ',S);
      S:=Host.GetAddress(stnext);
      end;
    end;
  Writeln ('Creating Host Object with Addresslookup(''tflily'')');
  Host.AddressLookup (lily);
  If Host.LastError=0 then
    begin
    Writeln   ('Name       : ',host.name);
    Writeln   ('IP Address : ',host.IPstring);
    S:=Host.GetAlias (stfirst);
    While S<>'' do
      begin
      Writeln ('Host alias : ',S);
      S:=Host.GetAlias(stnext);
      end;
    S:=Host.GetAddress (stfirst);
    While S<>'' do
      begin
      Writeln ('Host address : ',S);
      S:=Host.GetAddress(stnext);
      end;
    end;
  Writeln ('Creating net Object with namelookup(''loopback'')');
  net.NameLookup ('loopback');
  If net.LastError=0 then
    begin
    Writeln   ('Name       : ',net.name);
    Writeln   ('IP address : ',net.IPstring);
    S:=net.GetAlias (stfirst);
    While S<>'' do
      begin
      Writeln ('net alias : ',S);
      S:=net.GetAlias(stnext);
      end;
    end;
  Writeln ('Creating net Object with Addrlookup((127 shl 24))');
  net.AddressLookup ((127 shl 24));
  If net.LastError=0 then
    begin
    Writeln   ('Name       : ',net.name);
    Writeln   ('IP address : ',net.IPstring);
    S:=net.GetAlias (stfirst);
    While S<>'' do
      begin
      Writeln ('net alias : ',S);
      S:=net.GetAlias(stnext);
      end;
    end;
  S:='134.58.81.164';
  TheAddr:=StrToHostAddr (S);
  Writeln (S,' = ',TheAddr[1],'.',theaddr[2],'.',theaddr[3],'.',theaddr[4]);
  Writeln ('Creating Service Object with Namelookup(''telnet'',''tcp'')');
  Service.Namelookup('telnet','tcp');
  If Service.LastError=0 then
    begin
    Writeln   ('Name       : ',Service.name);
    Writeln   ('Protocol   : ',service.protocol);
    Writeln   ('Port       : ',ShortNetToHost(service.port));
    S:=service.GetAlias (stfirst);
    While S<>'' do
      begin
      Writeln ('service alias : ',S);
      S:=service.GetAlias(stnext);
      end;
    end;
  Writeln ('Creating Service Object with Portlookup(23 shl 8 ,''tcp'')');
  Service.Portlookup(23 shl 8,'tcp');
  If Service.LastError=0 then
    begin
    Writeln   ('Name       : ',Service.name);
    Writeln   ('Protocol   : ',service.protocol);
    Writeln   ('Port       : ',ShortNetToHost(service.port));
    S:=service.GetAlias (stfirst);
    While S<>'' do
      begin
      Writeln ('service alias : ',S);
      S:=service.GetAlias(stnext);
      end;
    end;

end.  $Log$
end.  Revision 1.2  2002-09-07 15:42:52  peter
end.    * old logs removed and tabs fixed
end.
end.  Revision 1.1  2002/01/29 17:54:53  peter
end.    * splitted to base and extra
end.
}
