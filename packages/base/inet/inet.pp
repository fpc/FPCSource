Unit inet;

{ --------------------------------------------------------------------
  Unit for internet domain calls.
  Copyright (C) 1997  Michael Van Canneyt

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 1, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  ChangeLog
  ---------

  Current version is 0.6

  Version          Date             Remarks
  -------          ----             ----
  0.1              07/16/97         Unit started. Michael.
  0.2              07/06/98         Updated for version 0.99.5
  0.4              08/01/98         Objects for name lookup implemented
  0.5              09/10/98         Updated calls for 0.99.8.
  0.6              05/04/99         Added explicit asmmode.

  ------------------------------------------------------------------- }


interface

{$LINKLIB c}

Const
  { Net type }
  AF_INET = 2;

  { Error constants. Returned by LastError method of THost, TNet}

  NETDB_INTERNAL= -1;       { see errno }
  NETDB_SUCCESS = 0;        { no problem }
  HOST_NOT_FOUND= 1;        { Authoritative Answer Host not found }
  TRY_AGAIN     = 2;        { Non-Authoritive Host not found, or SERVERFAIL }
  NO_RECOVERY   = 3;        { Non recoverable errors, FORMERR, REFUSED, NOTIMP }
  NO_DATA       = 4;        { Valid name, no data record of requested type }
  NO_ADDRESS    = NO_DATA;  { no address, look for MX record }




Type
  THostAddr = array[1..4] of byte;
  PHostAddr = ^THostAddr;

Const
  NoAddress : THostAddr = (0,0,0,0);

Type

  { THostEnt Object }
  THostEnt = record
    Name     : pchar;   { Official name }
    Aliases  : ppchar;  { Null-terminated list of aliases}
    Addrtype : longint; { Host address type }
    Addrlen  : longint; { Length of address }
    Addrlist : ppchar;  { null-terminated list of adresses }
  end;
  PHostEnt = ^THostEnt;

  { TNetEnt object }
  TNetEnt = record
    Name     : pchar;   { Official name }
    Aliases  : ppchar;  { Nill-terminated alias list }
    AddrType : longint; { Net address type }
    net      : Longint; { Network number }
  end;
  PNetEnt = ^TNetEnt;

  TServEnt = record
    name    : pchar;    { Service name }
    aliases : ppchar;   { Null-terminated alias list }
    port    : longint;  { Port number }
    proto   : pchar;    { Protocol to use }
  end;
  PServEnt = ^TServEnt;

  { Pascal Wrapper objects }

  TSelectType = (stFirst,stNext,stPrevious);

  THost = Object
    FHostEntry : PHostEnt;
    FAlias,FAddr,FError : Longint;
    Constructor NameLookup (HostName : String);
    Constructor AddressLookup (Const Address : THostAddr);
    Destructor Done;
    Function Name : String;
    Function GetAddress (Select : TSelectType) : String;
    Function GetAlias (Select : TSelectType) : String;
    Function IPAddress : THostAddr;
    Function IPString : String;
    Function LastError : Longint;
  end;

  TNet = Object
    FNetEntry : PNetEnt;
    FAlias,FError : Longint;
    Constructor NameLookup (NetName : String);
    Constructor AddressLookup (Const Address : Longint);
    Destructor Done;
    Function Name : String;
    Function GetAlias (Select : TSelectType) : String;
    Function IPAddress : Longint;
    Function IPString : String;
    Function LastError : Longint;
  end;

  TService = Object
    FServiceEntry : PServEnt;
    FAlias,FError : Longint;
    Constructor NameLookup (ServiceName,Proto : String);
    Constructor PortLookup (APort : Longint; Proto: string);
    Destructor Done;
    Function Name : String;
    Function Protocol : String;
    Function GetAlias (Select : TSelectType) : String;
    Function Port : Longint;
    Function LastError : Longint;
  end;



{ Pascal style calls }

function HostAddrToStr (Entry : THostAddr) : String;
function StrToHostAddr (IP : String) : THostAddr;
function NetAddrToStr (Entry : Longint) : String;
function StrToNetAddr (IP : String) : Longint;
Function HostToNet (Host : Longint) : Longint;
Function NetToHost (Net : Longint) : Longint;
Function ShortHostToNet (Host : Word) : Word;
Function ShortNetToHost (Net : Word) : Word;

{ C style calls, linked in from Libc }

function gethostent : PHostEnt; cdecl; external;
function gethostbyname ( Name : Pchar) : PHostEnt; cdecl; external;
function gethostbyaddr ( Addr : PHostAddr; Len : Longint; HType : Longint) : PHostent ; cdecl; external;
procedure sethostent (stayopen : longint); cdecl; external;
procedure endhostent; cdecl; external;

function getnetent : PNetEnt; cdecl; external;
function getnetbyname ( Name : pchar) : PNetEnt; cdecl; external;
function getnetbyaddr ( Net : Longint; nettype : Longint) : PNetEnt; cdecl; external;
procedure setnetent ( Stayopen : Longint);  cdecl; external;
procedure endnetent; cdecl; external;

function getservent : PServEnt; cdecl; external;
function getservbyname (name : pchar  ; protocol : pchar) : PServEnt; cdecl; external;
function getservbyport (port : longint; protocol : pchar) : PServEnt; cdecl; external;
procedure setservent (StayOpen : longint); cdecl; external;
procedure endservent; cdecl; external;

var
  GetDNSError : longint;external name 'h_errno';


implementation

Uses strings;


function HostAddrToStr (Entry : THostAddr) : String;

Var Dummy : String[4];
    I : Longint;

begin
  HostAddrToStr:='';
  For I:=1 to 4 do
   begin
   Str(Entry[I],Dummy);
   HostAddrToStr:=HostAddrToStr+Dummy;
   If I<4 Then HostAddrToStr:=HostAddrToStr+'.';
   end;
end;

function StrToHostAddr(IP : String) : THostAddr ;

Var Dummy : String[4];
    I : Longint;
    J : Integer;
    Temp : THostAddr;

begin
  StrToHostAddr:=NoAddress;
  For I:=1 to 4 do
   begin
   If I<4 Then
     begin
     J:=Pos('.',IP);
     If J=0 then exit;
     Dummy:=Copy(IP,1,J-1);
     Delete (IP,1,J);
     end
   else
     Dummy:=IP;
   Val (Dummy,Temp[I],J);
   If J<>0 then Exit;
   end;
 StrToHostAddr:=Temp;
end;

function NetAddrToStr (Entry : longint) : String;

Var Dummy : String[4];
    I : Longint;

begin
  NetAddrToStr:='';
  For I:=4 downto 1 do
   begin
   Str(THostAddr(Entry)[I],Dummy);
   NetAddrToStr:=NetAddrToStr+Dummy;
   If I>1 Then NetAddrToStr:=NetAddrToStr+'.';
   end;
end;

function StrToNetAddr(IP : String) : Longint;

begin
  StrToNetAddr:=Longint(StrToHostAddr(IP));
end;


Constructor THost.NameLookup (HostName : String);

begin
  HostName:=HostName+#0;
  FHostEntry:=GetHostByName(pchar(@HostName[1]));
  If FHostEntry=Nil then
    FError:=GetDNSError
  else
    begin
    FAlias:=0;
    FAddr:=0;
    Ferror:=0;
    end;
end;

Constructor THost.AddressLookup (Const Address: THostAddr);

begin
  FHostEntry:=GetHostByAddr(PHostAddr(@Address),SizeOf(Address),AF_INET);
  If FHostEntry=Nil then
    FError:=GetDNSError
  else
    begin
    FAlias:=0;
    FAddr:=0;
    FError:=0;
    end;
end;


Function THost.Name : String;

begin
  Name:='';
  If (FHostEntry=Nil) or (FError<>0) then exit;
  Name:=StrPas(FHostEntry^.Name);
end;

Function THost.GetAlias (Select : TSelectType) : String;

begin
  GetAlias:='';
  If (FHostEntry=Nil) or (FError<>0) then exit;
  Case Select of
    stFirst     : FAlias:=0;
    stnext      : If FHostEntry^.Aliases[FAlias]<>Nil then
                    Falias:=Falias+1;
    stprevious  : If FAlias=0 Then Exit else FAlias:=FAlias-1;
  end;
  If FHostEntry^.Aliases[FAlias]<>Nil then
    GetAlias:=StrPas(FHostEntry^.Aliases[FAlias]);
end;

Function THost.GetAddress (Select : TSelectType) : String;

begin
  GetAddress:='';
  If (FHostEntry=Nil) or (FError<>0) then exit;
  Case Select of
    stFirst     : FAddr:=0;
    stnext      : If FHostEntry^.AddrList[FAddr]<>Nil then
                    FAddr:=FAddr+1;
    stprevious  : If FAddr=0 Then Exit else FAddr:=FAddr-1;
  end;
  If FHostEntry^.AddrList[FAddr]<>Nil then
    GetAddress:=HostAddrToStr(PHostAddr(FHostEntry^.AddrList[FAddr])^);
end;

Function THost.IPstring : String;

begin
  IPString:='';
  If (FHostEntry=Nil) or (FError<>0) then exit;
  If FHostEntry^.AddrList[0]<>Nil then
    IPString:=HostAddrToStr(PHostAddr(FHostEntry^.AddrList[0])^);
end;

Function THost.IPaddress : THostAddr;

begin
  IPAddress:=NoAddress;
  If (FHostEntry=Nil) or (FError<>0) then exit;
  IPAddress:=PHostAddr(FHostEntry^.AddrList[0])^;
end;

Destructor THost.Done;

begin
end;

Function THost.LastError : Longint;

begin
  LastError:=FError;
end;

Constructor TNet.NameLookup (NetName : String);

begin
  NetName:=NetName+#0;
  FNetEntry:=GetNetByName(pchar(@NetName[1]));
  If FNetEntry=Nil then
    FError:=GetDNSError
  else
    begin
    FAlias:=0;
    Ferror:=0;
    end;
end;

Constructor TNet.AddressLookup (Const Address: Longint);

begin
  FNetEntry:=GetNetByAddr(Address,AF_INET);
  If FNetEntry=Nil then
    FError:=GetDNSError
  else
    begin
    FAlias:=0;
    FError:=0;
    end;
end;


Function TNet.Name : String;

begin
  Name:='';
  If (FNetEntry=Nil) or (FError<>0) then exit;
  Name:=StrPas(FNetEntry^.Name);
end;

Function TNet.GetAlias (Select : TSelectType) : String;

begin
  GetAlias:='';
  If (FNetEntry=Nil) or (FError<>0) then exit;
  Case Select of
    stFirst     : FAlias:=0;
    stnext      : If FNetEntry^.Aliases[FAlias]<>Nil then
                    Falias:=Falias+1;
    stprevious  : If FAlias=0 Then Exit else FAlias:=FAlias-1;
  end;
  If FNetEntry^.Aliases[FAlias]<>Nil then
    GetAlias:=StrPas(FNetEntry^.Aliases[FAlias]);
end;

Function TNet.IPstring : String;

begin
  IPString:='';
  If (FNetEntry=Nil) or (FError<>0) then exit;
  IPString:=NetAddrToStr(FNetEntry^.Net);
end;

Function TNet.IPaddress : Longint;

begin
  IPAddress:=0;
  If (FNetEntry=Nil) or (FError<>0) then exit;
  IPAddress:=FNetEntry^.Net;
end;

Destructor TNet.Done;

begin
end;

Function TNet.LastError : Longint;

begin
  LastError:=FError;
end;

Constructor TService.NameLookup (ServiceName,Proto : String);

begin
  ServiceName:=ServiceName+#0;
  Proto:=Proto+#0;
  FServiceEntry:=GetServByName(pchar(@ServiceName[1]),pchar(@Proto[1]));
  If FServiceEntry=Nil then
    FError:=GetDNSError
  else
    begin
    FAlias:=0;
    Ferror:=0;
    end;
end;

Constructor TService.PortLookup (APort: Longint; Proto : String);

begin
  Proto:=proto+#0;
  FServiceEntry:=GetServByPort(APort,pchar(@proto[1]));
  If FServiceEntry=Nil then
    FError:=GetDNSError
  else
    begin
    FAlias:=0;
    FError:=0;
    end;
end;


Function TService.Name : String;

begin
  Name:='';
  If (FServiceEntry=Nil) or (FError<>0) then exit;
  Name:=StrPas(FServiceEntry^.Name);
end;

Function TService.GetAlias (Select : TSelectType) : String;

begin
  GetAlias:='';
  If (FServiceEntry=Nil) or (FError<>0) then exit;
  Case Select of
    stFirst     : FAlias:=0;
    stnext      : If FServiceEntry^.Aliases[FAlias]<>Nil then
                    Falias:=Falias+1;
    stprevious  : If FAlias=0 Then Exit else FAlias:=FAlias-1;
  end;
  If FServiceEntry^.Aliases[FAlias]<>Nil then
    GetAlias:=StrPas(FServiceEntry^.Aliases[FAlias]);
end;

Function TService.Protocol : String;

begin
  Protocol:='';
  If (FServiceEntry=Nil) or (FError<>0) then exit;
  Protocol:=Strpas(FServiceEntry^.proto);
end;

Function TService.Port : Longint;

begin
  Port:=0;
  If (FServiceEntry=Nil) or (FError<>0) then exit;
  Port:=FServiceEntry^.Port;
end;

Destructor TService.Done;

begin
end;

Function TService.LastError : Longint;

begin
  LastError:=FError;
end;

Function HostToNet (Host : Longint) : Longint;

begin
  HostToNet:=THostAddr(host)[1];
  HostToNEt:=HostTONet or ( (THostAddr(host)[2]) shl 8);
  HostToNEt:=HostToNet or ( (THostAddr(host)[3]) shl 16);
  HostToNEt:=HostToNet or ( (THostAddr(host)[4]) shl 24);
end;

Function NetToHost (Net : Longint) : Longint;

begin
  NetToHost:=THostAddr(Net)[1];
  NetToHost:=NetToHost or ( (THostAddr(Net)[2]) shl 8);
  NetToHost:=NetToHost or ( (THostAddr(Net)[3]) shl 16);
  NetToHost:=NetToHost or ( (THostAddr(Net)[4]) shl 24);
end;

Function ShortHostToNet (Host : Word) : Word;

begin
  ShortHostToNet:=lo(host)*256+Hi(Host);
end;

Function ShortNetToHost (Net : Word) : Word;

begin
  ShortNetToHost:=lo(Net)*256+Hi(Net);
end;

end.


   $Log$
   Revision 1.2  2002-09-07 15:42:52  peter
     * old logs removed and tabs fixed

   Revision 1.1  2002/01/29 17:54:53  peter
     * splitted to base and extra

}
