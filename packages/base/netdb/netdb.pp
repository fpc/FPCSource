{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Implement networking routines.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

unit netdb;

Interface

{$i hsh.inc} // disappears if part of resolve.pp !!

Const
  DNSPort        = 53;
  MaxServers     = 4;
  MaxResolveAddr = 10;
  SResolveFile   = '/etc/resolv.conf';
  SServicesFile  = '/etc/services'; 
  SHostsFile     = '/etc/hosts';
  SNetworksFile  = '/etc/networks';
  
Type
  TDNSServerArray = Array[1..MaxServers] of THostAddr;
  TServiceEntry = record
    Name     : String;
    Protocol : String;
    Port     : Word;
    Aliases  : String;
  end;
     
  THostEntry = record
    Name : String;
    Addr : THostAddr;
    Aliases : String;
  end;  
  
  TNetworkEntry = Record
    Name : String;
    Addr : TNetAddr;
    Aliases : String;
  end;  
  
Var  
  DNSServers            : TDNSServerArray;
  DNSServerCount        : Integer;
  DefaultDomainList     : String;
  CheckResolveFileAge   : Boolean; 
  TimeOutS,TimeOutMS    : Longint;
  
  
Function GetDNSServers(FN : String) : Integer;

Function ResolveName(HostName : String; Var Addresses : Array of THostAddr) : Integer;
Function ResolveAddress(HostAddr : THostAddr; Var Addresses : Array of String) : Integer;

Function ResolveHostByName(HostName : String; Var H : THostEntry) : Boolean;
Function ResolveHostByAddr(HostAddr : THostAddr; Var H : THostEntry) : Boolean;

Function GetHostByName(HostName: String;  Var H : THostEntry) : boolean;
Function GetHostByAddr(Addr: THostAddr;  Var H : THostEntry) : boolean;

Function GetNetworkByName(NetName: String; Var N : TNetworkEntry) : boolean;
Function GetNetworkByAddr(Addr: THostAddr; Var N : TNetworkEntry) : boolean;

Function GetServiceByName(Const Name,Proto : String; Var E : TServiceEntry) : Boolean;
Function GetServiceByPort(Port : Word;Const Proto : String; Var E : TServiceEntry) : Boolean;

Implementation

uses 
{$ifdef VER1_0}
   Linux,
{$else}
   Unix,
{$endif}
   sockets,sysutils;

{$i hs.inc}

const
  DNSQRY_A     = 1;                     // name to IP address 
  DNSQRY_AAAA  = 28;                    // name to IP6 address
  DNSQRY_PTR   = 12;                    // IP address to name 
  DNSQRY_MX    = 15;                    // name to MX 
  DNSQRY_TXT   = 16;                    // name to TXT

  // Flags 1
  QF_QR     = $80;
  QF_OPCODE = $78;
  QF_AA     = $04;
  QF_TC     = $02;  // Truncated.
  QF_RD     = $01;

  // Flags 2
  QF_RA     = $80;
  QF_Z      = $70;
  QF_RCODE  = $0F;


   
Type 
  TPayLoad  = Array[0..511] of char;
  TQueryData = packed Record
    id      : Array[0..1] of Byte;
    flags1  : Byte;
    flags2  : Byte; 
    qdcount : word;
    ancount : word;
    nscount : word;
    arcount : word;
    Payload : TPayLoad;
  end;
  
  TRRData = Packed record       // RR record
    Atype    : Word;            // Answer type
    AClass   : Word;
    TTL      : Cardinal;
    RDLength : Word;
  end;

Var
  ResolveFileAge  : Longint;
  ResolveFileName : String;

{ ---------------------------------------------------------------------
    Auxiliary functions.
  ---------------------------------------------------------------------}

Function htons(var W : Word) : word;

begin
  w:=Swap(w);
  Result:=W;
end;

Function ntohs(var W : Word) : Word;

begin
  w:=Swap(w);
  Result:=W;
end;


{ ---------------------------------------------------------------------
   Resolve.conf handling
  ---------------------------------------------------------------------}
  
Function GetDNSServers(Fn : String) : Integer;

Var
  R : Text;
  L : String;
  I : Integer;
  H : THostAddr;

  Function CheckDirective(Dir : String) : Boolean;
  
  Var
    P : Integer;
  
  begin
    P:=Pos(Dir,L);
    Result:=(P<>0);
    If Result then
      begin
      Delete(L,1,P+Length(Dir));
      Trim(L);
      end;
  end;
   
begin
  Result:=0;
  ResolveFileName:=Fn;
  ResolveFileAge:=FileAge(FN);
  {$i-}
  Assign(R,FN);
  Reset(R);
  {$i+}
  If (IOResult<>0) then 
    exit;
  Try  
    While not EOF(R) do
      begin
      Readln(R,L);
      I:=Pos('#',L);
      If (I<>0) then
        L:=Copy(L,1,I-1);
      If CheckDirective('nameserver') then
        begin
        H:=HostToNet(StrToHostAddr(L));
        If (H[1]<>0) then
          begin
          Inc(Result);
          DNSServers[Result]:=H;
          end;
        end
      else if CheckDirective('domain') then
        DefaultDomainList:=L
      else if CheckDirective('search') then
        DefaultDomainList:=L;
      end;
  Finally
    Close(R);
  end;    
  DNSServerCount:=Result;
end;

Procedure CheckResolveFile;

Var
  F : Integer;

begin
  If CheckResolveFileAge then
    begin
    F:=FileAge(ResolveFileName);
    If ResolveFileAge<F then
      GetDnsServers(ResolveFileName);
    end;  
end;

{ ---------------------------------------------------------------------
    Payload handling functions.
  ---------------------------------------------------------------------}
  

Procedure DumpPayLoad(Q : TQueryData; L : Integer);

Var 
  i : Integer;

begin
  Writeln('Payload : ',l);
  For I:=0 to L-1 do
    Write(Byte(Q.Payload[i]),' ');
  Writeln;  
end;
  
Function BuildPayLoad(Var Q : TQueryData; Name : String; RR : Word; QClass : Word) : Integer;

Var
  P : PByte;
  l,S : Integer;
  
begin
  Result:=-1;
  If length(Name)>506 then
    Exit;
  Result:=0;  
  P:=@Q.Payload;
  Repeat
    L:=Pos('.',Name);
    If (L=0) then
      S:=Length(Name)
    else
      S:=L-1;
    P[Result]:=S;
    Move(Name[1],P[Result+1],S);
    Inc(Result,S+1);
    If (L>0) then
      Delete(Name,1,L);
  Until (L=0);
  P[Result]:=0;
  htons(rr);
  Move(rr,P[Result+1],2);
  Inc(Result,3);
  htons(QClass);
  Move(qclass,P[Result],2);
  Inc(Result,2);
end;



Function NextRR(Const PayLoad : TPayLoad;Var Start : LongInt; AnsLen : LongInt; Var RR : TRRData) : Boolean;

Var
  I : Integer;
  HaveName : Boolean;
  PA : ^TRRData;
  RClass,RType : Word;
  
begin
  Result:=False;
  I:=Start;
  // Skip labels and pointers. At least 1 label or pointer is present.
  Repeat
    HaveName:=True;
    If (Payload[i]>#63) then // Pointer, skip
      Inc(I,2)
    else If Payload[i]=#0 then // Null termination of label, skip.
      Inc(i)
    else  
      begin
      Inc(I,Ord(Payload[i])+1); // Label, continue scan.
      HaveName:=False;
      end;
  Until HaveName or (I>(AnsLen-SizeOf(TRRData)));
  Result:=(I<=(AnsLen-SizeOf(TRRData)));
  // Check RR record.
  PA:=@Payload[i];
  RR:=PA^;
  Start:=I+SizeOf(TRRData);
end;


Function BuildName (Const PayLoad : TPayLoad; Start,len : Integer) : String;

Const
  FIREDNS_POINTER_VALUE = $C000;
  
Var
  I,O : Integer;
  P : Word;
  
begin
  SetLength(Result,512);
  I:=Start;
  O:=1;
  // Copy labels and pointers. At least 1 label or pointer is present.
  Repeat
    If (Payload[i]>#63) then // Pointer, move.
      begin
      Move(Payload[i],P,2);
      I:=ntohs(p)-FIREDNS_POINTER_VALUE-12;
      end
    else if Payload[i]<>#0 then // Label, copy
      begin
      If O<>1 then
        begin
        Result[O]:='.';
        Inc(O);
        end;
      P:=Ord(Payload[i]);  
      Move(Payload[i+1],Result[o],P);
      Inc(I,P+1);
      Inc(O,P);
      end;
   Until (Payload[I]=#0);    
end;


{ ---------------------------------------------------------------------
    QueryData handling functions
  ---------------------------------------------------------------------}
  
Function CheckAnswer(Const Qry : TQueryData; Var Ans : TQueryData) : Boolean;

begin
  Result:=False;
  With Ans do
    begin
    // Check ID.
    If (ID[1]<>QRY.ID[1]) or (ID[0]<>Qry.ID[0]) then
      exit;  
    // Flags ?
    If (Flags1 and QF_QR)=0 then
      exit;
    if (Flags1 and QF_OPCODE)<>0 then 
      exit;  
    if (Flags2 and QF_RCODE)<>0 then
      exit;  
    // Number of answers ?  
    htons(Ancount);
    If Ancount<1 then
      Exit;
    Result:=True;
    end;
end;

Function SkipAnsQueries(Var Ans : TQueryData; L : Integer) : integer;

Var
  Q,I : Integer;

begin
  Result:=0;
  With Ans do
    begin
    htons(qdcount);
    i:=0;
    q:=0;
    While (Q<qdcount) and (i<l) do  
      begin
      If Ord(Payload[i])>63 then
        begin
        Inc(I,6);
        Inc(Q);
        end
      else
        begin
        If Payload[i]=#0 then
          begin
          inc(q);
          Inc(I,5);
          end
        else
          Inc(I,Ord(Payload[i])+1);  
        end;  
      end;
    Result:=I;  
    end;  
end;

{ ---------------------------------------------------------------------
    DNS Query functions.
  ---------------------------------------------------------------------}
  

Function Query(Resolver : Integer; Var Qry,Ans : TQueryData; QryLen : Integer; Var AnsLen : Integer) : Boolean;

Var
  BA,SA : TInetSockAddr;
  Sock,L,I : Longint;
  Al,RTO : Longint;
  ReadFDS : FDSet;
  
begin
  Result:=False;
  With Qry do
    begin
    ID[0]:=Random(256);
    ID[1]:=Random(256);
    Flags1:=QF_RD;
    Flags2:=0;
    qdcount:=1 shl 8;
    ancount:=0;
    nscount:=0;
    arcount:=0;
    end;
  Sock:=Socket(PF_INET,SOCK_DGRAM,0);
  If Sock=-1 then 
    exit;
  With SA do
    begin
    family:=AF_INET;
    port:=DNSport;
    htons(port);
    addr:=cardinal(HostToNet(DNSServers[Resolver]));
    end;
  sendto(sock,qry,qrylen+12,0,SA,SizeOf(SA));
  // Wait for answer.
  RTO:=TimeOutS*1000+TimeOutMS;
  FD_ZERO(ReadFDS);
  FD_Set(Sock,readfds);
  if Select(Sock+1,@readfds,Nil,Nil,RTO)<=0 then
    begin
    fdclose(Sock);
    exit;
    end;
  AL:=SizeOf(SA);
  L:=recvfrom(Sock,ans,SizeOf(Ans),0,SA,AL);
  fdclose(Sock);
  // Check lenght answer and fields in header data.
  If (L<12) or not CheckAnswer(Qry,Ans) Then
    exit;
  // Return Payload length.  
  Anslen:=L-12;  
  Result:=True;  
end;

Function ResolveNameAt(Resolver : Integer; HostName : String; Var Addresses : Array of THostAddr) : Integer;

Var
  Qry, Ans            : TQueryData;
  MaxAnswer,I,QryLen,
  AnsLen,AnsStart     : Longint;
  RR                  : TRRData;
  
begin
  Result:=0;
  QryLen:=BuildPayLoad(Qry,HostName,DNSQRY_A,1);
  If Not Query(Resolver,Qry,Ans,QryLen,AnsLen) then
    Result:=-1
  else  
    begin
    AnsStart:=SkipAnsQueries(Ans,AnsLen);
    MaxAnswer:=Ans.AnCount-1;
    If MaxAnswer>High(Addresses) then
      MaxAnswer:=High(Addresses);
    I:=0;
    While (I<=MaxAnswer) and NextRR(Ans.Payload,AnsStart,AnsLen,RR) do
      begin
      if (Ntohs(RR.AType)=DNSQRY_A) and (1=NtoHS(RR.AClass)) then
        begin
        Move(Ans.PayLoad[AnsStart],Addresses[i],SizeOf(THostAddr));
        inc(Result);
        Inc(AnsStart,RR.RDLength);
        end;
      Inc(I);
      end;  
    end;
end;

Function ResolveName(HostName : String; Var Addresses : Array of THostAddr) : Integer;

Var
  I : Integer;

begin
  CheckResolveFile;
  I:=1;
  Result:=0;
  While (Result=0) and (I<=DNSServerCount) do
    begin
    Result:=ResolveNameAt(I,HostName,Addresses);
    Inc(I);
    end;
end;

Function ResolveAddressAt(Resolver : Integer; Address : String; Var Names : Array of String) : Integer;


Var
  Qry, Ans            : TQueryData;
  MaxAnswer,I,QryLen,
  AnsLen,AnsStart     : Longint;
  RR                  : TRRData;
  S                   : String;
begin
  Result:=0;
  QryLen:=BuildPayLoad(Qry,Address,DNSQRY_PTR,1);
  If Not Query(Resolver,Qry,Ans,QryLen,AnsLen) then
    Result:=-1
  else  
    begin
    AnsStart:=SkipAnsQueries(Ans,AnsLen);
    MaxAnswer:=Ans.AnCount-1;
    If MaxAnswer>High(Names) then
      MaxAnswer:=High(Names);
    I:=0;
    While (I<=MaxAnswer) and NextRR(Ans.Payload,AnsStart,AnsLen,RR) do
      begin
      if (Ntohs(RR.AType)=DNSQRY_PTR) and (1=NtoHS(RR.AClass)) then
        begin
        Names[i]:=BuildName(Ans.Payload,AnsStart,AnsLen);
        inc(Result);
        Inc(AnsStart,RR.RDLength);
        end;
      Inc(I);
      end;  
    end;
end;


Function ResolveAddress(HostAddr : THostAddr; Var Addresses : Array of String) : Integer;

Var
  I : Integer;
  S : String;
  
begin
  CheckResolveFile;
  I:=1;
  Result:=0;
  S:=Format('%d.%d.%d.%d.in-addr.arpa',[HostAddr[4],HostAddr[3],HostAddr[2],HostAddr[1]]);
  While (Result=0) and (I<=DNSServerCount) do
    begin
    Result:=ResolveAddressAt(I,S,Addresses);
    Inc(I);
    end;
end;

Function ResolveHostByName(HostName : String; Var H : THostEntry) : Boolean;

Var
  Address : Array[1..MaxResolveAddr] of THostAddr;
  L : Integer;
  
begin
  L:=ResolveName(HostName,Address);
  Result:=(L>0);
  If Result then
    begin
    // We could add a reverse call here to get the real name and aliases.
    H.Name:=HostName;
    H.Addr:=Address[1];
    H.aliases:='';
    end;
end;

Function ResolveHostByAddr(HostAddr : THostAddr; Var H : THostEntry) : Boolean;

Var
  Names : Array[1..MaxResolveAddr] of String;
  I,L : Integer;
  
begin
  L:=ResolveAddress(HostAddr,Names);
  Result:=(L>0);
  If Result then
    begin
    H.Name:=Names[1];
    H.Addr:=HostAddr;
    H.Aliases:='';
    If (L>1) then
      For I:=2 to L do
        If (I=2) then
          H.Aliases:=Names[i]
        else  
          H.Aliases:=H.Aliases+','+Names[i];
    end;
end;

{ ---------------------------------------------------------------------
    Some Parsing routines
  ---------------------------------------------------------------------}

Const 
  Whitespace = [' ',#9];

Function NextWord(Var Line : String) : String;

Var 
  I,J : Integer;

begin
  I:=1;
  While (I<=Length(Line)) and (Line[i] in Whitespace) do
    inc(I);
  J:=I;
  While (J<=Length(Line)) and Not (Line[J] in WhiteSpace) do
    inc(j);
  Result:=Copy(Line,I,J-1);  
  Delete(Line,1,J);  
end;
  
Procedure StripComment(Var line : String);

Var
  P : Integer;

begin
  P:=Pos('#',Line);
  If (P<>0) then
    Line:=Trim(Copy(Line,1,P-1));
end;

Function MatchNameOrAlias(Const Entry,Name: String; Aliases : String) : Boolean;

Var
  P : Integer;
  A : String;

begin
  Result:=CompareText(Entry,Name)=0;
  If Not Result then
    While (Not Result) and (Length(Aliases)>0) do
      begin
      P:=Pos(',',Aliases);
      If (P=0) then
        P:=Length(Aliases)+1;
      A:=Copy(Aliases,1,P-1);
      Delete(Aliases,1,P);
      Result:=CompareText(A,Entry)=0;
      end;
end;

{ ---------------------------------------------------------------------
    /etc/hosts handling.
  ---------------------------------------------------------------------}

Function GetNextHostEntry(var F : Text; Var H : THostEntry): boolean;

Var
  Line,S : String;
  P : Integer;
  
begin
  Result:=False;
  Repeat
    ReadLn(F,Line);
    StripComment(Line);
    S:=NextWord(Line);
    If (S<>'') then
      begin
      H.Addr:=StrToHostAddr(S);
      if (H.Addr[1]<>0) then
        begin
        S:=NextWord(Line);
        If (S<>'') then
          begin
          H.Name:=S;
          Result:=True;
          H.Aliases:='';
          Repeat
            S:=NextWord(line);
            If (S<>'') then
              If (H.Aliases='') then
                H.Aliases:=S
              else
                H.Aliases:=H.Aliases+','+S;  
          until (S='');
          end;
        end;      
      end;
  until Result or EOF(F);
end;  

Function FindHostEntryInHostsFile(N: String; Addr: THostAddr; Var H : THostEntry) : boolean;

Var
  F : Text;
  HE : THostEntry;
  
begin
  Result:=False;
  If FileExists(SHostsFile) then
    begin
    Assign(F,SHostsFile);
    {$i-}
    Reset(F);
    {$i+}
    If (IOResult=0) then
      begin
      While Not Result and GetNextHostEntry(F,HE) do
        begin
        If (N<>'') then
          Result:=MatchNameOrAlias(N,HE.Name,HE.Aliases)
        else
          Result:=Cardinal(Addr)=Cardinal(HE.Addr);
        end; 
      Close(f);
      If Result then
        begin
        H.Name:=HE.Name;
        H.Addr:=HE.Addr;
        H.Aliases:=HE.Aliases;
        end;
      end;  
    end;
end;

Function GetHostByName(HostName: String;  Var H : THostEntry) : boolean;

begin
  Result:=FindHostEntryInHostsFile(HostName,NoAddress,H);
end;


Function GetHostByAddr(Addr: THostAddr;  Var H : THostEntry) : boolean;

begin
  Result:=FindHostEntryInHostsFile('',Addr,H);
end;

{ ---------------------------------------------------------------------
    /etc/networks handling
  ---------------------------------------------------------------------}

Function GetNextNetworkEntry(var F : Text; Var N : TNetworkEntry): boolean;

Var
  NN,Line,S : String;
  P : Integer;
  A : TNetAddr;
  
begin
  Result:=False;
  Repeat
    ReadLn(F,Line);
    StripComment(Line);
    S:=NextWord(Line);
    If (S<>'') then
      begin
      NN:=S;
      A:=StrToHostAddr(NextWord(Line));
      Result:=(NN<>'') and (A[1]<>0); // Valid addr.
      If result then
        begin
        N.Addr:=A;
        N.Name:=NN;
        N.Aliases:='';
        end;      
      end;
  until Result or EOF(F);
end;  

Function FindNetworkEntryInNetworksFile(Net: String; Addr: TNetAddr; Var N : TNetworkEntry) : boolean;

Var
  F : Text;
  NE : TNetworkEntry;
  
begin
  Result:=False;
  If FileExists(SNetworksFile) then
    begin
    Assign(F,SNetworksFile);
    {$i-}
    Reset(F);
    {$i+}
    If (IOResult=0) then
      begin
      While Not Result and GetNextNetworkEntry(F,NE) do
        begin
        If (Net<>'') then
          Result:=MatchNameOrAlias(Net,NE.Name,NE.Aliases)
        else
          Result:=Cardinal(Addr)=Cardinal(NE.Addr);
        end; 
      Close(f);
      If Result then
        begin
        N.Name:=NE.Name;
        N.Addr:=NE.Addr;
        N.Aliases:=NE.Aliases;
        end;
      end;  
    end;
end;
  
Function GetNetworkByName(NetName: String; Var N : TNetworkEntry) : boolean;

begin
  Result:=FindNetworkEntryInNetworksFile(NetName,NoNet,N);
end;

Function GetNetworkByAddr(Addr: THostAddr; Var N : TNetworkEntry) : boolean;

begin
  Result:=FindNetworkEntryInNetworksFile('',Addr,N);
end;

{ ---------------------------------------------------------------------
    /etc/services section
  ---------------------------------------------------------------------}

Function GetNextServiceEntry(Var F : Text; Var E : TServiceEntry) : Boolean;


Var
  Line,S : String;
  P : INteger;
  
begin
  Result:=False;
  Repeat
    ReadLn(F,Line);
    StripComment(Line);
    S:=NextWord(Line);
    If (S<>'') then
      begin
      E.Name:=S;
      S:=NextWord(Line);
      P:=Pos('/',S);
      If (P<>0) then
        begin
        E.Port:=StrToIntDef(Copy(S,1,P-1),0);
        If (E.Port<>0) then
          begin
          E.Protocol:=Copy(S,P+1,Length(S)-P);
          Result:=length(E.Protocol)>0;
          E.Aliases:='';
          Repeat
            S:=NextWord(Line);
            If (S<>'') then
              If (Length(E.Aliases)=0) then
                E.aliases:=S
              else  
                E.Aliases:=E.Aliases+','+S;
          until (S='');
          end;
        end;
      end;
  until Result or EOF(F);
end;


Function FindServiceEntryInFile(Const Name,Proto : String; Port : Integer; Var E : TServiceEntry) : Boolean;

Var
  F : Text;
  TE : TServiceEntry;
  
begin
  Result:=False;
  If FileExists(SServicesFile) then
    begin
    Assign(F,SServicesFile);
    {$i-}
    Reset(F);
    {$i+}
    If (IOResult=0) then
      begin
      While Not Result and GetNextServiceEntry(F,TE) do
        begin
        If (Port=-1) then
          Result:=MatchNameOrAlias(Name,TE.Name,TE.Aliases)
        else 
          Result:=(Port=TE.Port);
        If Result and (Proto<>'') then
          Result:=(Proto=TE.Protocol);
        end; 
      Close(f);
      If Result then
        begin
        E.Name:=TE.Name;
        E.Port:=TE.Port;
        E.Protocol:=TE.Protocol;
        E.Aliases:=TE.Aliases;
        end;
      end;  
    end;
end;

Function GetServiceByName(Const Name,Proto : String; Var E : TServiceEntry) : Boolean;

begin
  Result:=FindServiceEntryInFile(Name,Proto,-1,E);  
end;

Function GetServiceByPort(Port : Word;Const Proto : String; Var E : TServiceEntry) : Boolean;

begin
  Result:=FindServiceEntryInFile('',Proto,Port,E);  
end;

{ ---------------------------------------------------------------------
    Initialization section
  ---------------------------------------------------------------------}

Procedure InitResolver;

Var
  I : Integer;

begin
  TimeOutS :=5;
  TimeOutMS:=0;
  CheckResolveFileAge:=False;
  If FileExists(SResolveFile) then
    GetDNsservers(SResolveFile);
end;

begin
  InitResolver;
end.


{
  $Log$
  Revision 1.3  2003-05-17 20:54:03  michael
  + uriparser unit added. Header/Footer blocks added

}
