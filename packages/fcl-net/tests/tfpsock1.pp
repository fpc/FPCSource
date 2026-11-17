program addrtest;

{$mode objfpc}{$H+}

uses
  fpsockets;

const
  IN4Val = '127.0.0.1';
  IN6Val = '132:42::1';
  IN6ValAlt = '132:42:0::1';

Function TestIN4Addr: String;

var
  Addr: TNetworkAddress;

begin
  Result:='';
  Addr:=NetAddr(IN4Val);
  if (Addr.AddressType<>atIN4) or (Addr.Address<>IN4Val) then
    Exit('Error with IN4Address Constructor function');
end;

Function TestIN6Addr: String;

var
  Addr: TNetworkAddress;

begin
  Result:='';
  Addr:=NetAddr(IN6Val);
  if (Addr.AddressType<>atIN6) or (Addr.Address<>IN6Val) then
    Exit('Error with IN6Address Constructor function');
end;

Function TestAddrDispatch: String;

var
  Addr: TNetworkAddress;

begin
  Result:='';
  Addr:=NetAddr(IN4Val);
  if (Addr.AddressType<>atIN4) or (Addr.Address<>IN4Val) then
    Exit('Error with INAddr Constructor function');
  Addr:=NetAddr(IN6Val);
  if (Addr.AddressType<>atIN6) or (Addr.Address<>IN6Val) then
    Exit('Error with INAddr Constructor function');
  Addr:=IN4Val;
  if (Addr.AddressType<>atIN4) or (Addr.Address<>IN4Val) then
    Exit('Error with Address Assignment Operator');
  Addr:=IN6Val;
  if (Addr.AddressType<>atIN6) or (Addr.Address<>IN6Val) then
    Exit('Error with Address Assignment Operator');
end;


Function TestIN6Equality: String;

var
  A1, A2: TNetworkAddress;

begin
  Result:='';
  if not IN6Equal(IN6Val, IN6ValAlt) then
    Exit('IN6 Comparison failed');
  A1:=IN6Address(IN6Val);
  A2:=IN6Address(IN6ValAlt);
  if not (A1 = A2) then
    Exit('IN6 = Comparison failed');
  if A1 <> A2 then
    Exit('IN6 <> Comparison failed');
end;

Function TestIPv4Mapping: String;

var
  Mapped, Extracted: TNetworkAddress;

begin
  Result:='';
  Mapped:=IN4MappedIN6Address('172.18.200.255');
  if not IsIPv4Mapped(Mapped) then
    Exit('IN4MappedIN6Address result not recognized as mapped');
  Extracted:=ExtractIPv4Address(Mapped);
  if (Extracted.AddressType<>atIN4) or (Extracted.Address<>'172.18.200.255') then
    Exit('ExtractIPv4Address returned '+Extracted.Address+' for 172.18.200.255');
  Extracted:=ExtractIPv4Address(IN4MappedIN6Address(IN4Val));
  if Extracted.Address<>IN4Val then
    Exit('ExtractIPv4Address returned '+Extracted.Address+' for '+IN4Val);
end;

Procedure DoTest(aTest,aResult : String);

begin
  if aResult<>'' then
    begin
    writeln(aTest,' failed : ',aResult);
    Halt(1);
    end
  else
    Writeln(aTest,' OK.');
end;


begin
  DoTest('TestIN4Addr',TestIN4Addr);
  DoTest('TestIN6Addr',TestIN6Addr);
  DoTest('TestAddrDispatch',TestAddrDispatch);
  DoTest('TestIN6Equality',TestIN6Equality);
  DoTest('TestIPv4Mapping',TestIPv4Mapping);
end.

