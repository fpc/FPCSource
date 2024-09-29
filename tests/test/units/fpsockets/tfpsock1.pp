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
  Addr:=IN4Address(IN4Val);
  if (Addr.AddressType<>atIN4) or (Addr.Address<>IN4Val) then
    Exit('Error with IN4Address Constructor function');
end;

Function TestIN6Addr: String;

var
  Addr: TNetworkAddress;

begin
  Result:='';
  Addr:=IN6Address(IN6Val);
  if (Addr.AddressType<>atIN6) or (Addr.Address<>IN6Val) then
    Exit('Error with IN6Address Constructor function');
end;

Function TestAddrDispatch: String;

var
  Addr: TNetworkAddress;

begin
  Result:='';
  Addr:=INAddr(IN4Val);
  if (Addr.AddressType<>atIN4) or (Addr.Address<>IN4Val) then
    Exit('Error with INAddr Constructor function');
  Addr:=INAddr(IN6Val);
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
end.

