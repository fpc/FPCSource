program protocoltest;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

type
  MyProtocolA = objcprotocol
    procedure newMethod; message 'newMethod';
  end;

  MyProtocolB = objcprotocol(MyProtocolA)
    class procedure newClassMethod; message 'newClassMethod';
  end;


  { TMyObject }

  TMyObjectA = objcclass(NSObject, MyProtocolA, MyProtocolB)
    procedure newMethod;
    class procedure newClassMethod;
  end;

  TMyObjectB = objcclass(NSObject,MyProtocolA)
    procedure newMethod; message 'newMethod';
    class procedure newClassMethod; message 'newClassMethod';
  end;

{ TMyObjectA }

procedure TMyObjectA.newMethod;
begin
end;

class procedure TMyObjectA.newClassMethod;
begin
end;

{ TMyObjectB }

procedure TMyObjectB.newMethod;
begin
end;

class procedure TMyObjectB.newClassMethod;
begin
end;


var
  pMyProtocolA : Protocol;
  pMyProtocolB : Protocol;
  pNSProxy     : Protocol;
  a   : TMyObjectA;
  b   : TMyObjectB;
begin
  pMyProtocolA:=objcprotocol(MyProtocolA);
  pMyProtocolB:=objcprotocol(MyProtocolB);
  pNSProxy:=Protocol(objc_getprotocol('NSProxy'));
  writeln('TMyObjectA conforms to MyProtocolA protocol: ',  TMyObjectA.classconformsToProtocol_(pMyProtocolA)); {true}
  if not TMyObjectA.classconformsToProtocol_(pMyProtocolA) then
    halt(1);
  writeln('TMyObjectA conforms to MyProtocolB protocol: ',  TMyObjectA.classconformsToProtocol_(pMyProtocolB)); {true}
  if not TMyObjectA.classconformsToProtocol_(pMyProtocolB) then
    halt(2);
  writeln('TMyObjectB conforms to MyProtocolA protocol: ',  TMyObjectB.classconformsToProtocol_(pMyProtocolA)); {true}
  if not TMyObjectB.classconformsToProtocol_(pMyProtocolA) then
    halt(3);
  writeln('TMyObjectB conforms to MyProtocolB protocol: ',  TMyObjectB.classconformsToProtocol_(pMyProtocolB)); {false}
  if TMyObjectB.classconformsToProtocol_(pMyProtocolB) then
    halt(4);
  writeln('TMyObjectA conforms to NSProxy protocol:     ',  TMyObjectA.classconformsToProtocol_(pNSProxy));    {false}
  if TMyObjectA.classconformsToProtocol_(pNSProxy) then
    halt(5);

  a := TMyObjectA.alloc;
  writeln('TMyObjectA instance conforms to MyProtocolA protocol: ',  a.classconformsToProtocol_(pMyProtocolA)); {true}
  if not a.classconformsToProtocol_(pMyProtocolA) then
    halt(6);
  writeln('TMyObjectA instance conforms to MyProtocolB protocol: ',  a.classconformsToProtocol_(pMyProtocolB)); {true}
  if not a.classconformsToProtocol_(pMyProtocolB) then
    halt(7);
  writeln('TMyObjectA instance conforms to NSProxy protocol:     ',  a.classconformsToProtocol_(pNSProxy));     {false}
  if a.classconformsToProtocol_(pNSProxy) then
    halt(8);
  a.Release;

  b := TMyObjectB.alloc;
  writeln('TMyObjectB instance conforms to MyProtocolA protocol: ',  b.conformsToProtocol_(pMyProtocolA)); {true}
  if not b.conformsToProtocol_(pMyProtocolA) then
    halt(6);
  writeln('TMyObjectB instance conforms to MyProtocolB protocol: ',  b.conformsToProtocol_(pMyProtocolB)); {false}
  if b.conformsToProtocol_(pMyProtocolB) then
    halt(7);
  b.Release;
end.

