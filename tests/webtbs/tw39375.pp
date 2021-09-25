{ %target=darwin,ios,iphonesim }

program Project1;
{$mode objfpc}{$H+}
{$ModeSwitch objectivec1}
type
CBCentralManager = pointer;
CBCentralManagerDelegateProtocol = objcprotocol external
  procedure centralManagerDidUpdateState (central: CBCentralManager); message 'centralManagerDidUpdateState:';
end;

Test=objccategory(NSObject,CBCentralManagerDelegateProtocol)
procedure centralManagerDidUpdateState (central: CBCentralManager);
end;

var
  called: boolean = false;

procedure Test.centralManagerDidUpdateState (central: CBCentralManager);
begin
  called:=true;
end;

var
  o: NSObject;
begin
  o := NSObject.alloc.init;
  o.centralManagerDidUpdateState(nil);
  if not called then
    halt(1);
end.
