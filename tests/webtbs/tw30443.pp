{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm,aarch64 }

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

uses
  CocoaAll;

procedure CheckProtocol;
const
  NSObjectProtocolName: UTF8String = 'NSObject';
var
  nso: id;
  proto: pobjc_protocol;
  cls: pobjc_class;
begin
  nso := NSObject.alloc.init;
  cls := object_getClass(nso);
  proto := objc_getProtocol(PAnsiChar(NSObjectProtocolName));
  if class_conformsToProtocol(cls, proto) then
    WriteLn('NSObject implements the NSObject protocol, as expected.')
  else
    halt(1);
  nso.release;
end;

begin
  CheckProtocol;
end.
