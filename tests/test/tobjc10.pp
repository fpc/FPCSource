{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{$mode objfpc}
{$modeswitch objectivec1}

var
  a: NSObject;
begin
  a:=NSObject(NSObject(NSObject.alloc).init);
  if a.conformsToProtocol_(objcprotocol(NSObjectProtocol)) then
    writeln('ok conformsToProtocol')
  else
    halt(1);
end.
