{ %fail }
{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{ Written by Jonas Maebe in 2010, released into the public domain }

{$mode delphi}
{$modeswitch objectivec1}

uses
  CocoaAll;

var
  arr: NSMutableArray;
  element: NSString;
  pool: NSAutoreleasePool;
  i: longint;
begin
  pool:=NSAutoreleasePool.alloc.init;
  arr:=NSMutableArray.arrayWithObjects(
    NSSTR('One'),
    NSSTR('Two'),
    NSSTR('Three'),
    NSSTR('Four'),
    NSSTR('Five'),
    NSSTR('Six'),
    NSSTR('Seven'),
    nil);

  i:=0;
  for element in arr do
    begin
      inc(i);
      if i=2 then
        continue;
      if i=5 then
        break;
      if i in [2,5..10] then
        halt(1);
      NSLog(NSSTR('element: %@'),element);
    end;
  pool.release;
end.
