{ %target=darwin }
{ %cpu=powerpc,i386 }

{$mode objfpc}
{$modeswitch objectivec1}

type
  ttest = NSObject;

var
  a: ttest;
begin
  a:=ttest(ttest.alloc).init;
  if a._class<>NSObject.classClass then
    halt(1);
  a.release;
end.
