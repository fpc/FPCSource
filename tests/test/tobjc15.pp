{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

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
