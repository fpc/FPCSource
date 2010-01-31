{ %fail }

{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{$mode objfpc}
{$modeswitch objectivec1}

type
  tc = objcclass(NSObject)
    s: ansistring;
  end;

begin
end.
