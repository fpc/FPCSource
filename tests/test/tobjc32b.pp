{ %fail }

{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm,aarch64 }

{$mode objfpc}
{$modeswitch objectivec1}

type
  tc = objcclass(NSObject)
    procedure test(s: ansistring); message 'test:';
  end;

procedure tc.test(s: ansistring);
  begin
  end;

begin
end.
