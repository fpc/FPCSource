{ %fail }
{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{$modeswitch objectivec1}

type
  ta = objcclass
    { should give an error because the selector is invalid }
    procedure test(l:longint); message 'test:l';
  end;

begin
end.
