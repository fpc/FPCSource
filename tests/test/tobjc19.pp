{ %fail }
{ %target=darwin }
{ %cpu=powerpc,i386 }

{$modeswitch objectivec1}

type
  ta = objcclass
    { should give an error because the selector is invalid }
    procedure test(l:longint); message 'test:l';
  end;

begin
end.
