{ %fail }
{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{$modeswitch objectivec1}

type
  ta = objcclass
    { no destructors in Objective-C }
    destructor done; message 'done';
  end; external;

begin
end.
