{ %fail }
{ %target=darwin }
{ %cpu=powerpc,i386 }

{$modeswitch objectivec1}

type
  ta = objcclass
    { no destructors in Objective-C }
    destructor done;
  end; external;

begin
end.
