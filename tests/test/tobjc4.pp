{ %fail }
{ %target=darwin }
{ %cpu=powerpc,i386 }

{$modeswitch objectivec1}

type
  ta = objcclass
    { no constructors in Objective-C }
    constructor create;
  end; external;

begin
end.
