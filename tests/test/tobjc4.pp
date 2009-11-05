{ %fail }
{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{$modeswitch objectivec1}

type
  ta = objcclass
    { no constructors in Objective-C }
    constructor create; message 'create';
  end; external;

begin
end.
