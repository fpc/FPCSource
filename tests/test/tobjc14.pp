{ %OPT=-Sew -vw }
{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm,aarch64 }
{ %norun }

{ Written by Jonas Maebe in 2009, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

{ test forward-declared objcprotocol }
{ make sure no warning is given about the absense of any constructors }

type
  ta = objcprotocol;

  tb = objcclass external name 'NSObject'
    a: ta;
  end;

  ta = objcprotocol external name 'NSObject'
  end; 

begin
end.
