{ %fail }
{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{$mode objfpc}
{$modeswitch objectivec1}

type
  tmyprotocol = objcprotocol;
  tmyclass = objcclass(NSObject,tmyprotocol)
  end;

  tmyprotocol = objcprotocol
  end;

begin
end.
