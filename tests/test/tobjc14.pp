{ %OPT=-Sew -vw }
{ %norun }

{$mode objfpc}
{$modeswitch objectivec1}

{ test forward-declared objcprotocol }
{ make sure no warning is given about the absense of any constructors }

type
  ta = objcprotocol;

  tb = objcclass
    a: ta;
  end; external name 'NSObject';

  ta = objcprotocol
  end; external name 'NSObject';

begin
end.
