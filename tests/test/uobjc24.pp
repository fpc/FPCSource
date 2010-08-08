{$modeswitch objectivec1}

{ Written by Jonas Maebe in 2009, released into the public domain }

unit uobjc24;

interface

type
  ta = objcclass(NSObject)
  end;

implementation

type
  ca = objccategory(ta)
    procedure implementationcategorymethod;
  end;

procedure ca.implementationcategorymethod;
begin
end;

end.
