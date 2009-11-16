{$modeswitch objectivec1}

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
