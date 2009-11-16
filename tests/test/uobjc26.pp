{$modeswitch objectivec1}

unit uobjc26;

interface

type
  ta = objcclass(NSObject)
    l: longint;
    procedure taproc; message 'taproc';
  end;

implementation

type
  ca = objccategory(ta)
    procedure implementationcategorymethod; message 'implementationcategorymethod';
  end;

procedure ca.implementationcategorymethod;
begin
  l:=1;
end;

procedure ta.taproc;
begin
  l:=0;
  implementationcategorymethod;
  if l<>1 then
    halt(1);
end;

end.
