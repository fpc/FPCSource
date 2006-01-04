{$mode delphi}
type
  ta = interface
    function a: longint;
  end;

  tb = interface(ta)
    function a: ansistring;
  end;

begin
end.
