{$mode macpas}
{$calling mwpascal}

unit tw11254a;

interface

type
  t1 = function ( theDataPtr: Pointer): Integer;
  t2 = function ( theDataPtr: Pointer): Integer;

procedure Iterate( theCallback: t1);

implementation
procedure Iterate( theCallback: t2); begin end;

end.
