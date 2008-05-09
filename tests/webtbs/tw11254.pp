{ %norun }

{$mode macpas}
{$calling mwpascal}

unit tw11254;

interface

procedure Iterate( function theCallback( theDataPtr: Pointer): Integer);

implementation
procedure Iterate( function theCallback( theDataPtr: Pointer): Integer); begin end;

end.
