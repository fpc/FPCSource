{$IFNDEF FPC_DOTTEDUNITS}
unit libtap;
{$ENDIF FPC_DOTTEDUNITS}
interface

procedure InitTAP(bufA: pbyte; lenA: longint; bufB: pbyte; lenB: longint); external;
procedure StartTAP; external;
procedure StopTAP; external;
procedure EnableTAP; external;
procedure DisableTAP; external;

implementation
begin
end.