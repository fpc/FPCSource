//  libtap.h: Multi Tap Interface
unit libtap;
interface

procedure InitTAP(bufA: pbyte; lenA: longint; bufB: pbyte; lenB: longint); external;
procedure StartTAP; external;
procedure StopTAP; external;
procedure EnableTAP; external;
procedure DisableTAP; external;

implementation
begin
end.