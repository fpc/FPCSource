//  libetc.h: Light Gun Interface
unit libgun;
interface

procedure InitGUN(bufA: Pbyte; lenA: longint; bufB: Pbyte; lenB: longint; buf1: Pbyte; buf2: Pbyte; len: longint); external;
procedure RemoveGUN; external;
procedure SelectGUN(ch: longint; mask: byte); external;
function StartGUN: longint; external;
procedure StopGUN; external;

implementation
begin
end.