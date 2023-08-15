{ %norun }

{$mode objfpc}

unit tw40395;

interface

implementation

var x: TRTLCriticalSection;

initialization
x := default(TRTLCriticalSection);
InitCriticalSection(x);

finalization
DoneCriticalsection(x);

end.
