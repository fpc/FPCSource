{ %norun }

{$mode objfpc}

unit tw40395a;

interface

implementation

var x: TRTLCriticalSection;

finalization
x := default(TRTLCriticalSection);
InitCriticalSection(x);
DoneCriticalsection(x);

end.
