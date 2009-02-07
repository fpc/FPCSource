{ %target=win32 }
{$mode objfpc}
uses sysutils
    , windows
    ;
var
    SharedInt : PInteger;
    target : integer;
    savetarget : integer;

begin
    target := 0;
    savetarget := system.InterlockedCompareExchange(Target, 1, 0);
    Writeln(format('%d = InterlockedCompareExchange(Target= %d , 1, 0)',[savetarget, target]));
    SharedInt := AllocMem(SizeOf(Integer));
    SharedInt^ := 0;
// here is runtime exception rised - access to invalid memory
    savetarget := windows.InterlockedCompareExchange(SharedInt^, 1, 0);
    Writeln(format('%d = InterlockedCompareExchange(SharedInt^= %d , 1, 0)',[savetarget, SharedInt^]));
    FreeMem(SharedInt);
end.
