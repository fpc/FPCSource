{ %norun }
program BuggyProgram;
    
const
SE_SysEvtMask = $144;
SE_SysEvtMask2 = $14422241;

type
intPtr = ^integer;

function GetSystemEventMask: INTEGER;
    begin
        GetSystemEventMask := intPtr(SE_SysEvtMask)^;
        GetSystemEventMask := intPtr(SE_SysEvtMask2)^;
    end;
procedure StoreSystemEventMask (theMask: INTEGER);
    begin
        intPtr(SE_SysEvtMask)^ := theMask;
        intPtr(SE_SysEvtMask2)^ := theMask;
    end;
    
begin
end.
