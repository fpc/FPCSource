program HelloOS2;

var A,B: ^word;

begin
    WriteLn ('Hello World.');
    case os_mode of
     osDOS: WriteLn ('Running under DOS.');
     osDPMI: WriteLn ('Running under DPMI (RSX extender).');
     else WriteLn ('Running under OS/2.');
    end;
    WriteLn ('Free memory: ', MemAvail);
    WriteLn ('Largest block: ', MaxAvail);
    WriteLn ('Heap start: ',longint(heaporg));
    WriteLn ('Heap end: ',longint(heapend));
    WriteLn ('Memory allocation.');
    GetMem (A, 1000);
    GetMem (B, 2000);
    A^ := 2;
    B^ := 10;
    WriteLn ('Free memory: ', MemAvail);
    WriteLn ('Largest block: ', MaxAvail);
    FreeMem (A, 1000);
    FreeMem (B, 2000);
end.
