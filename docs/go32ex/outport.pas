{ This example demonstrates the use of the outport functions.

It simply turns the PC's internal speaker on for 50 ms and off again
}
uses
        crt,
        go32;

begin
        { turn on speaker }
        outportb($61, $ff);
        { wait a little bit }
        delay(50);
        { turn it off again }
        outportb($61, $0);
end.