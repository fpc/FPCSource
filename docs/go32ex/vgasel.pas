{This example demonstrates the use of the segment_to_descriptor()
function.

It switches to VGA mode 13h (320x200x256 color), creates a selector
to the memory (based at $A000:0000), clears this memory with color
15 (white) and waits until the enter key is pressed }

uses go32;

var
        vgasel : Word;
        r : trealregs;

begin
        { set VGA mode 13h }
        r.eax := $13; realintr($10, r);
        { allocate descriptor to VGA memory quickly; it could be done
        with allocate_ldt_descriptors() too, but we would have to
        initialize it by ourselves... unlike segment_to_descriptor()
        which automatically sets the limit and the base address
        correctly }
        vgasel := segment_to_descriptor($A000);
        { simply fill the screen memory with color 15 }
        seg_fillchar(vgasel, 0, 64000, #15);
        { wait for a return press }
        readln;
        { back to text mode }
        r.eax := $3; realintr($10, r);
        { don't deallocate vgasel, that can't be done }
end.