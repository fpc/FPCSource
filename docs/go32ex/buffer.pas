{ This program demonstrates the usage of DOS real mode memory by
executing a software interrupt which needs a buffer to store data
into. Because these interrupts are real mode funcs, the buffer must
be located in real mode memory space (first MB of memory). Such
memory can only be allocated by the global_dos_alloc() and
global_dos_free() functions of the GO32 unit.

In more detail this program tries to detect a VESA 2.0 BIOS
extension of your graphics card and outputs its version.

Here's the necessary interrupt call description:

  Int 10h 4f00h : VESA BIOS extension installation check
  Input : AX = 4F00h
          ES:DI = pointer to 512 byte information buffer
  Output : AX = 004Fh if successful
           ES:DI = pointer to filled buffer

  Buffer structure : (relevant to this example)

           must be 'VESA' in the first 4 chars of the buffer to be
           valid VBE version in the next word

  Note : to request VBE 2.0 information, the first 4 bytes of the
        buffer must contain 'VBE2' prior to the interrupt call.

        (this makes the problem a bit tougher; we first have to copy the
        buffer with the 'VBE2' id to dos memory...)
}

uses
        go32;

{The following 2 functions are wrappers to the GO32
global_dos_alloc() and global_dos_free() functions to simplify their
usage }

{ Function : dosalloc }
{ Input    : size of a real mode location }
{ Output   : selector and segment of a real mode location }
procedure dosalloc(var selector : word;
        var segment : word; size : longint);
var
        res : longint;
begin
        { try to allocate real mode memory  }
        res := global_dos_alloc(size);
        { the lower 16 bits of the result contain the selector to the
        allocated memory block }
        selector := word(res);
        { the upper 16 bits contain the real mode segment address of
        this block; the offset is always 0, so we don't need to return
        this }
        segment := word(res shr 16);
end;

{ Function    : dosfree }
{ Input       : selector of a real mode block }
{ Output      : none }
{ Description : de-allocates a previously allocated real mode
memory}
procedure dosfree(selector : word);
begin
        { call the GO32 function with the selector }
        global_dos_free(selector);
end;

type
        VBEInfoBuf = packed record
                { contains 'VESA' if successful }
                Signature : array[0..3] of char;
                Version : Word;
                { pad to 512 bytes length }
                reserved : array[0..505] of byte;
        end;

var
        { selector to our real mode buffer }
        selector,
        { real mode segment address of buffer }
        segment : Word;

        { register structure to issue a software interrupt }
        r : trealregs;
        infobuf : VBEInfoBuf;

begin
        { first we reset the registers and infobuf variable }
        fillchar(r, sizeof(r), 0);
        fillchar(infobuf, sizeof(VBEInfoBuf), 0);
        { allocate real mode memory }
        dosalloc(selector, segment, sizeof(VBEInfoBuf));
        { check if an error occured during allocation }
        if (int31error<>0) then begin
                Writeln('Error while allocating real mode memory, halting');
                halt;
        end;
        { request VBE 2.0 information, fill out information buffer }
        infobuf.Signature := 'VBE2';
        { copy buffer to the allocated real mode memory }
        dosmemput(segment, 0, infobuf, sizeof(infobuf));
        { issue the interrupt; remember : DI = 0 }
        r.ax := $4f00; r.es := segment;
        realintr($10, r);
        { copy buffer to our infobuf variable again }
        dosmemget(segment, 0, infobuf, sizeof(infobuf));
        { free allocated real mode memory, because we don't need it
        anymore }
        dosfree(selector);
        { check if interrupt call was successful }
        if (r.ax <> $4f) then begin
                { write message and exit, because the infobuf doesn't contain
                any useful data we could tell the user }
                Writeln('VBE BIOS extension not available, function call ',
                        'failed');
                halt;
        end;
        { check if buffer is valid }
        if (infobuf.signature[0] = 'V') and
                (infobuf.signature[1] = 'E') and
                (infobuf.signature[2] = 'S') and
                (infobuf.signature[3] = 'A') then begin
                Writeln('VBE version ', hi(infobuf.version), '.',
                        lo(infobuf.version), ' detected');
        end;
end.