uses
        go32;

procedure dosalloc(var selector : word;
        var segment : word; size : longint);
var
        res : longint;
begin
        res := global_dos_alloc(size);
        selector := word(res);
        segment := word(res shr 16);
end;

procedure dosfree(selector : word);
begin
        global_dos_free(selector);
end;

type
        VBEInfoBuf = packed record
                Signature : array[0..3] of char;
                Version : Word;
                reserved : array[0..505] of byte;
        end;

var
        selector,
        segment : Word;

        r : trealregs;
        infobuf : VBEInfoBuf;

begin
        fillchar(r, sizeof(r), 0);
        fillchar(infobuf, sizeof(VBEInfoBuf), 0);
        dosalloc(selector, segment, sizeof(VBEInfoBuf));
        if (int31error<>0) then begin
                Writeln('Error while allocating real mode memory, halting');
                halt;
        end;
        infobuf.Signature := 'VBE2';
        dosmemput(segment, 0, infobuf, sizeof(infobuf));
        r.ax := $4f00; r.es := segment;
        realintr($10, r);
        dosmemget(segment, 0, infobuf, sizeof(infobuf));
        dosfree(selector);
        if (r.ax <> $4f) then begin
                Writeln('VBE BIOS extension not available, function call ',
                        'failed');
                halt;
        end;
        if (infobuf.signature[0] = 'V') and
                (infobuf.signature[1] = 'E') and
                (infobuf.signature[2] = 'S') and
                (infobuf.signature[3] = 'A') then begin
                Writeln('VBE version ', hi(infobuf.version), '.',
                        lo(infobuf.version), ' detected');
        end;
end.