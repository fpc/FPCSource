{$mode delphi}
uses
        crt,
        go32;

const
        maxx = 80;
        maxy = 25;
        bytespercell = 2;
        screensize = maxx * maxy * bytespercell;

        linB8000 = $B800 * 16;

type
        string80 = string[80];

var
        text_save : array[0..screensize-1] of byte;
        text_oldx, text_oldy : Word;

        text_sel : Word;

procedure status(s : string80);
begin
     gotoxy(1, 1); clreol; write(s); readkey;
end;

procedure selinfo(sel : Word);
begin
     gotoxy(1, 24);
     clreol; writeln('Descriptor base address : $',
        hexstr(get_segment_base_address(sel), 8));
     clreol; write('Descriptor limit : ', get_segment_limit(sel));
end;

function makechar(ch : char; color : byte) : Word;
begin
     result := byte(ch) or (color shl 8);
end;

begin
     seg_move(dosmemselector, linB8000, get_ds, longint(@text_save),
        screensize);
     text_oldx := wherex; text_oldy := wherey;
     seg_fillword(dosmemselector, linB8000, screensize div 2,
        makechar(' ', Black or (Black shl 4)));
     status('Creating selector ''text_sel'' to a part of ' +
        'text screen memory');
     text_sel := allocate_ldt_descriptors(1);
     set_segment_base_address(text_sel,
        linB8000 + bytespercell * maxx * 1);
     set_segment_limit(text_sel, screensize - 1 - bytespercell *
        maxx * 3);
     selinfo(text_sel);

     status('and clearing entire memory selected by ''text_sel''' +
        ' descriptor');
     seg_fillword(text_sel, 0, (get_segment_limit(text_sel)+1) div 2,
        makechar(' ', LightBlue shl 4));

     status('Notice that only the memory described by the' +
        ' descriptor changed, nothing else');

     status('Now reducing it''s limit and base and setting it''s ' +
        'described memory');
     set_segment_base_address(text_sel,
        get_segment_base_address(text_sel) + bytespercell * maxx);
     set_segment_limit(text_sel,
        get_segment_limit(text_sel) - bytespercell * maxx * 2);
     selinfo(text_sel);
     status('Notice that the base addr increased by one line but ' +
        'the limit decreased by 2 lines');
     status('This should give you the hint that the limit is ' +
        'relative to the base');
     seg_fillword(text_sel, 0, (get_segment_limit(text_sel)+1) div 2,
        makechar(#176, LightMagenta or Brown shl 4));

     status('Now let''s get crazy and copy 10 lines of data from ' +
        'the previously saved screen');
     seg_move(get_ds, longint(@text_save), text_sel,
        maxx * bytespercell * 2, maxx * bytespercell * 10);

     status('At last freeing the descriptor and restoring the old '+
        ' screen contents..');
     status('I hope this little program may give you some hints on '+
        'working with descriptors');
     free_ldt_descriptor(text_sel);
     seg_move(get_ds, longint(@text_save), dosmemselector,
        linB8000, screensize);
     gotoxy(text_oldx, text_oldy);
end.