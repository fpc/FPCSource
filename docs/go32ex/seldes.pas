{
This example demonstrates the usage of descriptors and the effects of
changing its limit and base address.

In more detail, the program fills the region described by an
allocated descriptor in text screen memory with various characters.
Before doing this it saves the entire screen contents to the heap and
restores it afterwards.

Some additional background:

The text screen of a VGA card has it's address space at $B800:0;
screen memory is organized in a linear fashion, e.g. the second line
comes directly after the first, where each cell occupies 2 bytes of
memory (1 byte character data, 1 byte attributes). It is 32 kb in
size.

Hence the offset of a single memory cell from its origin is:

        Y * columns * 2 + X * 2

where X and Y mark the point and columns is the number of character
cells per line
}
{$mode delphi}
uses
        crt,
        go32;

const
        { screen x and y dimensions }
        maxx = 80;
        maxy = 25;
        { bytes used for every character cell }
        bytespercell = 2;
        { screen size in bytes }
        screensize = maxx * maxy * bytespercell;

        { the linear address of $B800:0 }
        linB8000 = $B800 * 16;

type
        string80 = string[80];

var
        { holds the old screen contents }
        text_save : array[0..screensize-1] of byte;
        { old cursor x and y coordinates }
        text_oldx, text_oldy : Word;

        { selector to the text mode screen }
        text_sel : Word;

{ prints a status message on the first line of the screen and then
waits for a keypress }
procedure status(s : string80);
begin
     gotoxy(1, 1); clreol; write(s); readkey;
end;

{ writes some descriptor info on the last 2 lines }
procedure selinfo(sel : Word);
begin
     gotoxy(1, 24);
     clreol; writeln('Descriptor base address : $',
        hexstr(get_segment_base_address(sel), 8));
     clreol; write('Descriptor limit : ', get_segment_limit(sel));
end;

{ returns a 2 byte character cell, which includes character data
and its color attributes }
function makechar(ch : char; color : byte) : Word;
begin
     result := byte(ch) or (color shl 8);
end;

begin
     { save original screen contents to variable, this time by using
     seg_move() and the dosmemselector variable }
     seg_move(dosmemselector, linB8000, get_ds, longint(@text_save),
        screensize);
     { additionally we have to save the old screen cursor
     coordinates }
     text_oldx := wherex; text_oldy := wherey;
     { clear the whole screen }
     seg_fillword(dosmemselector, linB8000, screensize div 2,
        makechar(' ', Black or (Black shl 4)));
     { output message }
     status('Creating selector ''text_sel'' to a part of ' +
        'text screen memory');
     { allocate descriptor }
     text_sel := allocate_ldt_descriptors(1);
     { set its base address to the linear address of the text screen
     + the byte size of one line (=maxx * bytespercell * 1) }
     set_segment_base_address(text_sel,
        linB8000 + bytespercell * maxx * 1);
     { the limit is set to the screensize reduced by one (a must be)
     and the number of lines we don't want to have touched (first
     line + lower 2 lines) }
     set_segment_limit(text_sel, screensize - 1 - bytespercell *
        maxx * 3);
     { write descriptor info  }
     selinfo(text_sel);

     status('and clearing entire memory selected by ''text_sel''' +
        ' descriptor');
     { fill the entire selected memory with single characters }
     seg_fillword(text_sel, 0, (get_segment_limit(text_sel)+1) div 2,
        makechar(' ', LightBlue shl 4));

     status('Notice that only the memory described by the ' +
        'descriptor changed, nothing else');

     status('Now reducing it''s limit and base and setting it''s ' +
        'described memory');
     { set the base address of the descriptor (increase it by the
     byte size of one line) }
     set_segment_base_address(text_sel,
        get_segment_base_address(text_sel) + bytespercell * maxx);
     { decrease the limit by byte size of 2 lines (1 line because
        base address changed, one line on the lower end) }
     set_segment_limit(text_sel,
        get_segment_limit(text_sel) - bytespercell * maxx * 2);
     { write descriptor info  }
     selinfo(text_sel);
     status('Notice that the base addr increased by one line but ' +
        'the limit decreased by 2 lines');
     status('This should give you the hint that the limit is ' +
        'relative to the base');
     { fill the descriptor area }
     seg_fillword(text_sel, 0, (get_segment_limit(text_sel)+1) div 2,
        makechar(#176, LightMagenta or Brown shl 4));

     status('Now let''s get crazy and copy 10 lines of data from ' +
        'the previously saved screen');
     { copy memory from the data segment to screen }
     seg_move(get_ds, longint(@text_save), text_sel,
        maxx * bytespercell * 2, maxx * bytespercell * 10);

     status('At last freeing the descriptor and restoring the old ' +
        ' screen contents..');
     status('I hope this little program may give you some hints ' +
        'on working with descriptors');
     { free the descriptor so that it can be used for things }
     free_ldt_descriptor(text_sel);
     { restore old state  }
     seg_move(get_ds, longint(@text_save), dosmemselector,
        linB8000, screensize);
     gotoxy(text_oldx, text_oldy);
end.