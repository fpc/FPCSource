{ This example copies around some blocks of memory in DOS memory
space.

In more detail, the program copies a string randomly to the text
mode screen. Aditionally it messes around a bit with the color
attributes of the string.
Before doing this it saves the entire screen contents to the heap
and restores it afterwards.

Some additional background:

The text screen of a VGA card has it's address space at $B800:0;
screen memory is organized in a linear fashion, e.g. the second line
comes directly after the first, where each cell occupies 2 bytes of
memory (1 byte character data, 1 byte attributes). It is 32 kb in
size.

Hence the offset of a single memory cell from its origin is:

Y*columns*2 + X*2

where X and Y mark the point and columns is the number of character
cells per line
}

uses
        crt,
        go32;

const
        { number of columns on screen }
        columns = 80;
        { number of rows on screen }
        rows = 25;
        screensize = rows*columns*2;

        { sample text string }
        text = '! Hello world !';

var
        textofs : Longint;
        { this variable holds the entire screen contents }
        save_screen : array[0..screensize-1] of byte;
        { These two hold the previous cursor coordinates }
    curx, cury : Integer;

begin
        randomize;
        { save screen contents to save_screen variable }
        dosmemget($B800, 0, save_screen, screensize);
        { save current cursor coordinates }
        curx := wherex; cury := wherey;
        { This is our demo text }
        gotoxy(1, 1); Write(text);
        { calculate the address in offscreen memory (to be sure it will
        not be overwritten by the copy process later, we don't put it
        exactly at the end of the visible screen area) }
        textofs := screensize + length(text)*2;
        { copy it to offscreen memory }
        dosmemmove($B800, 0, $B800, textofs, length(text)*2);
        { clear the screen by writing zeros on the whole visible screen}
        dosmemfillchar($B800, 0, screensize, #0);
        while (not keypressed) do begin
                { set the attribute field (byte 2 of every cell) of the
                text in offscreen memory to random values }
                dosmemfillchar($B800, textofs + random(length(text))*2 + 1,
                        1, char(random(255)));
                { copy the string from offscreen to visibly screen by calculating
                it's destination address randomly }
                dosmemmove($B800, textofs, $B800,
                        random(columns)*2+random(rows)*columns*2,
                        length(text)*2);
                { small delay, else it is too fast }
                delay(1);
        end;
        { clear the keyboard buffer }
        readkey;
        { wait for a keypress }
        readkey;
        { restore old screen contents afterwards }
        dosmemput($B800, 0, save_screen, screensize);
        gotoxy(curx, cury);
end.