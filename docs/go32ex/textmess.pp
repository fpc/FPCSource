uses
        crt,
        go32;

const
        columns = 80;
        rows = 25;
        screensize = rows*columns*2;

        text = '! Hello world !';

var
        textofs : Longint;
        save_screen : array[0..screensize-1] of byte;
    curx, cury : Integer;

begin
        randomize;
        dosmemget($B800, 0, save_screen, screensize);
        curx := wherex; cury := wherey;
        gotoxy(1, 1); Write(text);
        textofs := screensize + length(text)*2;
        dosmemmove($B800, 0, $B800, textofs, length(text)*2);
        dosmemfillchar($B800, 0, screensize, #0);
        while (not keypressed) do begin
                dosmemfillchar($B800, textofs + random(length(text))*2 + 1,
                        1, char(random(255)));
                dosmemmove($B800, textofs, $B800,
                        random(columns)*2+random(rows)*columns*2,
                        length(text)*2);
                delay(1);
        end;
        readkey;
        readkey;
        dosmemput($B800, 0, save_screen, screensize);
        gotoxy(curx, cury);
end.