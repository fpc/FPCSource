{************************************************}
{                                                }
{ CRT Unit Demo                                  }
{ Copyright (c) 1985,90 by Borland International }
{                                                }
{************************************************}

program CrtDemo;
{ Example program that uses the Crt unit. Uses the following routines
  from the Crt unit:

    ClrScr
    DelLine
    GoToXY
    InsLine
    KeyPressed
    ReadKey
    TextBackground
    TextColor
    TextMode
    WhereX
    WhereY
    Window
    Write
    WriteLn;

  Also uses LastMode and WindMax variables from Crt unit.

    1. Init routine:
       - Save original video mode. On an EGA or VGA, use the 8x8 font
         (43 lines on an EGA, 50 on VGA).
       - Setup LastRow to preserve last line on screen for messages
         (preserves last 2 lines in 40-column mode). Setup LastCol.
       - Initialize the random number generator.
    2. MakeWindow routine:
       - Puts up random-sized, random-colored windows on screen.
    3. Program body:
       - Call Init
       - Loop until Contrl-C is typed:
         - Echo keystrokes (Turbo Pascal windows automatically wrap
           and scroll).
         - Support special keys:
             <Ins>    inserts a line at the cursor
             <Del>    deletes a line at the cursor
             <Up>,
             <Dn>,
             <Right>,
             <Left>   position the cursor in the window
             <Alt-R>  generate random text until a key is pressed
             <Alt-W>  creates another random window
             <ESC>    exits the program
}

uses Crt;

var
  OrigMode,LastCol,LastRow: Word;
  Ch: Char;
  Done: Boolean;

procedure Initialize;
{ Initialize the video mode, LastCol, LastRow, and the random number }
{ generator. Paint the help line. }
begin
  OrigMode:=LastMode;                  { Remember original video mode }
  TextMode(_80cols+_50rows);           { use 43 or 50 lines on EGA/VGA }
  LastCol:=Lo(WindMax)+1;              { get last column, row }
  LastRow:=Hi(WindMax)+1;
  GoToXY(1,LastRow);                   { put message line on screen }
  TextBackground(Black);
  TextColor(White);
  Write(' Ins-InsLine  '+
        'Del-DelLine  '+
        #27#24#25#26'-Cursor  '+
        'Alt-W-Window  '+
        'Alt-R-Random  '+
        'Esc-Exit');
  LastRow:=lastrow-80 div LastCol;     { don't write on message line }
  Randomize;                           { init random number generator }
end; { Init }

procedure MakeWindow;
{ Make a random window, with random background and foreground colors }
var
  X,Y,Width,Height: Word;
begin
  Width:=Random(LastCol-2)+2;               { random window size }
  Height:=Random(LastRow-2)+2;
  X:=Random(LastCol-Width)+1;           { random position on screen }
  Y:=Random(LastRow-Height)+1;
  Window(X,Y,X+Width,Y+Height);
  TextBackground(Random(8));
  TextColor(Random(7)+9);
  ClrScr;
end; { MakeWindow }

procedure RandomText;
{ Generate random text until a key is pressed. Filter out }
{ control characters. }
begin
  repeat
    Write(Chr(Random(256-32)+32));
  until KeyPressed;
end; { RandomText }

begin { program body }
  Initialize;
  MakeWindow;
  Done:=False;
  repeat
    Ch:=ReadKey;
    case Ch of
      #0:                               { Function keys }
      begin
        Ch:=ReadKey;
        case Ch of
          #17: MakeWindow;              { Alt-W }
          #19: RandomText;              { Alt-R }
          #45: Done:=True;              { Alt-X }
          #72: GotoXY(WhereX,WhereY-1); { Up }
          #75: GotoXY(WhereX-1,WhereY); { Left }
          #77: GotoXY(WhereX+1,WhereY); { Right }
          #80: GotoXY(WhereX,WhereY+1); { Down }
          #82: InsLine;                 { Ins }
          #83: DelLine;                 { Del }
        end;
      end;
      #3: Done:=True;                   { Ctrl-C }
      #13: WriteLn;                     { Enter }
      #27: Done:=True;                  { Esc }
    else
      Write(Ch);
    end;
  until Done;
  TextMode(OrigMode);
end.
