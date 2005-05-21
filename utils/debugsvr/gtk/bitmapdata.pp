{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Bitmaps for toolbar buttons.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}
Unit bitmapdata;

Interface

  uses fpgtk,SysUtils;

Const
  BitmapFileNames : Array[1..10] of AnsiString = (
  'clear',
  'pause',
  'run',
  'close',
  'save',
  'connect',
  'disconnect',
  'info',
  'warning',
  'error'
  );

{ Auto bitmap data }
  BitmapDataclear : Array[0..30-6] of AnsiString = ( // clear.xpm
    '16 16 8 1',
    '  c black',
    '. c #830000',
    'X c red',
    'o c #838100',
    'O c #ff8100',
    '+ c #ffd200',
    '@ c yellow',
    '# c white',
    { pixels }
    '############## @',
    '############# @@',
    '### ######## @@@',
    '########### @@@O',
    '#### ##### @ @+o',
    '######### @ @ o ',
    '##  # ## X @ o #',
    '#  # ## XXX o ##',
    '#  ### XXXX. ###',
    '#### # XXX. ####',
    '##  # # X. #####',
    '#  ##  #  ######',
    '  ###  #########',
    '  ##   ## ######',
    '        ### ####',
    '#    #   #######'
  );

  BitmapDatapause : Array[0..33-6] of AnsiString = ( // pause.xpm
    '16 16 11 1',
    '  c black',
    '. c #7b7d7b',
    'X c #008100',
    'o c #00ba00',
    'O c green',
    '+ c #ee7dee',
    '@ c #bdbebd',
    '# c #eeb6ee',
    '$ c #ffbeff',
    '% c #fff6ff',
    '& c white',
    { pixels }
    '&&&&&     &&&&&&',
    '&&&  &&&@%  &&&&',
    '&& &%.   .%$ &&&',
    '& &% &O&O& %@ &&',
    '& % &     & % &&',
    ' &.&  &&O  &.% &',
    ' & O &OOoO O @ &',
    ' & & OOOOX & . &',
    ' $ O OoOoX O . &',
    ' %.&  OXX  &.+ &',
    '& % &     & # &&',
    '& @% &O&O& +. &&',
    '&& @%.   .#. &&&',
    '&&&  %@..+  &&&&',
    '&&&&&     &&&&&&',
    '&&&&&&&&&&&&&&&&'
  );

  BitmapDatarun : Array[0..32-6] of AnsiString = ( // run.xpm
    '16 16 10 1',
    '  c black',
    '. c #7b7d7b',
    'X c #008100',
    'o c #00ba00',
    'O c #ee7dee',
    '+ c #bdbebd',
    '@ c #eeb6ee',
    '# c #ffbeff',
    '$ c #fff6ff',
    '% c white',
    { pixels }
    '%%%%%     %%%%%%',
    '%%%  %%%+$  %%%%',
    '%% %$.   .$# %%%',
    '% %$       $+ %%',
    '% $         $ %%',
    ' %.   +$X   .$ %',
    ' %   +oXoX   + %',
    ' %   XXXXX   . %',
    ' #   XoXoX   . %',
    ' $.   XXX   .O %',
    '% $         @ %%',
    '% +$       O. %%',
    '%% +$.   .@. %%%',
    '%%%  $+..O  %%%%',
    '%%%%%     %%%%%%',
    '%%%%%%%%%%%%%%%%'
  );

  BitmapDataclose : Array[0..29-6] of AnsiString = ( // close.xpm
    '16 16 7 1',
    '  c black',
    '. c #7b7d7b',
    'X c #8b7d8b',
    'o c #8b858b',
    'O c #bdbebd',
    '+ c #cdc6cd',
    '@ c white',
    { pixels }
    '@@@@@     @@@@@@',
    '@@@@@ ... @@@@@@',
    '@@           @@@',
    '@@ @O+OX...X @@@',
    '@@           @@@',
    '@@@ . . . . @@@@',
    '@@@ @ + o o @@@@',
    '@@  @ O O .  @@@',
    '@ @ @ + o o @ @@',
    '@@@ @ O O . @@@@',
    '@@@ @ + o o @@@@',
    '@@@ @ O O . @@@@',
    '@@@ @ + o o @@@@',
    '@@@ @ O O . @@@@',
    '@@@ @.o.o.o @@@@',
    '@@@         @@@@'
  );

  BitmapDatasave : Array[0..30-6] of AnsiString = ( // save.xpm
    '16 16 8 1',
    '  c black',
    '. c #7b7d7b',
    'X c blue',
    'o c red',
    'O c #bdbebd',
    '+ c #ffbeff',
    '@ c #ffceff',
    '# c white',
    { pixels }
    '      ##########',
    ' ####  #ooooo###',
    ' # +# # ##@ooo##',
    ' #### ## ##ooo##',
    ' #  #     #ooo##',
    ' ########ooooooo',
    ' #    # # ooooo#',
    ' ######## #ooo##',
    ' #  #  O XXXoXX ',
    ' ####### ###### ',
    ' ####### ###### ',
    '                ',
    '########        ',
    '########  O#O#  ',
    '########  # #O  ',
    '########  . ..  '
  );

  BitmapDataconnect : Array[0..28-6] of AnsiString = ( // connect.xpm
    '16 16 6 1',
    '  c black',
    '. c #7b7d7b',
    'X c none',
    'o c yellow',
    'O c #bdbebd',
    '+ c white',
    { pixels }
    'XXXXXXXoXXXXXXXX',
    'XXXXXXXoXXXXXXXX',
    'XXoXXoooooXXoXXX',
    'XXXoooooooooXXXX',
    'XXXooo   oooXXXX',
    'XXooo +o+ oooXXX',
    'XXoo +o+o+ ooXXX',
    'oooo o+.+o ooooX',
    'XXoo +o.o+ ooXXX',
    'XXooo +.+ oooXXX',
    'XXXooo + oooXXXX',
    'XXXooo o oooXXXX',
    'XXoXXo   oXXoXXX',
    'XXXXXX O XXXXXXX',
    'XXXXXX   XXXXXXX',
    'XXXXXXX XXXXXXXX'
  );

  BitmapDatadisconnect : Array[0..27-6] of AnsiString = ( // disconnect.xpm
    '16 16 5 1',
    '  c black',
    '. c #7b7d7b',
    'X c none',
    'o c #bdbebd',
    'O c white',
    { pixels }
    'XXXXXXXXXXXXXXXX',
    'XXXXXXXXXXXXXXXX',
    'XXXXXXXXXXXXXXXX',
    'XXXXXXXXXXXXXXXX',
    'XXXXXX   XXXXXXX',
    'XXXXX OOO XXXXXX',
    'XXXX OOOOO XXXXX',
    'XXXX OO.OO XXXXX',
    'XXXX OO.OO XXXXX',
    'XXXXX O.O XXXXXX',
    'XXXXXX O XXXXXXX',
    'XXXXXX O XXXXXXX',
    'XXXXXX   XXXXXXX',
    'XXXXXX o XXXXXXX',
    'XXXXXX   XXXXXXX',
    'XXXXXXX XXXXXXXX'
  );

  BitmapDatainfo : Array[0..31-6] of AnsiString = ( // info.xpm
    '16 16 9 1',
    '  c black',
    '. c #000083',
    'X c #080083',
    'o c #0800c5',
    'O c blue',
    '+ c #0800ff',
    '@ c #cdc2ff',
    '# c #ffc2ff',
    '$ c white',
    { pixels }
    '$$$$$$$$$$$$$$$$',
    '$$$$$+.X.+.$$$$$',
    '$$$.+@$#$@$  $$$',
    '$$.$@$@OO$@$o $$',
    '$$+@$#$.O#$@$ +$',
    '$+@$@$@$@$@$@+ $',
    '$.$#$@+OO@$#$@ $',
    '$X@$@$@+O$@$@$ $',
    '$.+@$#$.O#$@$O $',
    '$$.$@$OOOO@$o .$',
    '$$$ +@$#$@$O .+$',
    '$$$$  o$@+  .X$$',
    '$$$$$$  $ +.$$$$',
    '$$$$$$$ @ .$$$$$',
    '$$$$$$$$  +$$$$$',
    '$$$$$$$$$$$$$$$$'
  );

  BitmapDatawarning : Array[0..30-6] of AnsiString = ( // warning.xpm
    '16 16 8 1',
    '  c black',
    '. c #080000',
    'X c #000083',
    'o c #0800ff',
    'O c #cdc200',
    '+ c #ffc200',
    '@ c yellow',
    '# c white',
    { pixels }
    '################',
    '#######.  ######',
    '######.+@ o#####',
    '#####.O@O@ #####',
    '##### @ @+ X####',
    '#### @   @O ####',
    '####.O   O@ o###',
    '###.O@   @O@ ###',
    '##.O@+. .+@O X##',
    '## @O@O O@O@O ##',
    '# @+@O@+@O@+@ o#',
    '#.O@O@X X@O@O@ #',
    '# @O@+@O@+@O@+ #',
    '##            X#',
    '###XoXoXoXoXoXo#',
    '################'
  );

  BitmapDataerror : Array[0..33-6] of AnsiString = ( // error.xpm
    '16 16 11 1',
    '  c black',
    '. c #000083',
    'X c #080083',
    'o c #0000c5',
    'O c #0800c5',
    '+ c blue',
    '@ c #0800ff',
    '# c #c50000',
    '$ c red',
    '% c #cdc2ff',
    '& c white',
    { pixels }
    '&&&&&&&&&&&&&&&&',
    '&&&&&@    &&&&&&',
    '&&&. #$$$# .&&&&',
    '&&.$#$#$#$#$.&&&',
    '&& #@+$#$+@# .&&',
    '&@#$O&O$O&O$# &&',
    '& $$$o&+&o$$$ @&',
    '& #$#$O&%$#$# .&',
    '& $#$+&%&+$#$ @&',
    '& #$O&O$O&O$# .&',
    '&& $@o$$$o&$ .@&',
    '&&.$#$#$#$#$ X&&',
    '&&&. $$#$$  @+&&',
    '&&&&.     .XO&&&',
    '&&&&&&@.@.@&&&&&',
    '&&&&&&&&&&&&&&&&'
  );

Function PixmapFromFile (FN : String) :TFPGtkPixmap;

Implementation

Function PixmapFromFile (FN : String) :TFPGtkPixmap;

Var
  I : integer;

begin
  Result:=TFPGtkPixmap.CReate;
  If FileExists ('bitmaps'+directoryseparator+FN+'.xpm') then
    Result.LoadFromFile('bitmaps'+directoryseparator+FN+'.xpm')
  else
    begin
    I:=10;
    While (I>0) and (FN<>BitmapFileNames[i]) do
      Dec(i);
    With Result do
      Case I of
        1  : LoadFromArray(BitmapDataclear);
        2  : LoadFromArray(BitmapDatapause);
        3  : LoadFromArray(BitmapDatarun);
        4  : LoadFromArray(BitmapDataclose);
        5  : LoadFromArray(BitmapDatasave);
        6  : LoadFromArray(BitmapDataconnect);
        7  : LoadFromArray(BitmapDatadisconnect);
        8  : LoadFromArray(BitmapDatainfo);
        9  : LoadFromArray(BitmapDatawarning);
        10 : LoadFromArray(BitmapDataerror);
      end;
    end;
end;

end.
