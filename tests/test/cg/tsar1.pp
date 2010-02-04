program tsar1;
{$mode objfpc}
{$o-}
var
  c0,c4,c7,c15,c31,c63,c36,c20,c68,c12 : integer;
  c3f : shortint;
  c3fff : smallint;
  c3fffffff : longint;
  c3fffffffffffffff : int64;

begin
 c0:=0;
 c4:=4;
 c7:=7;
 c15:=15;
 c31:=31;
 c63:=63;
 c36:=36;
 c20:=20;
 c68:=68;
 c12:=12;
 c3f:=$3f;
 c3fff:=$3fff;
 c3fffffff:=$3fffffff;
 c3fffffffffffffff:=$3fffffffffffffff;

 writeln('Testing constant SarInt64...');
 if SarInt64(-$3FFFFFFFFFFFFFFF,4)<>-$400000000000000 then begin
  writeln('Fail!');
 end else begin
  writeln('Pass!');
 end;
 if SarInt64($3FFFFFFFFFFFFFFF,4)<>$3FFFFFFFFFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64(-$3FFFFFFFFFFFFFF0,4)<>-$3FFFFFFFFFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64($3FFFFFFFFFFFFFF0,4)<>$3FFFFFFFFFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64(-$3FFFFFFFFFFFFFFF,0)<>-$3FFFFFFFFFFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64($3FFFFFFFFFFFFFFF,0)<>$3FFFFFFFFFFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64(-$3FFFFFFFFFFFFFFF,63)<>-1 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64($3FFFFFFFFFFFFFFF,63)<>0 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64(-$3FFFFFFFFFFFFFFF)<>-$2000000000000000 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64($3FFFFFFFFFFFFFFF)<>$1FFFFFFFFFFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 writeln;

 writeln('Testing constant SarLongint...');
 if SarLongint(-$3FFFFFFF,4)<>-$4000000 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint($3FFFFFFF,4)<>$3FFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint(-$3FFFFFF0,4)<>-$3FFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint($3FFFFFF0,4)<>$3FFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint(-$3FFFFFFF,0)<>-$3FFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint($3FFFFFFF,0)<>$3FFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint(-$3FFFFFFF,31)<>-1 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint($3FFFFFFF,31)<>0 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint(-$3FFFFFFF)<>-$20000000 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint($3FFFFFFF)<>$1FFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 writeln;

 writeln('Testing constant SarSmallint...');
 if SarSmallint(-$3FFF,4)<>-$400 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint($3FFF,4)<>$3FF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint(-$3FF0,4)<>-$3FF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint($3FF0,4)<>$3FF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint(-$3FFF,0)<>-$3FFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint($3FFF,0)<>$3FFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint(-$3FFF,15)<>-1 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint($3FFF,15)<>0 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint(-$3FFF)<>-$2000 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint($3FFF)<>$1FFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 writeln;

 writeln('Testing constant SarShortint...');
 if SarShortint(-$3F,4)<>-$4 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint($3F,4)<>$3 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint(-$30,4)<>-$3 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint($30,4)<>$3 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint(-$3F,0)<>-$3F then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint($3F,0)<>$3F then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint(-$3F,7)<>-1 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint($3F,7)<>0 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint(-$3F)<>-$20 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint($3F)<>$1F then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 writeln;

 writeln('Testing constant shifting overflows');
 if SarInt64($3fffffffffffffff,68)<>$3ffffffffffffff then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint($3fffffff,36)<>$3ffffff then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint($3fff,20)<>$3ff then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint($3f,12)<>$3 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 writeln;

 writeln('Testing SarInt64...');
 if SarInt64(-$3FFFFFFFFFFFFFFF,c4)<>-$400000000000000 then begin
  writeln('Fail!');
 end else begin
  writeln('Pass!');
 end;
 if SarInt64($3FFFFFFFFFFFFFFF,c4)<>$3FFFFFFFFFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64(-$3FFFFFFFFFFFFFF0,c4)<>-$3FFFFFFFFFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64($3FFFFFFFFFFFFFF0,c4)<>$3FFFFFFFFFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64(-$3FFFFFFFFFFFFFFF,c0)<>-$3FFFFFFFFFFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64($3FFFFFFFFFFFFFFF,c0)<>$3FFFFFFFFFFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64(-$3FFFFFFFFFFFFFFF,c63)<>-1 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64($3FFFFFFFFFFFFFFF,c63)<>0 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64(-c3FFFFFFFFFFFFFFF,63)<>-1 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64(c3FFFFFFFFFFFFFFF,63)<>0 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64(-$3FFFFFFFFFFFFFFF)<>-$2000000000000000 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarInt64($3FFFFFFFFFFFFFFF)<>$1FFFFFFFFFFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 writeln;

 writeln('Testing SarLongint...');
 if SarLongint(-$3FFFFFFF,c4)<>-$4000000 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint($3FFFFFFF,c4)<>$3FFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint(-$3FFFFFF0,c4)<>-$3FFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint($3FFFFFF0,c4)<>$3FFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint(-$3FFFFFFF,c0)<>-$3FFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint($3FFFFFFF,c0)<>$3FFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint(-$3FFFFFFF,c31)<>-1 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint($3FFFFFFF,c31)<>0 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint(-c3FFFFFFF,31)<>-1 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint(c3FFFFFFF,31)<>0 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint(-$3FFFFFFF)<>-$20000000 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint($3FFFFFFF)<>$1FFFFFFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 writeln;

 writeln('Testing SarSmallint...');
 if SarSmallint(-$3FFF,c4)<>-$400 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint($3FFF,c4)<>$3FF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint(-$3FF0,c4)<>-$3FF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint($3FF0,c4)<>$3FF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint(-$3FFF,c0)<>-$3FFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint($3FFF,c0)<>$3FFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint(-$3FFF,c15)<>-1 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint($3FFF,c15)<>0 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint(-c3FFF,15)<>-1 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint(c3FFF,15)<>0 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint(-$3FFF)<>-$2000 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint($3FFF)<>$1FFF then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 writeln;

 writeln('Testing SarShortint...');
 if SarShortint(-$3F,c4)<>-$4 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint($3F,c4)<>$3 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint(-$30,c4)<>-$3 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint($30,c4)<>$3 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint(-$3F,c0)<>-$3F then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint($3F,c0)<>$3F then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint(-$3F,c7)<>-1 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint(c3F,7)<>0 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint(-c3F,7)<>-1 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint($3F,c7)<>0 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint(-$3F)<>-$20 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint($3F)<>$1F then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 writeln;


 { the overflow behaviour is different for different CPUs
 writeln('Testing shifting overflows');
 if SarInt64($3fffffffffffffff,c68)<>$3ffffffffffffff then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarLongint($3fffffff,c36)<>$3ffffff then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarSmallint($3fff,c20)<>$3ff then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 if SarShortint($3f,c12)<>$3 then begin
  halt(1);
 end else begin
  writeln('Pass!');
 end;
 }

 writeln('All passed');
end.

