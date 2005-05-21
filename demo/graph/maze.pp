{A demo with some interesting algoritms, and for Graph.

The sources for this game was found on a site that claims to only have
PD stuff with the below header(which was only reindented), and the webmaster
said that everything he published was sent to him with that purpose. We tried
to contact the authors mentioned below via mail over internet, but that
failed. If there is somebody that claims authorship of these programs,
please mail marco@freepascal.org, and the sources will be removed from our
websites.

------------------------------------------------------------------------

ORIGINAL Header:

created by Randy Ding July 16,1983   <April 21,1992>

Very small FPC fixes by Marco van de Voort (EgaHi to vgahi), and tried
setting the maze dimensions maxx and maxy to a bigger size.
Won't work, you'll have to update all vars to al least word to increase the
complexity of the grid further. I didn't do it, since 200x200 is already
unreadable to me.

Don't forget the BGIPATH of InitGraph.
}

{$R-}   { range checking }

program makemaze;

{apptype GUI, disabled, there are too many writes PM }

uses
 {$ifdef Win32}
  WinCrt,Windows,
 {$else}
  crt,
 {$endif}
  graph;

const
  screenwidth   = 640;
  screenheight  = 480;
  minblockwidth = 2;
  maxx = 200;   { BP: [3 * maxx * maxy] must be less than 65520 (memory segment) }
                { FPC: Normally no problem. ( even if you'd use 1600x1200x3< 6MB)}
  maxy = 200;   { here maxx/maxy about equil to screenwidth/screenheight }
  flistsize = maxx*maxy DIV 2; { flist size (fnum max, about 1/3 of maxx * maxy) }

  background = black;
  gridcolor  = green;
  solvecolor = white;

  rightdir = $01;
  updir    = $02;
  leftdir  = $04;
  downdir  = $08;

  unused   = $00;    { cell types used as flag bits }
  frontier = $10;
{  reserved = $20; }
  tree     = $30;


type
  frec = record
          column, row : byte;
         end;
  farr = array [1..flistsize] of frec;

  cellrec = record
              point : word;  { pointer to flist record }
              flags : byte;
            end;
  cellarr = array [1..maxx,1..maxy] of cellrec;

  {
    one byte per cell, flag bits...

    0: right, 1 = barrier removed
    1: top    "
    2: left   "
    3: bottom "
    5,4: 0,0 = unused cell type
         0,1 = frontier "
         1,1 = tree     "
         1,0 = reserved "
    6: (not used)
    7: solve path, 1 = this cell part of solve path
  }


var
  flist     : farr;         { list of frontier cells in random order }
  cell      : ^cellarr;      { pointers and flags, on heap }
  fnum,
  width,
  height,
  blockwidth,
  halfblock,
  maxrun    : word;
  runset    : byte;
  ch        : char;

procedure initbgi;
var
  grdriver,
  grmode,
  errcode : integer;
begin
  grdriver := vga;
  grmode   := vgahi;
  initgraph(grdriver, grmode, 'd:\pp\bp\bgi');
  errcode:= graphresult;
  if errcode <> grok then
  begin
    CloseGraph;
    writeln('Graphics error: ', grapherrormsg(errcode));
    halt(1);
  end;
end;


function adjust(var x, y : word; d : byte) : boolean;
begin                              { take x,y to next cell in direction d }
  case d of                        { returns false if new x,y is off grid }
    rightdir:
    begin
      inc (x);
      adjust:= x <= width;
    end;

    updir:
    begin
      dec (y);
      adjust:= y > 0;
    end;

    leftdir:
    begin
      dec (x);
      adjust:= x > 0;
    end;

    downdir:
    begin
      inc (y);
      adjust:= y <= height;
    end;
  end;
end;


procedure remove(x, y : word);      { remove a frontier cell from flist }
var
  i : word; { done by moving last entry in flist into it's place }
begin
  i := cell^[x,y].point;          { old pointer }
  with flist[fnum] do
    cell^[column,row].point := i;   { move pointer }
  flist[i] := flist[fnum];        { move data }
  dec(fnum);                    { one less to worry about }
end;


procedure add(x, y : word; d : byte);  { add a frontier cell to flist }
var
  i : byte;
begin
  i := cell^[x,y].flags;
  case i and $30 of   { check cell type }
    unused :
    begin
      cell^[x,y].flags := i or frontier;  { change to frontier cell }
      inc(fnum);                        { have one more to worry about }
      if fnum > flistsize then
      begin     { flist overflow error! }
        dispose(cell);  { clean up memory }
        closegraph;
        writeln('flist overflow! - To correct, increase "flistsize"');
        write('hit return to halt program ');
        readln;
        halt(1);        { exit program }
      end;
      with flist[fnum] do
      begin    { copy data into last entry of flist }
        column := x;
        row    := y;
      end;
      cell^[x,y].point := fnum; { make the pointer point to the new cell }
      runset := runset or d;   { indicate that a cell in direction d was }
    end;                      {    added to the flist }

    frontier : runset := runset or d;     { allready in flist }
  end;
end;


procedure addfront(x, y : word);    { change all unused cells around this }
var                              {    base cell to frontier cells }
  j, k : word;
  d    : byte;
begin
  remove(x, y);       { first remove base cell from flist, it is now }
  runset := 0;         {    part of the tree }
  cell^[x,y].flags := cell^[x,y].flags or tree;   { change to tree cell }
  d := $01;            { look in all four directions- $01,$02,$04,$08 }
  while d <= $08 do
  begin
    j := x;
    k := y;
    if adjust(j, k, d) then
      add(j, k, d);  { add only if still in bounds }
    d := d shl 1;    { try next direction }
  end;
end;


procedure remline(x, y : word; d : byte);  { erase line connecting two blocks }
begin
  setcolor(background);
  x := (x - 1) * blockwidth;
  y := (y - 1) * blockwidth;
  case d of
    rightdir : line (x + blockwidth, y + 1, x + blockwidth, y + blockwidth - 1);
    updir    : line (x + 1, y, x + blockwidth - 1, y);
    leftdir  : line (x, y + 1, x, y + blockwidth - 1);
    downdir  : line (x + 1, y + blockwidth, x + blockwidth - 1, y + blockwidth);
  end;
end;


{ erase line and update flags to indicate the barrier has been removed }
procedure rembar(x, y : word; d : byte);
var
  d2 : byte;
begin
  remline(x, y, d);       { erase line }
  cell^[x,y].flags := cell^[x,y].flags or d; { show barrier removed dir. d }
  d2 := d shl 2;  { shift left twice to reverse direction }
  if d2 > $08 then
    d2 := d2 shr 4;  { wrap around }
  if adjust(x, y, d) then  { do again from adjacent cell back to base cell }
    cell^[x,y].flags := cell^[x,y].flags or d2;    { skip if out of bounds }
end;


function randomdir : byte;  { get a random direction }
begin
  case random(4) of
    0 : randomdir := rightdir;
    1 : randomdir := updir;
    2 : randomdir := leftdir;
    3 : randomdir := downdir;
  end;
end;


procedure connect(x, y : word);    { connect this new branch to the tree }
var                             {    in a random direction }
  j, k  : word;
  d     : byte;
  found : boolean;
begin
  found := false;
  while not found do
  begin { loop until we find a tree cell to connect to }
    j := x;
    k := y;
    d := randomdir;
    if adjust(j, k, d) then
      found := cell^[j,k].flags and $30 = tree;
  end;
  rembar(x, y, d);   { remove barrier connecting the cells }
end;


procedure branch(x, y : word);  { make a new branch of the tree }
var
  runnum : word;
  d      : byte;
begin
  runnum := maxrun;      { max number of tree cells to add to a branch }
  connect(x, y);        { first connect frontier cell to the tree }
  addfront(x, y);       { convert neighboring unused cells to frontier }
  dec(runnum);         { number of tree cells left to add to this branch }
  while (runnum > 0) and (fnum > 0) and (runset > 0) do
  begin
    repeat
      d := randomdir;
    until d and runset > 0;  { pick random direction to known frontier }
    rembar(x, y, d);          {    and make it part of the tree }
    adjust(x, y, d);
    addfront(x, y);      { then pick up the neighboring frontier cells }
    dec(runnum);
  end;
end;


procedure drawmaze;
var
  x, y, i : word;
begin
  setcolor(gridcolor);    { draw the grid }
  y := height * blockwidth;
  for i := 0 to width do
  begin
    x := i * blockwidth;
    line(x, 0, x, y);
  end;
  x := width * blockwidth;
  for i := 0 to height do
  begin
    y := i * blockwidth;
    line (0, y, x, y);
  end;
  fillchar(cell^, sizeof(cell^), chr(0));    { zero flags }
  fnum   := 0;   { number of frontier cells in flist }
  runset := 0; { directions to known frontier cells from a base cell }
  randomize;
  x := random(width) + 1;   { pick random start cell }
  y := random(height) + 1;
  add(x, y, rightdir);       { direction ignored }
  addfront(x, y);      { start with 1 tree cell and some frontier cells }
  while (fnum > 0) do
  with flist[random(fnum) + 1] do
    branch(column, row);
end;

procedure dot(x, y, colr : word);
begin
  putpixel(blockwidth * x - halfblock, blockwidth * y - halfblock, colr);
end;

procedure solve(x, y, endx, endy : word);
var
  j, k : word;
  d    : byte;
begin
  d := rightdir;  { starting from left side of maze going right }
  while (x <> endx) or (y <> endy) do
  begin
    if d = $01 then
      d := $08
    else
      d := d shr 1; { look right, hug right wall }
    while cell^[x,y].flags and d = 0 do
    begin { look for an opening }
      d := d shl 1;                            { if no opening, turn left }
      if d > $08 then
        d := d shr 4;
    end;
    j := x;
    k := y;
    adjust(x, y, d);         { go in that direction }
    with cell^[j,k] do
    begin    { turn on dot, off if we were here before }
      flags := ((((cell^[x,y].flags xor $80) xor flags) and $80) xor flags);
      if flags and $80 <> 0 then
        dot(j, k, solvecolor)
      else
        dot(j, k, background);
    end;
  end;
  dot(endx, endy, solvecolor);    { dot last cell on }
end;

procedure mansolve (x,y,endx,endy: word);
var
  j, k : word;
  d    : byte;
  ch   : char;
begin
  ch := ' ';
  while ((x <> endx) or (y <> endy)) and (ch <> 'X') and (ch <> #27) do
  begin
    dot(x, y, solvecolor);    { dot man on, show where we are in maze }
    ch := upcase(readkey);
    dot(x, y, background);    { dot man off after keypress }
    d := 0;
    case ch of
      #0:
      begin
        ch := readkey;
        case ch of
          #72 : d := updir;
          #75 : d := leftdir;
          #77 : d := rightdir;
          #80 : d := downdir;
        end;
      end;

      'I' : d := updir;
      'J' : d := leftdir;
      'K' : d := rightdir;
      'M' : d := downdir;
    end;

    if d > 0 then
    begin
      j := x;
      k := y;    { move if no wall and still in bounds }
      if (cell^[x,y].flags and d > 0) and adjust(j, k, d) then
      begin
        x := j;
        y := k;
      end;
    end;
  end;
end;

procedure solvemaze;
var
  x, y,
  endx,
  endy : word;
begin
  x := 1;                         { pick random start on left side wall }
  y := random(height) + 1;
  endx := width;                  { pick random end on right side wall }
  endy := random(height) + 1;
  remline(x, y, leftdir);         { show start and end by erasing line }
  remline(endx, endy, rightdir);
  mansolve(x, y, endx, endy);      { try it manually }
  solve(x, y, endx, endy);         { show how when he gives up }
  while keypressed do
   readkey;
  readkey;
end;


procedure getsize;
var
  j, k : real;
begin
 {$ifndef win32}
  clrscr;
 {$endif}
  writeln('       Mind');
  writeln('       Over');
  writeln('       Maze');
  writeln;
  writeln('   by Randy Ding');
  writeln;
  writeln('Use I,J,K,M or arrow keys to walk thru maze,');
  writeln('then hit X when you give up!');
  repeat
    writeln;
    write('Maze size: ', minblockwidth, ' (hard) .. 95 (easy) ');
    readln(blockwidth);
  until (blockwidth >= minblockwidth) and (blockwidth < 96);
  writeln;
  write('Maximum branch length: 1 easy .. 50 harder, (0 unlimited) ');
  readln(maxrun);
  if maxrun <= 0 then
    maxrun := 65535;  { infinite }
  j := Real(screenwidth) / blockwidth;
  k := Real(screenheight) / blockwidth;
  if j = system.int(j) then
    j := j - 1;
  if k= system.int(k) then
    k := k - 1;
  width  := trunc(j);
  height := trunc(k);
  if (width > maxx) or (height > maxy) then
  begin
    width  := maxx;
    height := maxy;
  end;
  halfblock := blockwidth div 2;
end;

begin
 {$ifdef Win32}
  ShowWindow(GetActiveWindow,0);
  Initbgi;
 {$endif}
  repeat
    getsize;
    {$ifndef Win32}
     initbgi;
    {$endif}
    new(cell);    { allocate this large array on heap }
    drawmaze;
    solvemaze;
    dispose(cell);
    {$ifndef Win32}
     closegraph;
    {$endif}
    while keypressed do
      ch := readkey;
    write ('another one? ');
    ch := upcase (readkey);
  until (ch = 'N') or (ch = #27);
  {$ifdef Win32}
   CloseGraph;
  {$endif}
end.
