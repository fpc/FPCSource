{
   Author: Vitaliy Trifonov
}
program pad_demo;

{$MODE OBJFPC}

{$IFDEF DEBUG}
{$ASSERTIONS ON}
{$OVERFLOWCHECKS ON}
{$RANGECHECKS ON}
{$CHECKPOINTER ON}
{$ENDIF}

uses
  ncurses, panel, sysutils;

type
  TNcCoord = array[0..1] of Smallint;

  TNcStr = packed record
    str: AnsiString;
    attr: attr_t;
    coord: TNcCoord;
  end;

const y = 0; x = 1;

function CTRL( ch: chtype ): chtype; inline;
begin
  CTRL := ch AND $001F
end;

function randomchar: chtype;
var
  ch: Char = #0;
begin
  while not (ch in ['0'..'9','A'..'Z','a'..'z']) do
    ch := Char(Random(123));
  randomchar := chtype(ch);
end;

function randompair: longint;
var
  pair: longint = 0;
begin
  while not (pair in [1..5]) do
    pair := Random(6);
  randompair := pair;
end;


procedure draw;
var
  y, x:  Smallint;
begin
  for y := 0 to LINES - 1 do
    for x := 0 to COLS - 1 do
      mvaddch(y, x, randomchar OR COLOR_PAIR(randompair));
end;

procedure draw_pad(win: PWINDOW);

var
  y, x, my, mx:  Smallint;
begin
  getmaxyx(win,my,mx);
  wborder(win, ACS_CKBOARD,ACS_CKBOARD,ACS_CKBOARD,ACS_CKBOARD,
          ACS_CKBOARD,ACS_CKBOARD,ACS_CKBOARD,ACS_CKBOARD);
  for y := 1 to my - 2 do
    if (y mod 5) = 1 then
      for x := 1 to mx - 2 do
        if (x mod 10) = 1 then
          mvwaddch(win, y, x, randomchar OR COLOR_PAIR(randompair))
        else
          mvwaddch(win, y, x, ACS_HLINE)
    else
      for x := 1 to mx - 2 do
        if (x mod 10) = 1 then
          mvwaddch(win, y, x, ACS_VLINE)
        else
          mvwaddch(win, y, x, chtype(' '))
end;


function st_middle(scrlen, itemlen: Smallint): Smallint; inline;
begin
  st_middle := (scrlen - itemlen) div 2;
end;

procedure print_in_middle(win: PWINDOW; var nstr: TNcStr; width: Longint);
var
  my, mx: Smallint;
begin
  getmaxyx(win, my, mx);
  mx -= nstr.coord[1];

  if (width > length(nstr.str)) OR  (width < 1) then
    width := length(nstr.str);

  if width > mx then
    width := mx;

  nstr.coord[x] += st_middle(mx,width);

  wattron(win,nstr.attr);
  mvwaddnstr(win,nstr.coord[y],nstr.coord[x],PChar(nstr.str),width);
  wattroff(win,nstr.attr);
end;

type
  TBarData = packed record
    beg, len, slen: Smallint;
  end;

  TPad = class
  private
    wyx, pyx, ppos, grid: TNcCoord;
    hbar, vbar: TBarData;
    padwin, projwin: PWINDOW;
    panel: PPANEL;
    header: TNcStr;
    changed: Boolean;
    procedure init_bars;
    procedure draw_hbar;
    procedure draw_vbar;
  public
    function scroll_right: Boolean;
    function scroll_left: Boolean;
    function scroll_down: Boolean;
    function scroll_up: Boolean;
    function  doevent: chtype;
    procedure dorefresh;
    function  move(const ncoord: array of Smallint): Boolean; inline;
    function  hide: Boolean; inline;
    function  show: Boolean; inline;
    procedure resize;
    function  resize(const nsize: array of Smallint): Boolean;
    constructor create(const parm: array of TNcCoord; const hdr: TNcStr);
    destructor destroy; override;
    property win: PWINDOW read padwin;
    property ysize: Smallint read wyx[y];
    property xsize: Smallint read wyx[x];
  end;


procedure TPad.init_bars;

function get_scrl_len(blen, wsz, psz: Smallint): Smallint; inline;
begin
  get_scrl_len := (blen * wsz) div psz;
end;

begin
  hbar.beg  := 4;
  hbar.len  := wyx[x] - hbar.beg * 2;
  hbar.slen := get_scrl_len(hbar.len, wyx[x], pyx[x]);

  vbar.beg  := 2;
  vbar.len  := wyx[y] - vbar.beg * 2;
  vbar.slen := get_scrl_len(vbar.len, wyx[y], pyx[y]);
end;

function get_scrl_beg(ind, slen, blen, wsz, psz, bbeg: Smallint): Smallint;
begin
  if psz <> wsz then
    get_scrl_beg := (ind * (blen - slen)) div (psz - wsz) + bbeg
  else
    get_scrl_beg := bbeg;
end;

procedure TPad.draw_hbar;
var
  i, sbeg: Smallint;
begin
  with hbar do
  begin
    sbeg := get_scrl_beg(ppos[x],hbar.slen,hbar.len,wyx[x], pyx[x],hbar.beg);
    wattron(projwin,header.attr);
    for i :=  beg to beg + len - 1 do
    if (i < sbeg) OR (i > sbeg + slen) then
      mvwaddch(projwin,wyx[y]-1,i  ,ACS_CKBOARD)
    else
      mvwaddch(projwin,wyx[y]-1,i,ACS_BLOCK);
    wattroff(projwin,header.attr);
  end
end;

procedure TPad.draw_vbar;
var
  i, sbeg: Smallint;
begin
  with vbar do
  begin
    sbeg := get_scrl_beg(ppos[y],vbar.slen,vbar.len,wyx[y], pyx[y],vbar.beg);
    wattron(projwin,header.attr);
    for i :=  beg to beg + len - 1 do
    if (i < sbeg) OR (i > sbeg + slen) then
      mvwaddch(projwin,i,wyx[x]-1,ACS_CKBOARD)
    else
      mvwaddch(projwin,i,wyx[x]-1,ACS_BLOCK);
    wattroff(projwin,header.attr);
  end
end;

function TPad.scroll_right: Boolean;
begin
  if ppos[x] > 0 then
  begin
    if (ppos[x] < grid[x]) then
      ppos[x] := 0
    else
      ppos[x] -= grid[x];
    draw_hbar;
    changed := true;
    scroll_right := true
  end
  else
    scroll_right := false
end;

function TPad.scroll_left: Boolean;
var
  dwidth: Longint;
begin
  dwidth := pyx[x] - wyx[x] + 2;
  if ppos[x] < dwidth then
  begin
    if ppos[x] > (dwidth - grid[x]) then
      ppos[x] := dwidth
    else
      ppos[x] += grid[x];
    draw_hbar;
    changed := true;
    scroll_left := true
  end
  else
    scroll_left := false
end;

function TPad.scroll_down: Boolean;
begin
  if ppos[y] > 0 then
  begin
    if ppos[y] < grid[y] then
      ppos[y] := 0
    else
      ppos[y] -= grid[y];
    draw_vbar;
    changed := true;
    scroll_down := true
  end
  else
    scroll_down := false
end;

function TPad.scroll_up: Boolean;
var
  dheight: Longint;
begin
  dheight := pyx[y] - wyx[y] + 2;
  if ppos[y] < dheight then
  begin
    if ppos[y] > (dheight - grid[x]) then
      ppos[y] := dheight
    else
      ppos[y] += grid[x];
    draw_vbar;
    changed := true;
    scroll_up := true
  end
  else
    scroll_up := false
end;

function  TPad.doevent: chtype;
var
  ch: chtype;
  rval: Boolean = true;
begin
  ch := wgetch(projwin);
  case ch of
    KEY_DOWN:  rval := scroll_up;
    KEY_UP:    rval := scroll_down;
    KEY_LEFT:  rval := scroll_right;
    KEY_RIGHT: rval := scroll_left;
  end;
  if not rval then
  begin
    ncurses.beep();
    flash();
  end;
  doevent := ch
end;

procedure TPad.dorefresh;
var
  rval: Longint = OK;
begin
  if changed then
  begin
    rval := copywin(padwin,projwin,ppos[y],ppos[x],1,1,wyx[y]-2,wyx[x]-2, 0);
    assert(rval=OK,'copywin error');
    if rval = OK then
      changed := false;
  end
end;

function TPad.move(const ncoord: array of Smallint): Boolean;
begin
  move :=  move_panel(panel, ncoord[y], ncoord[x]) = OK
end;

function TPad.hide: Boolean;
begin
  hide := hide_panel(panel) = OK
end;

function TPad.show: Boolean;
begin
  show := show_panel(panel) = OK
end;

procedure TPad.resize;
var
  nsize: TNcCoord;
  doresize: Boolean = false;
begin
  getbegyx(projwin,nsize[y],nsize[x]);

  nsize[y] += wyx[y];
  nsize[x] += wyx[x];

  if nsize[y] > LINES then
  begin
    nsize[y] := LINES; doresize := true
  end
  else
    nsize[y] := wyx[y];

  if nsize[x] > COLS then
  begin
    nsize[x] := COLS; doresize := true
  end
  else
    nsize[x] := wyx[x];

  if doresize then
    resize(nsize)
end;

function TPad.resize(const nsize: array of Smallint): Boolean;
var
  by, bx: Smallint;
  domove: Boolean = false;
  tcoord: TNcCoord;
begin

  if (nsize[y] <= LINES)AND(nsize[x] <= COLS) then
  begin
    if nsize[y] > pyx[y] + 2 then
      tcoord[y] := pyx[y] + 2
    else
      tcoord[y] := nsize[y];

    if nsize[x] > pyx[x] + 2 then
      tcoord[x] := pyx[x] + 2
    else
      tcoord[x] := nsize[x];


    getbegyx(projwin, by, bx);

    if tcoord[y] + by >= LINES then
    begin
      by := LINES - tcoord[y]; domove := true
    end;

    if tcoord[x] + bx >= COLS then
    begin
      bx := COLS - tcoord[x]; domove := true
    end;

    if tcoord[x] > (pyx[x] - ppos[x]) then
      scroll_right;
    if tcoord[y] > (pyx[y] - ppos[y]) then
      scroll_down;

    hide_panel(panel);
    wresize(projwin, tcoord[y], tcoord[x]);

    if domove then
      move_panel(panel, by, bx);
    show_panel(panel);

    box(projwin, ACS_VLINE, ACS_HLINE);

    getmaxyx(projwin,wyx[y],wyx[x]);
    header.coord[y] := 0; header.coord[x] := 0;

    print_in_middle(projwin, header, 0);
    init_bars;
    draw_hbar;
    draw_vbar;

    changed := true;
    resize := true
  end
  else
    resize := false
end;

constructor TPad.create(const parm: array of TNcCoord; const hdr: TNcStr);
{$IFDEF DEBUG}
var
  tysz, txsz: Smallint;
{$ENDIF}
begin
  if parm[0,y] >= parm[1,y] + 2 then
    wyx[y] := parm[1,y] + 2
  else
    wyx[y] := parm[0,y];

  if parm[0,x] >= parm[1,x] + 2  then
    wyx[x] := parm[1,x] + 2
  else
    wyx[x] := parm[0,x];

  projwin := newwin(wyx[y], wyx[x], (LINES - wyx[y]) div 2, (COLS - wyx[x]) div 2);
  intrflush(projwin, FALSE);
  keypad(projwin, TRUE);
  box(projwin, ACS_VLINE, ACS_HLINE);

  panel := new_panel(projwin);
  padwin := newpad(parm[1,y], parm[1,x]);

  header := hdr;
  pyx := parm[1];
  grid := parm[2];

{$IFDEF DEBUG}
  getmaxyx(projwin,tysz, txsz);
  assert((wyx[y]=tysz)AND(wyx[x]=txsz), 'Invalid window');

  getmaxyx(padwin,tysz, txsz);
  assert((pyx[y]=tysz)AND(pyx[x]=txsz), 'Invalid pad');
{$ENDIF}
  FmtStr(header.str, '%s, pad: h=%d w=%d, win: h=%d w=%d', [hdr.str,pyx[y],pyx[x],wyx[y],wyx[x]]);


  print_in_middle(projwin, header, 0);

  init_bars;
  draw_hbar;
  draw_vbar;

  changed := true;
end;

destructor TPad.destroy;
begin
  del_panel(panel);
  delwin(padwin);
  delwin(projwin);
end;

procedure init_stdscr;
begin
  draw;
  attron(COLOR_PAIR(7));
  mvaddstr(LINES - 3, 0,'press "+" "-" to resize              ');
  mvaddstr(LINES - 2, 0,'press UP, DOWN, LEFT, RIGHT to scroll');
  mvaddstr(LINES - 1, 0,'press F10 or q to exit               ');
  attroff(COLOR_PAIR(7));
end;



var
  ch: chtype;
  ncpad: TPad;
  my_bg: Smallint = COLOR_BLACK;
  wnd, pad, grid: TNcCoord;
  code: Word;
  header: TNcStr = (str:'Pad demo';attr:A_NORMAL;coord:(0,0));
begin
  try
    initscr();
    noecho();
    clear();
    cbreak();
    curs_set(0);
    keypad(stdscr, TRUE);
    meta(stdscr, TRUE);
    mousemask(1, nil);

   if has_colors() then
   begin
     start_color();
     if (use_default_colors() = OK) then
       my_bg := -1
     else
       my_bg := COLOR_BLACK;

     init_pair(1, COLOR_YELLOW, my_bg);
     init_pair(2, COLOR_MAGENTA, my_bg);
     init_pair(3, COLOR_WHITE, my_bg);
     init_pair(4, COLOR_CYAN, my_bg);
     init_pair(5, COLOR_GREEN, my_bg);
     init_pair(6, COLOR_WHITE, COLOR_BLUE);
     init_pair(7, COLOR_BLACK, COLOR_YELLOW);
   end;

    init_stdscr;
    //refresh();

    wnd[y]  := LINES - 6;
    wnd[x]  := COLS - 12;
    pad[y]  := wnd[y] + 6;
    pad[x]  := wnd[x] + 6;
    grid[y] := 3;
    grid[x] := 3;


    if paramcount > 1 then
    begin
      val(ParamStr(1),pad[y],code);
      val(ParamStr(2),pad[x],code);
    end;

    if paramcount > 3 then
    begin
      val(ParamStr(3),wnd[y],code);
      val(ParamStr(4),wnd[x],code);
    end;

    header.attr := COLOR_PAIR(6);
    ncpad := TPad.create([wnd,pad,grid],header);
    draw_pad(ncpad.win);
    ncpad.dorefresh;
    update_panels();
    doupdate();

    repeat
      ch := ncpad.doevent;
      case ch of
        chtype('+'): ncpad.resize([ncpad.ysize + 1,ncpad.xsize + 1]);
        chtype('='): ncpad.resize([ncpad.ysize + 1,ncpad.xsize + 1]);
        chtype('-'): ncpad.resize([ncpad.ysize - 1,ncpad.xsize - 1]);
        chtype(' '): ncpad.resize([wnd[y],wnd[x]]);
        KEY_RESIZE:
        begin
          flash();
          init_stdscr;
          ncpad.resize;
        end;
      end;
      ncpad.dorefresh;
      update_panels();
      doupdate();
    until (ch = chtype('q')) OR (ch = KEY_F(10));
  finally
    ncpad.destroy;
    curs_set(1);
    endwin();
  end;
end.
