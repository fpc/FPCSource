Program GravityWars;
{A demo for TP 4.0 compability of Graph.

The sources for this game was found on a site that claims to only have
PD stuff with the below header(which was only reindented), and the webmaster
said that everything he published was sent to him with that purpose. We tried
to contact the authors mentioned below via mail over internet, but that
failed. If there is somebody that claims authorship of these programs,
please mail marco@freepascal.org, and the sources will be removed from our
websites.

------------------------------------------------------------------------

ORIGINAL Header:

     by Sohrab Ismail-Beigi     Completed 4/23/89
     SYSOP of The 3D Graphics BBS
     300/1200/2400 baud, N-8-1 Full duplex
     (201) 444-4154

     Turbo Pascal 4.0 source code.  Requires VGA 640x480x16 display.
     Note: pix=pixels in the comments}

{$ifdef Win32}
 {$apptype GUI}
{$endif}

Uses
 {$ifdef Win32}
  Windows,
  WinCrt,
 {$else}
  Crt,
 {$endif}
 Graph;

Type
    spacecraft=Record                       {used for ships and pointer}
                 coffx,coffy,r : longint;   {center offsets and radius in pix}
                 imagex,imagey : longint;   {upper left of image}
                 imagepointr   : pointer;   {pointer to image data}
                 imagesize     : word;      {size in bytes}
               end;
    planettype=Record
                 cx,cy,r : longint;         {planet center and radius}
                 d,GM    : real;            {density and G*M product}
               end;

Const
     color : array[1..3] of byte=(Red,Green,LightBlue); {colors for planets}
     G=0.1;                                             {gravity constant}
     bhr=15;                                            {black hole radius}
     Esc=#27;                                           {ASCII for Esc}
     Return=#13;                                        { "     "  RETURN}

Var
  ship      : array[1..2] of spacecraft;    {2 ships}
  tp,pointr : spacecraft;                   {tp is temporary, 1 pointer}
  pl        : array[1..9] of planettype;    {the 9 planets}
  screen    : Record                        {the game area}
                sx,ex,sy,ey,cx,cy,lx,ly : longint; {start x/y, end x/y, center}
              end;                                 {x/y, length x/y}
  np,GraphDriver,GraphMode : integer;              {# of planets}
  criticaldist : real;                             {for escape velocity calc}
  playsong  : boolean;                             {play the songs?}

Procedure Init;              {initialize everything}
begin
  //SetGraphBufSize(10);
  GraphDriver:=VGA;
  GraphMode:=VGAHi;
  {$ifdef Win32}
   ShowWindow(GetActiveWindow,0);
  {$endif}

  InitGraph(GraphDriver,GraphMode,'');
  setbkcolor(black);
  setviewport(0,0,getmaxx,getmaxy,clipoff);
  clearviewport;
  SetColor(LightGray);
  SetFillStyle(SolidFill,LightGray);      {Hull of ships}
  Circle(100,100,9);
  FloodFill(100,100,LightGray);
  Bar(77,98,100,102);
  MoveTo(82,98);
  LineRel(-3,-8);
  LineRel(-13,0);               LineRel(0,-3);
  LineRel(24,0);                LineRel(0,3);
  LineRel(-7,0);                LineRel(3,8);
  FloodFill(83,97,LightGray);
  MoveTo(82,101);               LineRel(-3,8);
  LineRel(-13,0);               LineRel(0,3);
  LineRel(24,0);                LineRel(0,-3);
  LineRel(-7,0);                LineRel(3,-8);
  FloodFill(83,103,LightGray);
  MoveTo(200,200);              LineRel(5,-5);
  LineRel(5,5);                 LineRel(10,0);
  LineRel(5,-8);                LineRel(15,0);
  LineRel(-6,9);                LineRel(6,9);
  LineRel(-15,0);               LineRel(-5,-7);
  LineRel(-10,0);               LineRel(-5,5);
  LineRel(-6,-7);               LineRel(2,-2);
  FloodFill(201,201,LightGray);
  SetColor(LightRed);
  SetFillStyle(SolidFill,LightRed); {Red lights on ships}
  Circle(100,100,2);
  FloodFill(100,100,LightRed);
  Bar(89,87,91,90);             Bar(89,109,91,112);
  Bar(224,200,226,203);         Bar(240,192,242,194);
  Bar(240,208,242,210);
  SetColor(Yellow);
  MoveTo(0,0);                  LineRel(0,10);
  MoveTo(0,0);                  LineRel(10,0);
  MoveTo(0,0);                  LineRel(15,15);   {pointer}
  tp.imagesize:=ImageSize(0,0,16,16);     {kludge to subdue compiler bug}
  GetMem(tp.imagepointr,tp.imagesize);
  GetImage(0,0,16,16,tp.imagepointr^);
  pointr.imagesize:=ImageSize(0,0,16,16);
  GetMem(pointr.imagepointr,pointr.imagesize);
  GetImage(0,0,16,16,pointr.imagepointr^);           {get pointer}
  pointr.coffx:=7;
  pointr.coffy:=7;
  pointr.r:=9;
  ship[1].imagesize:=ImageSize(66,87,110,113);
  GetMem(ship[1].imagepointr,ship[1].imagesize);
  GetImage(66,87,110,113,ship[1].imagepointr^);      {enterprise}
  ship[1].coffx:=22; ship[1].coffy:=13; ship[1].r:=26;
  ship[2].imagesize:=ImageSize(199,192,242,210);
  GetMem(ship[2].imagepointr,ship[2].imagesize);
  GetImage(199,192,242,210,ship[2].imagepointr^);     {klingon}
  ship[2].coffx:=21; ship[2].coffy:=9; ship[2].r:=23;
  ClearDevice;
  screen.sx:=1;
  screen.ex:=638;
  screen.sy:=33;
  screen.ey:=478;
  screen.cx:=(screen.sx+screen.ex) div 2;                 {initialize screen}
  screen.cy:=(screen.sy+screen.ey) div 2;                            {bounds}
  screen.lx:=screen.ex-screen.sx+1;
  screen.ly:=screen.ey-screen.sy+1;
  criticaldist:=2.0*sqrt(sqr(screen.lx)+sqr(screen.ly)); {critical distance}
  playsong:=true;                                    {for escape vel. calc}
end;

Procedure Finish;   {free memory and end}
begin
  FreeMem(ship[1].imagepointr,ship[1].imagesize);
  FreeMem(ship[2].imagepointr,ship[2].imagesize);
  FreeMem(pointr.imagepointr,pointr.imagesize);
  FreeMem(tp.imagepointr,tp.imagesize);
  CloseGraph;
end;

Function InBounds(cx,cy,r:longint):boolean; {is the point with radius}
begin                                       {completely in screen bounds?}
   InBounds:=true;
   if r<>0 then
     if (cx-r<=screen.sx) or (cx+r>=screen.ex) or
        (cy-r<=screen.sy) or (cy+r>=screen.ey) then
          InBounds:=false
   else
     if (cx-bhr<=screen.sx) or (cx+bhr>=screen.ex) or
        (cy-bhr<=screen.sy) or (cy+bhr>=screen.ey) then
          InBounds:=false;
end;

Procedure RandomSetup;   {make a random setup}
var i,j : integer;
    a,b : longint;
    ok  : boolean;
begin
  Randomize;
  np:=Random(9)+1;   {random # of planets 1-9}
  for i:=1 to np do  {pick planet positions}
    Repeat
      ok:=true;
      pl[i].cx:=Random(screen.lx)+screen.sx;
      pl[i].cy:=Random(screen.ly)+screen.sy;
      pl[i].d:=(Random(3)+2)/2.0;
      pl[i].r:=0;
      if Random>0.05 then pl[i].r:=Random(70)+20; {5% chance of blackhole}
      if pl[i].r<>0 then
        pl[i].GM:=G*2*pi*sqr(pl[i].r)*pl[i].d
      else
        pl[i].GM:=G*2*pi*sqr(30)*1.0;
      ok:=InBounds(pl[i].cx,pl[i].cy,pl[i].r);
      if (i>1) and (ok) then          {any collisions with existing planets?}
        for j:=1 to i-1 do
          begin
          if sqrt(sqr(pl[i].cx-pl[j].cx)+sqr(pl[i].cy-pl[j].cy))<=
            pl[i].r+pl[j].r+2*bhr then
               ok:=false;
          end;
    Until ok;
  for i:=1 to 2 do   {pick ship positions}
    Repeat
      ok:=true;
      ship[i].imagex:=Random(screen.lx div 2)+screen.sx; {enterprise to the}
      if i=2 then ship[2].imagex:=ship[i].imagex+screen.lx div 2; {left and}
      ship[i].imagey:=Random(screen.ly)+screen.sy;      {klingon to the right}
      a:=ship[i].imagex+ship[i].coffx; b:=ship[i].imagey+ship[i].coffy;
      ok:=InBounds(a,b,ship[i].r);
      for j:=1 to np do           {any collisions with planets?}
        if sqrt(sqr(a-pl[j].cx)+sqr(b-pl[j].cy))<=pl[j].r+ship[i].r+bhr then
           ok:=false;
    Until ok;
end;

Procedure DrawSetup;  {draw current setup}
var i,j : integer;
begin
  ClearDevice;
  SetColor(White);
  Rectangle(screen.sx-1,screen.sy-1,screen.ex-1,screen.ey-1); {game box}
  for i:=1 to 2000 do             {2000 random stars}
    PutPixel(Random(screen.lx)+screen.sx,Random(screen.ly)+screen.sy,White);
  for i:=1 to 2 do  {2 ships}
    PutImage(ship[i].imagex,ship[i].imagey,ship[i].imagepointr^,NormalPut);
  for i:=1 to np do  {np planets}
    if pl[i].r>0 then   {normal}
      begin
        SetColor(color[trunc(pl[i].d*2-1)]);
        Circle(pl[i].cx,pl[i].cy,pl[i].r);
        SetFillStyle(SolidFill,color[trunc(pl[i].d*2-1)]);
        FloodFill(pl[i].cx,pl[i].cy,color[trunc(pl[i].d*2-1)]);
      end
    else               {black hole}
      begin
        SetColor(Black);
        for j:=0 to bhr do
          Circle(pl[i].cx,pl[i].cy,j);
      end;
end;

Procedure ClearDialogBox;  {clear text message area}
begin
  SetFillStyle(SolidFill,Black);
  Bar(0,0,screen.ex-1,screen.sy-2);
end;

Function GetString:string;  {get a string until RETURN is pressed}
var s : string;
    c : char;
begin
  s:='';
  Repeat
    c:=ReadKey;
    if (c=chr(8)) and (length(s)>0) then          {backspace key}
        begin
          delete(s,length(s),1);
          MoveRel(-8,0);                          {delete last char}
          SetFillStyle(SolidFill,Black);
          Bar(GetX,GetY,GetX+8,GetY+8);
        end
    else if c<>Return then
      begin
        s:=concat(s,c);                           {get and draw char}
        SetColor(LightGray);
        OutText(c);
      end;
  Until c=Return;
  GetString:=s;
end;

Procedure PlayGame;
Const number_of_explosion_dots=20;   {# dots for explosion with planet surface}
Var vx,vy,vc,x,y,dt,ax,ay,dx,dy,dr,k : real;
    v0,angle : array[1..2] of real;
    s : string;
    ch : char;
    i,event,player,winner : integer;
    ok,donecritical,offscreen : boolean;
    buffer : array[1..number_of_explosion_dots] of Record  {for explosion}
                                                     x,y,color : integer;
                                                   end;
begin
  v0[1]:=0; v0[2]:=0; angle[1]:=0; angle[2]:=0;
  player:=1;
  donecritical:=false;
  Repeat                               {infinite loop}
    ClearDialogBox;
    SetColor(LightGray);
    str(player,s);
    s:=concat('Player ',s);        {player #}
    OutTextXY(0,0,s);
    Repeat                         {get angle}
      MoveTo(0,10);
      str(angle[player]:3:5,s);
      s:=concat('Angle: [',s,']: ');
      OutText(s);
      s:=GetString;
      if (s[1]='Q') or (s[1]='q') then exit;
      i:=0;
      if s<>'' then Val(s,angle[player],i);
      SetFillStyle(SolidFill,Black);
      ok:=(i=0) and (angle[player]>=0.0) and (angle[player]<=360);
      if not ok then Bar(0,10,screen.ex-1,18);
    Until ok;
    Repeat                        {get initial velocity}
      MoveTo(0,20);
      str(v0[player]:2:5,s);
      s:=concat('Initial Velocity: [',s,']: ');
      OutText(s);
      s:=GetString;
      if (s[1]='Q') or (s[1]='q') then exit;
      i:=0;
      if s<>'' then Val(s,v0[player],i);
      SetFillStyle(SolidFill,Black);
      ok:=(i=0) and (v0[player]>=0.0) and (v0[player]<=10.0);
      if not ok then Bar(0,20,screen.ex-1,28);
    Until ok;
    k:=pi*angle[player]/180.0;   {angle in radians}
    vx:=v0[player]*cos(k);
    vy:=-v0[player]*sin(k);
    x:=ship[player].imagex+ship[player].coffx+ship[player].r*cos(k);
    y:=ship[player].imagey+ship[player].coffy-ship[player].r*sin(k);
    ClearDialogBox;
    MoveTo(round(x),round(y));
    SetColor(White);
    offscreen:=false;
    Repeat                       {calculate and draw trajectory}
      dt:=0.25;                  {time interval [vel. is in pix/time]}
      x:=x+vx*dt; y:=y+vy*dt;
      ax:=0; ay:=0;
      for i:=1 to np do          {calc accel. due to gravity}
        begin
          dx:=x-pl[i].cx; dy:=y-pl[i].cy; dr:=sqrt(sqr(dx)+sqr(dy));
          k:=1/(sqr(dr)*dr);
          if pl[i].r<>0 then       {normal}
            begin
              ax:=ax-pl[i].GM*dx*k;
              ay:=ay-pl[i].GM*dy*k
            end
          else                     {black hole}
            begin
              ax:=ax-pl[i].GM*dx*(k+sqr(k*dr));
              ay:=ay-pl[i].GM*dy*(k+sqr(k*dr));
            end;
        end;
      vx:=vx+ax*dt; vy:=vy+ay*dt;
      event:=0;
      if keypressed then
        event:=1
      else if (x>=screen.sx) and (x<=screen.ex) and        {in screen bounds?}
              (y>=screen.sy) and (y<=screen.ey) then
         begin
           donecritical:=false;
           i:=GetPixel(round(x),round(y));
           if (i=color[1]) or (i=color[2]) or (i=color[3]) or
              (i=LightRed) or (i=LightGray) then event:=2
           else
             if offscreen then
               MoveTo(round(x),round(y))
             else
               LineTo(round(x),round(y));
           offscreen:=false;
         end                                               {off screen}
      else if not donecritical then
        begin
          offscreen:=true;               {offscreen and critical distance}
          dx:=x-screen.cx; dy:=y-screen.cy; dr:=sqrt(sqr(dx)+sqr(dy));
          if dr>=criticaldist then
            begin
              vc:=(dx*vx+dy*vy)/dr;
              k:=0; for i:=1 to np do k:=k+pl[i].GM;
              if 0.5*sqr(vc)>=k/dr then     {do we have escape velocity?}
                event:=3;
            end;
        end;
    Until event<>0;
    if event=1 then          {a key was pressed for a break}
      begin
        ClearDialogBox;
        ch:=ReadKey; {one already in buffer}
        SetColor(LightGray);
        OutTextXY(0,0,'Break... Esc to break, any other key to continue');
        ch:=ReadKey;
        if ch=Esc then exit;
      end
    else if event=3 then       {missile escaped the universe}
      begin
        ClearDialogBox;
        SetColor(LightGray);
        OutTextXY(0,0,'Missile left the galaxy...');
        delay(2000);
      end
    else           {event=2}   {hit something}
      begin
        if (i=color[1]) or (i=color[2]) or (i=color[3]) then  {hit a planet}
          begin
            for i:=1 to number_of_explosion_dots do     {draw explosion}
              begin
                buffer[i].x:=trunc(x+20*(Random-0.5));
                buffer[i].y:=trunc(y+20*(Random-0.5));
                buffer[i].color:=GetPixel(buffer[i].x,buffer[i].y);
                PutPixel(buffer[i].x,buffer[i].y,LightRed);
                delay(25);
              end;
            delay(1000);
            for i:=1 to number_of_explosion_dots do     {erase explosion}
              PutPixel(buffer[i].x,buffer[i].y,buffer[i].color);
          end
        else    {hit a ship!}
          begin
            if sqrt(sqr(x-ship[1].imagex-ship[1].coffx)+ {which one won?}
                    sqr(y-ship[1].imagey-ship[1].coffy))<=ship[1].r+5 then
                      winner:=2
            else winner:=1;
            for event:=1 to 100 do          {flash the screen}
              SetPalette(Black,Random(16));
            SetPalette(Black,Black);
            for i:=1 to 1000 do    {put some white and red points}
              begin
                k:=Random*2*pi;
                event:=Random(3);
                if event=0 then
                  PutPixel(trunc(x+30*Random*cos(k)),trunc(y+30*Random*sin(k)),Black)
                else if event=1 then
                  PutPixel(trunc(x+30*Random*cos(k)),trunc(y+30*Random*sin(k)),Red)
                else
                  PutPixel(trunc(x+20*Random*cos(k)),trunc(y+20*Random*sin(k)),White);
              end;
            ClearDialogBox;
            SetColor(LightGray);
            str(winner,s);
            s:=concat('Player ',s,' wins!!!');    {announce}
            OutTextXY(0,0,s);
            if playsong then                      {play a tune}
              begin
                Sound(440); delay(150);
                Nosound; delay(50);
                Sound(440); delay(150);
                Sound(554); delay(150);
                Sound(659); delay(350);
                Sound(554); delay(150);
                Sound(659); delay(450);
                Nosound; delay(500);
                Sound(880); delay(800);
                Nosound;
              end;
            delay(3000);
            exit;
          end;
      end; {if event=3}
    Inc(player); if player=3 then player:=1;    {next player}
  Until true=false; {infinite loop}
end;

Procedure PlayingtheGame;     {playing the game menu}
var option : char;
begin
  Repeat
    ClearDialogBox;
    SetColor(LightGray);
    OutTextXY(0,0,'1. Random setup   2. Play game    Esc quits menu');
    OutTextXY(0,10,'Option: ');
    option:=ReadKey;
    Case option of
      '1' : begin
              ClearDialogBox;
              RandomSetup;
              DrawSetup;
            end;
      '2' : PlayGame;
    end;
  Until option=Esc;
end;

Procedure Options;   {options menu}
var option : char;
begin
  Repeat
    ClearDialogBox;
    SetColor(LightGray);
    OutTextXY(0,0,'1. Redraw screen   2. Sound on/off     Esc quits menu');
    OutTextXY(0,10,'Option: ');
    option:=ReadKey;
    Case option of
      '1' : DrawSetUp;
      '2' : playsong:=not playsong;
    end;
  Until option=Esc;
end;

Procedure InterpKey(c:char; var x,y,coffx,coffy,r:longint;
                            var jump:integer; var moveit:boolean);
begin              {interprets keys for movement of pointer, mainly to save}
  Case c of                {space due to shared code in many Change routines}
    '+' : if jump<49 then Inc(jump,2);
    '-' : if jump>2 then Dec(jump,2);
    '8' : begin                              {up}
            Dec(y,jump);
            if InBounds(x+coffx,y+coffy,r) then
              moveit:=true
            else
              Inc(y,jump);
          end;
    '2' : begin                              {down}
            Inc(y,jump);
            if InBounds(x+coffx,y+coffy,r) then
              moveit:=true
            else
              Dec(y,jump);
          end;
    '4' : begin                              {left}
            Dec(x,jump);
            if InBounds(x+coffx,y+coffy,r) then
              moveit:=true
            else
              Inc(x,jump);
          end;
    '6' : begin                              {right}
            Inc(x,jump);
            if InBounds(x+coffx,y+coffy,r) then
              moveit:=true
            else
              Dec(x,jump);
          end;
  end; {case c of}
end;

Procedure MoveShip;    {move a given ship to a new legal position}
var c : char;
    s,jump,j : integer;
    x,y,xold,yold,a,b : longint;
    legal,moveit : boolean;
begin
  ClearDialogBox;
  SetColor(LightGray);
  OutTextXY(0, 0,'Ships:  1. Enterprise   2. Klingon    Esc aborts');
  OutTextXY(0,10,'Which ship? ');     {get the proper ship}
  Repeat
    c:=ReadKey;
  Until (c='1') or (c='2') or (c=Esc);
  if c=Esc then exit;
  if c='1' then s:=1 else s:=2;
  ClearDialogBox;
  OutTextXY(0, 0,'Use cursors to move ship. (Num Lock on)   Esc aborts');
  OutTextXY(0,10,'Enter to place, + and - to change size of jumps.');
  jump:=30;
  x:=ship[s].imagex; y:=ship[s].imagey;
  Repeat    {loop until Esc or somewhere legal}
    Repeat    {loop until Esc or RETURN}
      Repeat c:=ReadKey; Until (c='4') or (c='8') or (c='6') or (c='2') or
                               (c='+') or (c='-') or (c=Return) or (c=Esc);
      moveit:=false; xold:=x; yold:=y;
      InterpKey(c,x,y,ship[s].coffx,ship[s].coffy,ship[s].r,jump,moveit);
      if moveit then  {if can move the image,}
        begin
          PutImage(xold,yold,ship[s].imagepointr^,XORPut); {erase old}
          PutImage(x,y,ship[s].imagepointr^,XORPut);       {draw new}
          moveit:=false;
        end;
    Until (c=Return) or (c=Esc);
    if c=Esc then                     {abort}
      begin
        PutImage(x,y,ship[s].imagepointr^,XORPut);
        PutImage(ship[s].imagex,ship[s].imagey,ship[s].imagepointr^,NormalPut);
        exit;
      end;
    a:=x+ship[s].coffx; b:=y+ship[s].coffy;
    legal:=InBounds(a,b,ship[s].r);     {in bounds?}
    for j:=1 to np do                   {in collision with any planets?}
      if sqrt(sqr(a-pl[j].cx)+sqr(b-pl[j].cy))<=pl[j].r+ship[s].r+bhr then
         legal:=false;
    if not legal then                   {oops! not legal!}
      begin
        SetPalette(Black,White);
        SetFillStyle(SolidFill,Black);
        Bar(0,20,screen.ex,screen.sy-2);
        delay(100);
        SetPalette(Black,Black);
        SetColor(LightGray);
        OutTextXY(0,20,'Illegal ship position!');
      end;
  Until legal;
  ship[s].imagex:=x; ship[s].imagey:=y;    {ok, place it there}
end;

Procedure MovePlanet;   {move a planet}
var c : char;
    i,p,jump : integer;
    x,y,xold,yold,minr,t,cxorig,cyorig : longint;
    moveit,legal : boolean;
begin
  ClearDialogBox;
  if np=0 then         {no planets!}
    begin
      OutTextXY(0,0,'No planets to move!');
      delay(2000);
      exit;
    end;
  OutTextXY(0, 0,'Use cursors to move pointer. (Num Lock on)   Esc aborts');
  OutTextXY(0,10,'Enter to pick planet, + and - to change size of jumps.');
  jump:=30;
  x:=100; y:=100; PutImage(x,y,pointr.imagepointr^,XORPut);
  Repeat    {loop until Esc or RETURN}
    Repeat c:=ReadKey; Until (c='4') or (c='8') or (c='6') or (c='2') or
                             (c='+') or (c='-') or (c=Return) or (c=Esc);
    moveit:=false; xold:=x; yold:=y;
    InterpKey(c,x,y,pointr.coffx,pointr.coffy,pointr.r,jump,moveit);
    if moveit then
      begin
        PutImage(xold,yold,pointr.imagepointr^,XORPut);
        PutImage(x,y,pointr.imagepointr^,XORPut);
        moveit:=false;
      end;
  Until (c=Return) or (c=Esc);
  PutImage(x,y,pointr.imagepointr^,XORPut);   {erase pointer}
  if c=Esc then exit;
  p:=0; minr:=trunc(sqrt(sqr(screen.lx)+sqr(screen.ly)));
  for i:=1 to np do   {find the closest planet/black hole}
    begin
      t:=trunc(sqrt(sqr(x-pl[i].cx)+sqr(y-pl[i].cy)));
      if t<minr then begin minr:=t; p:=i; end;
    end;
  SetColor(LightGreen);                      {clear it out}
  Circle(pl[p].cx,pl[p].cy,pl[p].r);
  SetFillStyle(SolidFill,Black);
  FloodFill(pl[p].cx,pl[p].cy,LightGreen);
  SetColor(Black);
  Circle(pl[p].cx,pl[p].cy,pl[p].r);
  ClearDialogBox;
  SetColor(LightGray);
  OutTextXY(0, 0,'Use cursors to move pointer. (Num Lock on)   Esc aborts');
  OutTextXY(0,10,'Enter to place planet center, + - change size of jumps.');
  jump:=30;
  x:=100; y:=100; PutImage(x,y,pointr.imagepointr^,XORPut);
  cxorig:=pl[p].cx; cyorig:=pl[p].cy;   {save them as they may change later}
  Repeat    {loop until Esc or legal position}
    Repeat
      Repeat c:=ReadKey; Until (c='4') or (c='8') or (c='6') or (c='2') or
                               (c='+') or (c='-') or (c=Return) or (c=Esc);
      moveit:=false; xold:=x; yold:=y;
      InterpKey(c,x,y,pointr.coffx,pointr.coffy,pointr.r,jump,moveit);
      if moveit then
        begin
          PutImage(xold,yold,pointr.imagepointr^,XORPut);
          PutImage(x,y,pointr.imagepointr^,XORPut);
          moveit:=false;
        end;
    Until (c=Return) or (c=Esc);
    legal:=true;
    if c<>Esc then    {ok, RETURN pressed}
      begin
        pl[p].cx:=-1000; pl[p].cy:=-1000;  {so it won't collide with itself!}
        for i:=1 to np do   {any collisions with other planets?}
          if sqrt(sqr(x-pl[i].cx)+sqr(y-pl[i].cy))<=pl[i].r+pl[p].r+2*bhr then
            legal:=false;
        for i:=1 to 2 do    {any collisions with other ships?}
          if sqrt(sqr(x-ship[i].imagex-ship[i].coffx)+
                  sqr(y-ship[i].imagey-ship[i].coffy))<=pl[p].r+ship[i].r+bhr
             then legal:=false;
      end;
    if not legal then      {oops!}
      begin
        SetPalette(Black,White);
        SetFillStyle(SolidFill,Black);
        Bar(0,20,screen.ex,screen.sy-2);
        delay(100);
        SetPalette(Black,Black);
        SetColor(LightGray);
        OutTextXY(0,20,'Illegal planet position!');
      end;
  Until legal;
  pl[p].cx:=x; pl[p].cy:=y; {put it there}
  if c=Esc then             {abort and restore}
    begin
      pl[p].cx:=cxorig;
      pl[p].cy:=cyorig;
    end;
  DrawSetUp;                {redraw screen}
end;

Procedure MakePlanet;       {make a planet given center and radius}
var c : char;
    i,p,jump : integer;
    x,y,xold,yold : longint;
    moveit,legal : boolean;
begin
  ClearDialogBox;
  if np=9 then       {too many planets already!}
    begin
      OutTextXY(0,0,'Can not make any more planets!');
      delay(2000);
      exit;
    end;
  OutTextXY(0, 0,'Use cursors to move pointer. (Num Lock on)   Esc aborts');
  OutTextXY(0,10,'Enter to place center, + and - to change size of jumps.');
  jump:=30;
  x:=100; y:=100; PutImage(x,y,pointr.imagepointr^,XORPut);
  Repeat   {loop until a legal center is picked or Esc}
    Repeat
      Repeat c:=ReadKey; Until (c='4') or (c='8') or (c='6') or (c='2') or
                               (c='+') or (c='-') or (c=Return) or (c=Esc);
      moveit:=false; xold:=x; yold:=y;
      InterpKey(c,x,y,pointr.coffx,pointr.coffy,pointr.r,jump,moveit);
      if moveit then
        begin
          PutImage(xold,yold,pointr.imagepointr^,XORPut);
          PutImage(x,y,pointr.imagepointr^,XORPut);
          moveit:=false;
        end;
    Until (c=Return) or (c=Esc);
    if c=Esc then exit;
    legal:=true;
    for i:=1 to np do    {any collisions with planets?}
      if sqrt(sqr(x-pl[i].cx)+sqr(y-pl[i].cy))<=pl[i].r+2*bhr then
        legal:=false;
    for i:=1 to 2 do     {any collisions with ships?}
      if sqrt(sqr(x-ship[i].imagex-ship[i].coffx)+
              sqr(y-ship[i].imagey-ship[i].coffy))<=ship[i].r+bhr
         then legal:=false;
    if not legal then                    {uh oh!}
      begin
        SetPalette(Black,White);
        SetFillStyle(SolidFill,Black);
        Bar(0,20,screen.ex,screen.sy-2);
        delay(100);
        SetPalette(Black,Black);
        SetColor(LightGray);
        OutTextXY(0,20,'Illegal planet center!');
      end;
  Until legal;
  p:=np+1; pl[p].cx:=x; pl[p].cy:=y;   {ok, store the info}
  ClearDialogBox;
  OutTextXY(0, 0,'Use cursors to move pointer. (Num Lock on)   Esc aborts');
  OutTextXY(0,10,'Enter to radius, + and - change size of jumps.');
  jump:=30;
  Repeat     {loop until a legal radius is entered or Esc}
    Repeat
      Repeat c:=ReadKey; Until (c='4') or (c='8') or (c='6') or (c='2') or
                               (c='+') or (c='-') or (c=Return) or (c=Esc);
      moveit:=false; xold:=x; yold:=y;
      InterpKey(c,x,y,pointr.coffx,pointr.coffy,pointr.r,jump,moveit);
      if moveit then
        begin
          PutImage(xold,yold,pointr.imagepointr^,XORPut);
          PutImage(x,y,pointr.imagepointr^,XORPut);
          moveit:=false;
        end;
    Until (c=Return) or (c=Esc);
    if c=Esc then exit;
    legal:=true;
    pl[p].r:=round(sqrt(sqr(x-pl[p].cx)+sqr(y-pl[p].cy))); {find radius}
    for i:=1 to np do    {planet collisions?}
      if sqrt(sqr(x-pl[i].cx)+sqr(y-pl[i].cy))<=pl[p].r+pl[i].r+2*bhr then
        legal:=false;
    for i:=1 to 2 do     {ship collisions?}
      if sqrt(sqr(x-ship[i].imagex-ship[i].coffx)+
              sqr(y-ship[i].imagey-ship[i].coffy))<=pl[p].r+ship[i].r+bhr
         then legal:=false;
    if not legal then    {oh no!}
      begin
        SetPalette(Black,White);
        SetFillStyle(SolidFill,Black);
        Bar(0,20,screen.ex,screen.sy-2);
        delay(100);
        SetPalette(Black,Black);
        SetColor(LightGray);
        OutTextXY(0,20,'Illegal planet radius!');
      end;
  Until legal;
  PutImage(x,y,pointr.imagepointr^,XORPut); {kill the pointer}
  Inc(np);    {actually add the new planet info}
  pl[p].d:=1.0; pl[p].GM:=G*2*pi*sqr(pl[p].r)*1.0; {initialize it}
  SetColor(color[1]);                      {draw it}
  Circle(pl[p].cx,pl[p].cy,pl[p].r);
  SetFillStyle(SolidFill,color[1]);
  FloodFill(pl[p].cx,pl[p].cy,color[1]);
end;

Procedure ChangePlanet;   {change density [color] of a planet}
var c : char;               {will not change black holes}
    i,p,jump : integer;
    x,y,xold,yold,minr,t : longint;
    moveit,legal : boolean;
begin
  ClearDialogBox;
  legal:=false;
  if np>0 then             {see if any non-black holes exist}
    for i:=1 to np do
      if pl[i].r<>0 then legal:=true;
  if (np=0) or (not legal) then   {sorry!}
    begin
      OutTextXY(0,0,'No planets to change!');
      delay(2000);
      exit;
    end;
  OutTextXY(0, 0,'Use cursors to move pointer. (Num Lock on)   Esc aborts');
  OutTextXY(0,10,'Enter to pick planet, + and - to change size of jumps.');
  jump:=30;
  x:=100; y:=100; PutImage(x,y,pointr.imagepointr^,XORPut);
  Repeat   {repeat until RETURN or Esc}
    Repeat c:=ReadKey; Until (c='4') or (c='8') or (c='6') or (c='2') or
                             (c='+') or (c='-') or (c=Return) or (c=Esc);
    moveit:=false; xold:=x; yold:=y;
    InterpKey(c,x,y,pointr.coffx,pointr.coffy,pointr.r,jump,moveit);
    if moveit then
      begin
        PutImage(xold,yold,pointr.imagepointr^,XORPut);
        PutImage(x,y,pointr.imagepointr^,XORPut);
        moveit:=false;
      end;
  Until (c=Return) or (c=Esc);
  PutImage(x,y,pointr.imagepointr^,XORPut);  {kill the pointer}
  if c=Esc then exit;
  p:=0; minr:=trunc(sqrt(sqr(screen.lx)+sqr(screen.ly)));
  for i:=1 to np do   {find closest non-black hole planet}
    begin
      t:=trunc(sqrt(sqr(x-pl[i].cx)+sqr(y-pl[i].cy)));
      if (t<minr) and (pl[i].r<>0) then begin minr:=t; p:=i; end;
    end;
  ClearDialogBox;
  OutTextXY(0, 0,'Change to: 1. Red   2. Green   3. Blue    Esc aborts');
  OutTextXY(0,10,'Option: ');    {get a density}
  Repeat c:=ReadKey; Until (c='1') or (c='2') or (c='3') or (c=Esc);
  if c=Esc then exit;
  i:=Ord(c)-48;
  pl[p].d:=(i+1)/2.0;       {new density}
  SetColor(color[i]);       {redraw}
  Circle(pl[p].cx,pl[p].cy,pl[p].r);
  SetFillStyle(SolidFill,color[i]);
  FloodFill(pl[p].cx,pl[p].cy,color[i]);
end;

Procedure DeletePlanet;   {kill a planet/black hole}
var c : char;
    i,p,jump : integer;
    x,y,xold,yold,minr,t : longint;
    moveit : boolean;
begin
  ClearDialogBox;
  if np=0 then    {nobody there!}
    begin
      OutTextXY(0,0,'No planets to delete!');
      delay(2000);
      exit;
    end;
  OutTextXY(0, 0,'Use cursors to move pointer. (Num Lock on)   Esc aborts');
  OutTextXY(0,10,'Enter to pick planet, + and - to change size of jumps.');
  jump:=30;
  x:=100; y:=100; PutImage(x,y,pointr.imagepointr^,XORPut);
  Repeat
    Repeat c:=ReadKey; Until (c='4') or (c='8') or (c='6') or (c='2') or
                             (c='+') or (c='-') or (c=Return) or (c=Esc);
    moveit:=false; xold:=x; yold:=y;
    InterpKey(c,x,y,pointr.coffx,pointr.coffy,pointr.r,jump,moveit);
    if moveit then
      begin
        PutImage(xold,yold,pointr.imagepointr^,XORPut);
        PutImage(x,y,pointr.imagepointr^,XORPut);
        moveit:=false;
      end;
  Until (c=Return) or (c=Esc);
  PutImage(x,y,pointr.imagepointr^,XORPut);
  if c=Esc then exit;
  p:=0; minr:=trunc(sqrt(sqr(screen.lx)+sqr(screen.ly)));
  for i:=1 to np do  {find the closest planet/black hole}
    begin
      t:=trunc(sqrt(sqr(x-pl[i].cx)+sqr(y-pl[i].cy)));
      if t<minr then begin minr:=t; p:=i; end;
    end;
  if p<9 then           {move everybody above the one deleted one down}
    for i:=p to np-1 do
      pl[i]:=pl[i+1];
  Dec(np);         {delete}
  DrawSetup;       {redraw}
end;

Procedure Changes;   {changes menu}
var option : char;
begin
  Repeat
    ClearDialogBox;
    SetColor(LightGray);
    OutTextXY(0, 0,'1. Move ship       2. Move planet    3. Make planet');
    OutTextXY(0,10,'4. Change planet   5. Delete planet     Esc quits menu');
    OutTextXY(0,20,'Option: ');
    option:=ReadKey;
    Case option of
      '1' : MoveShip;
      '2' : MovePlanet;
      '3' : MakePlanet;
      '4' : ChangePlanet;
      '5' : DeletePlanet;
    end;
  Until option=Esc;
end;

Procedure MainMenu;   {main menu}
var option : char;
begin
  Repeat
    ClearDialogBox;
    SetColor(LightGray);
    OutTextXY(0,0,'1. Playing the game   2. Options   3. Changes   4. Quit');
    OutTextXY(0,10,'Option: ');
    option:=ReadKey;
    Case option of
      '1' : PlayingtheGame;
      '2' : Options;
      '3' : Changes;
    end;
  Until option='4';
end;

Procedure Title;   {title screen and credits}
begin
  SetTextStyle(SansSerifFont,HorizDir,9);
  OutTextXY(25,100,'Gravity Wars');
  SetTextStyle(SansSerifFont,HorizDir,2);
  OutTextXY(300,300,'by Sohrab Ismail-Beigi');
  delay(3000);
  SetTextStyle(DefaultFont,HorizDir,0);
end;

BEGIN
  Init;
  Title;
  RandomSetup;
  DrawSetup;
  MainMenu;
  Finish;
END.
