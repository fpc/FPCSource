Program MapMaker;

uses Exec, graphics, Intuition, Utility;

{$I tagutils.inc}

{
    Patrick Quaid.
    This program just draws a blocky map from straight overhead,
then repeatedly splits each block into four parts and adjusts the
elevation of each of the parts until it gets down to one pixel per
block.  It ends up looking something like a terrain map.  It's kind
of a fractal thing, but not too much.  Some program a long time ago
inspired this, but I apologize for forgetting which one.  As I
recall, that program was derived from Chris Gray's sc.
    Once upon a time I was thinking about writing an overblown
strategic conquest game, and this was the first stab at a map
maker.  The maps it produces look nifty, but have no sense of
geology so they're really not too useful for a game.
    When the map is finished, press the left button inside the
window somewhere and the program will go away.
}

{
    Changed the source to 2.0+.
    12 May 1998.

    Translated to FPC. This was one of the first
    program I tried with fpc, just to check that
    the amiga units worked.
    08 Aug 1998.
    nils.sjoholm@mailbox.swipnet.se
}

const
    MinX = 0;
    MaxX = 320;
    MinY = 0;
    MaxY = 200;

type
    MapArray = array [MinX .. MaxX - 1, MinY .. MaxY - 1] of Longint;

VAR
    average,x,y,
    nextx,nexty,count1,
    skip,level    : Longint;
    rp            : pRastPort;
    vp            : Pointer;
    s             : pScreen;
    w             : pWindow;
    m             : pMessage;
    Map           : MapArray;
    Quit          : Boolean;
    i             : Longint;
    thetags       : Array[0..12] of tTagItem;

Function FixX(x : Longint): Longint;
begin
    if x < 0 then
    FixX := x + MaxX
    else if x >= MaxX then
    FixX := x mod MaxX
    else
    FixX := x;
end;

Function FixY(y : Longint) : Longint;
begin
    if x < 0 then
    FixY := y + MaxY
    else if x >= MaxY then
    FixY := y mod MaxY
    else
    FixY := y;
end;

Procedure DrawMap;
begin
    if skip = 1 then begin
    for x := MinX to MaxX - 1 do begin
        for y := MinY to MaxY - 1 DO begin
        if Map[x,y] < 100 then begin
            SetAPen(rp, 0);
            i := WritePixel(rp, x, y)
        end else begin
            average := (Map[x,y] - 100) DIV 6 + 1;
            if average > 15 then
            average := 15;
            SetAPen(rp, average);
            i := WritePixel(rp, x, y)
        end
        end
    end
   end else begin
    x := MinX;
    while x < MaxX do begin
        y := MinY;
        while y < MaxY do begin
        if Map[x,y] < 100 then begin
            SetAPen(rp, 0);
            RectFill(rp,x,y,x + skip - 1,y + skip - 1)
        end else begin
            average := (Map[x,y] - 100) DIV 6 + 1;
            if average > 15 then
            average := 15;
            SetAPen(rp,average);
            RectFill(rp,x,y,x + skip - 1,y + skip - 1);
        end;
        y := y + skip;
        end;
        x := x + skip;
    end;
    end;
end;

Function Min(x,y : Longint) : Longint;
begin
    if x < y then
    Min := x
    else
    Min := y;
end;

Function Max(x,y : Longint) : Longint;
begin
    if x > y then
    Max := x
    else
    Max := y;
end;


Function Height(x,y : Longint) : Longint;
begin
    Height := Map[x,y] div 32;
end;

Procedure ChangeDelta(var d : Longint);
begin
    case Random(100) of
      51..75   : if d < 1 then
             Inc(d);
      76..100  : if d > -1 then
             Dec(d);
    end;
end;

Procedure MakeRivers;
var
    i    : Longint;
    x,y,
    dx,dy  : Longint;
    OK   : Boolean;
    LastHeight : Longint;
    count1      : Longint;
    cx,cy      : Longint;
    Search     : Longint;
    CheckHeight : Longint;
begin
    SetAPen(rp, 16);

    for cx := 0 to 319 do begin
    for cy := 0 to 199 do begin
        if (Map[cx,cy] > 153) and (Map[cx,cy] < 162) and
           (Random(100) < 3) then begin

        x := cx;
        y := cy;

        dx := 0;
        dy := 0;
        while (dx = 0) and (dy = 0) do begin
            dx := Random(2) - 1;
            dy := Random(2) - 1;
        end;

        OK := True;

        count1 := 0;
        while OK do begin
            LastHeight := Map[x,y]; { Height(x,y); }
            Map[x,y] := 0;
            i := WritePixel(rp, x, y);

            CheckHeight := -6;
            Search := 0;
            repeat
                repeat
                ChangeDelta(dx);
                ChangeDelta(dy);
                until (dx <> 0) or (dy <> 0);
            Inc(Search);
            if (Map[FixX(x + dx), FixY(y + dy)] > 0) and
                         {  (Height(FixX(x + dx), FixY(y + dy)) < CheckHeight) then begin }
               (Map[FixX(x + dx), FixY(y + dy)] < (LastHeight + CheckHeight)) then begin
                x := FixX(x + dx);
                y := FixY(y + dy);
                Search := 0;
            end else if Search > 200 then begin
                if CheckHeight < 6 then begin
                Inc(CheckHeight,2);
                Search := 1;
                end else begin
                Search := 0;
                OK := False;
                end;
            end;
            until Search = 0;

            Inc(count1);
            if count1 > 150 then
            OK := False;
            if Map[x,y] < 100 then
            OK := False;
        end;
        end;
    end;
    end;
end;

Procedure MakeMap;
begin

    rp:= w^.RPort;
    vp:= ViewPortAddress(w);

    SetRGB4(vp, 0, 0, 0, 12); { Ocean Blue }
    SetRGB4(vp, 1, 1, 1, 0);
    SetRGB4(vp, 2, 0, 3, 0);
    SetRGB4(vp, 3, 0, 4, 0); { Dark Green }
    SetRGB4(vp, 4, 0, 5, 0);
    SetRGB4(vp, 5, 1, 6, 0);
    SetRGB4(vp, 6, 2, 8, 0); { Medium Green }
    SetRGB4(vp, 7, 4, 10, 0);
    SetRGB4(vp, 8, 6, 10, 0);
    SetRGB4(vp, 9, 9, 9, 0); { Brown }
    SetRGB4(vp, 10, 8, 8, 0);
    SetRGB4(vp, 11, 7, 7, 0); { Dark Brown }
    SetRGB4(vp, 12, 10, 10, 0); { Dark Grey }
    SetRGB4(vp, 13, 10, 10, 10);
    SetRGB4(vp, 14, 12, 12, 12);
    SetRGB4(vp, 15, 14, 14, 15); { White }
    SetRGB4(vp, 16, 0, 0, 10);   { River blue }

    Randomize; { Seed the Random Number Generator }

    level := 7;
    skip  := 16;

    y := MinY;
    while y < MaxY do begin
    x := MinX;
    while x < MaxX do begin
        Map[x,y] := Random(220);
        x := x + skip;
    end;
    y := y + skip;
    end;

    DrawMap;

    for level := 2 to 5 do begin
    skip := skip DIV 2;
    y := MinY;
    while y < MaxY do begin
        if (y MOD (2*skip)) = 0 then
        nexty := skip * 2
        else
        nexty:=skip;
        x := MinX;
        while x < MaxX do begin
        if (x MOD (2*skip)) = 0 then
            nextx := skip * 2
        else
            nextx := skip;
        if (nextx = skip * 2) AND (nexty = skip * 2) then begin
            average := Map[x,y] * 5;
            count1 := 9;
        end else begin
            average := 0;
            count1 := 4;
        end;
        if (nextx = skip * 2) then begin
            average := average + Map[x,FixY(y - skip)];
            average := average + Map[x,FixY(y + nexty)];
            count1 := count1 + 2;
        end;
        if (nexty = skip * 2) then begin
            average := average + Map[FixX(x - skip),y];
            average := average + Map[FixX(x + nextx),y];
            count1 := count1 + 2;
        end;
        average := average + Map[FixX(x-skip),FixY(y-skip)]
                   + Map[FixX(x-nextx),FixY(y+nexty)]
                   + Map[FixX(x+skip),FixY(y-skip)]
                   + Map[FixX(x+nextx),FixY(y+nexty)];
        average := (average DIV count1) +
                (Random(4) - 2) * (9 - level);
        case Average of
          150..255 : Average := Average + 2;
          100..149 : Inc(Average);
        else
            Average := Average - 3;
        end;
        if average < 0 then
            average := 0;
        if average > 220 then
            average := 220;
        Map[x,y] := average;

        x := x + skip;
        end;
        m := GetMsg(w^.UserPort);
        if m <> Nil then begin
        Quit := True;
        Exit;
        end;
        y := y + skip;
    end;
    DrawMap;
    end;
    MakeRivers;
end;

begin
    GfxBase := OpenLibrary(GRAPHICSNAME,0);
    if GfxBase <> nil then begin
    thetags[0] := TagItem(SA_Left,      0);
    thetags[1] := TagItem(SA_Top,       0);
    thetags[2] := TagItem(SA_Width,     320);
    thetags[3] := TagItem(SA_Height,    200);
    thetags[4] := TagItem(SA_Depth,     5);
    thetags[5] := TagItem(SA_DetailPen, 3);
    thetags[6] := TagItem(SA_BlockPen,  2);
    thetags[7] := TagItem(SA_Type,      CUSTOMSCREEN_f);
    thetags[8].ti_Tag := TAG_END;

    s := OpenScreenTagList(NIL,@thetags);

    if s <> NIL then begin

        thetags[0]  := TagItem(WA_IDCMP,        IDCMP_MOUSEBUTTONS);
        thetags[1]  := TagItem(WA_Left,         MinX);
        thetags[2]  := TagItem(WA_Top,          MinY);
        thetags[3]  := TagItem(WA_Width,        MaxX);
        thetags[4]  := TagItem(WA_Height,       MaxY);
        thetags[5]  := TagItem(WA_MinWidth,     50);
        thetags[6]  := TagItem(WA_MinHeight,    20);
        thetags[7]  := TagItem(WA_Borderless,   1);
        thetags[8]  := TagItem(WA_BackDrop,     1);
        thetags[9]  := TagItem(WA_SmartRefresh, 1);
        thetags[10] := TagItem(WA_Activate,     1);
        thetags[11] := TagItem(WA_CustomScreen, longint(s));
        thetags[12].ti_Tag := TAG_END;

        w := OpenWindowTagList(NIL,@thetags);

        IF w <> NIL THEN begin
        Quit := False;
        ShowTitle(s, 0);
        MakeMap;
        if not Quit then
            m := WaitPort(w^.UserPort);
        Forbid;
        repeat
            m := GetMsg(w^.UserPort);
        until m = nil;
        CloseWindow(w);
        Permit;
        end else
        writeln('Could not open the window.');
        CloseScreen(s);
    end else
        writeln('Could not open the screen.');
    CloseLibrary(GfxBase);
    end else writeln('no graphics.library');
end.



