Program Bezier;

{
   This program draws Bezier curves using the degree elevation
   method.  For large numbers of points (more than 10, for
   example) this is faster than the recursive way.
}

{
   Changed the source to use 2.0+.
   Looks a lot better.
   Added CloseWindowSafely.
   Made the window dynamic, it will
   adjust the size after the screen size.
   9 May 1998.

   Translated the source to fpc.
   20 Aug 1998.

   nils.sjoholm@mailbox.swipnet.se
}

uses exec, intuition, graphics, utility;

{$I tagutils.inc}

type
    PointRec = packed Record
        X, Y : Real;
    end;

Const
    w  : pWindow  = Nil;
    s  : pScreen   = Nil;
    ltrue : longint = 1;
{
    This will make the new look for screen.
    SA_Pens, Integer(pens)
}
    pens : array [0..0] of integer = (not 0);

Var
    m  : pMessage;
    rp : pRastPort;

    PointCount : Word;
    Points : Array [1..200] of PointRec;

    t, tprime : Real;

    LastX, LastY : Word;
    tags : array[0..13] of tTagItem;

Procedure CleanUpAndDie;
begin
    if w <> Nil then CloseWindow(w);
    if s <> Nil then CloseScreen(s);
    if Gfxbase <> nil then CloseLibrary(GfxBase);
    Halt(0);
end;

Procedure DrawLine;
begin
    Move(rp, Trunc(Points[PointCount].X), Trunc(Points[PointCount].Y));
    Draw(rp, LastX, LastY);
end;

Procedure GetPoints;
var
    LastSeconds,
    LastMicros  : Longint;
    IM : pIntuiMessage;
    StoreMsg : tIntuiMessage;
    Leave : Boolean;
    OutOfBounds : Boolean;
    BorderLeft, BorderRight,
    BorderTop, BorderBottom : Word;
    dummy : Boolean;

    Procedure AddPoint;
    begin
    Inc(PointCount);
    with Points[PointCount] do begin
        X := Real(StoreMsg.MouseX);
        Y := Real(StoreMsg.MouseY);
    end;
    with StoreMsg do begin
        LastX := MouseX;
        LastY := MouseY;
        LastSeconds := Seconds;
        LastMicros := Micros;
    end;
    SetAPen(rp, 2);
    SetDrMd(rp, JAM1);
    DrawEllipse(rp, LastX, LastY, 5, 3);
    SetAPen(rp, 3);
    SetDrMd(rp, COMPLEMENT);
    DrawLine;
    end;

    Function CheckForExit : Boolean;
    {   This function determines whether the user wanted to stop
    entering points.  I added the position tests because my
    doubleclick time is too long, and I was too lazy to dig
    out Preferences to change it. }
    begin
    with StoreMsg do
        CheckForExit := DoubleClick(LastSeconds, LastMicros,
                    Seconds, Micros) and
                (Abs(MouseX - Trunc(Points[PointCount].X)) < 5) and
                (Abs(MouseY - TRunc(Points[PointCount].Y)) < 3);
    end;

    Procedure ClearIt;
    {  This just clears the screen when you enter your first point }
    begin
    SetDrMd(rp, JAM1);
    SetAPen(rp, 0);
    RectFill(rp, BorderLeft, BorderTop,
             BorderRight, BorderBottom);
    SetDrMd(rp, COMPLEMENT);
    SetAPen(rp, 3);
    end;

begin
    dummy := ModifyIDCMP(w, IDCMP_CLOSEWINDOW or IDCMP_MOUSEBUTTONS or IDCMP_MOUSEMOVE);
    SetDrMd(rp, COMPLEMENT);
    PointCount := 0;
    Leave := False;
    OutOfBounds := False;
    BorderLeft := w^.BorderLeft;
    BorderRight := (w^.Width - w^.BorderRight) -1;
    BorderTop := w^.BorderTop;
    BorderBottom := (w^.Height - w^.BorderBottom) -1;
    repeat
        IM := pIntuiMessage(WaitPort(w^.UserPort));
        IM := pIntuiMessage(GetMsg(w^.UserPort));
        StoreMsg := IM^;
        ReplyMsg(pMessage(IM));
        case StoreMsg.IClass of
           IDCMP_MOUSEMOVE : if PointCount > 0 then begin
                 if not OutOfBounds then
                 DrawLine;
                     LastX := StoreMsg.MouseX;
                     LastY := StoreMsg.MouseY;
                 if (LastX > BorderLeft) and
                (LastX < BorderRight) and
                (LastY > BorderTop) and
                (LastY < BorderBottom) then begin
                 DrawLine;
                 OutOfBounds := False;
                 end else
                 OutOfBounds := True;
                 end;
           IDCMP_MOUSEBUTTONS : if StoreMsg.Code = SELECTUP then begin
                    if PointCount > 0 then
                    Leave := CheckForExit
                else
                    ClearIt;
                    if (not Leave) and (not OutOfBounds) then
                    AddPoint;
                    end;
           IDCMP_CLOSEWINDOW : CleanUpAndDie;
        end;
    until Leave or (PointCount > 50);
    if not Leave then
        DrawLine;
    dummy := ModifyIDCMP(w, IDCMP_CLOSEWINDOW);
    SetDrMd(rp, JAM1);
    SetAPen(rp, 1);
end;

Procedure Elevate;
var
    t, tprime,
    RealPoints : Real;
    i : Integer;
begin
    Inc(PointCount);
    RealPoints := Real(PointCount);
    Points[PointCount] := Points[Pred(PointCount)];
    for i := Pred(PointCount) downto 2 do
    with Points[i] do begin
        t := Real(i) / RealPoints;
        tprime := 1.0 - t;
        X := t * Points[Pred(i)].X + tprime * X;
        Y := t * Points[Pred(i)].Y + tprime * Y;
    end;
end;

Procedure DrawCurve;
var
    i : Integer;
begin
    Move(rp, Trunc(Points[1].X), Trunc(Points[1].Y));
    for i := 2 to PointCount do
    Draw(rp, Round(Points[i].X), Round(Points[i].Y));
end;

Procedure DrawBezier;
var
    i : Word;
begin
    SetAPen(rp, 2);
    while PointCount < 100 do begin
    Elevate;
    DrawCurve;
    if GetMsg(w^.UserPort) <> Nil then
        CleanUpAndDie;
    end;
    SetAPen(rp, 1);
    DrawCurve;
end;

begin
   GfxBase := OpenLibrary(GRAPHICSNAME,37);

                       tags[0] := TagItem(SA_Pens,      Long(@pens));
                       tags[1] := TagItem(SA_Depth,     2);
                       tags[2] := TagItem(SA_DisplayID, HIRES_KEY);
                       tags[3] := TagItem(SA_Title,     Long(PChar('Simple Bezier Curves'#0)));
                       tags[4].ti_Tag := TAG_END;
    s := OpenScreenTagList(nil, @tags);
    if s = NIL then CleanUpAndDie;

                        tags[0] := TagItem(WA_IDCMP,        IDCMP_CLOSEWINDOW);
                        tags[1] := TagItem(WA_Left,         0);
                        tags[2] := TagItem(WA_Top,          s^.BarHeight +1);
                        tags[3] := TagItem(WA_Width,        s^.Width);
                        tags[4] := TagItem(WA_Height,       s^.Height - (s^.BarHeight + 1));
                        tags[5] := TagItem(WA_DepthGadget,  ltrue);
                        tags[6] := TagItem(WA_DragBar,      ltrue);
                        tags[7] := TagItem(WA_CloseGadget,  ltrue);
                        tags[8] := TagItem(WA_ReportMouse,  ltrue);
                        tags[9] := TagItem(WA_SmartRefresh, ltrue);
                        tags[10] := TagItem(WA_Activate,     ltrue);
                        tags[11] := TagItem(WA_Title,        long(PChar('Close the Window to Quit'#0)));
                        tags[12] := TagItem(WA_CustomScreen, long(s));
                        tags[13].ti_Tag := TAG_END;
    w := OpenWindowTagList(nil, @tags);
    IF w=NIL THEN CleanUpAndDie;

    rp := w^.RPort;
    Move(rp, 252, 20);
    Text(rp, PChar('Enter points by pressing the left mouse button'#0), 46);
    Move(rp, 252, 30);
    Text(rp, PChar('Double click on the last point to begin drawing'#0), 47);
    repeat
        GetPoints;  { Both these routines will quit if }
        DrawBezier; { the window is closed. }
    until False;
    CleanUpAndDie;
end.
