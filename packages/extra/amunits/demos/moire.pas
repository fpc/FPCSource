Program Moire;

{
      Will now open a default screen (can be any size) with
      the new look. The window get it's size depending on
      the screen size.
      14 May 1998

      Translated to FPC from PCQ Pascal.
      15 Aug 1998.
      nils.sjoholm@mailbox.swipnet.se
}

uses Exec, Intuition, Graphics, Utility;

{$I tagutils.inc}

const
    pens : array [0..0] of Integer = ( not 0);
    ltrue = 1;

var
    w  : pWindow;
    s  : pScreen;
    m  : pMessage;
    thetags : array[0..17] of tTagItem;


Procedure DoDrawing(RP : pRastPort);
var
    x  : word;
    Pen : Byte;
    Stop : word;
begin
    Pen := 1;
    while true do begin
    with w^ do begin
        x := 0;
        while x < Pred(Width - BorderRight - BorderLeft) do begin
        Stop := Pred(Width - BorderRight);
        SetAPen(RP, Pen);
        Move(RP, Succ(x + BorderLeft), BorderTop);
        Draw(RP, Stop - x, Pred(Height - BorderBottom));
        Pen := (Pen + 1) mod 4;
        Inc(x);
        end;
        m := GetMsg(UserPort);
        if m <> Nil then
        Exit;
        x := 0;
        while x < Pred(Height - BorderBottom - BorderTop) do begin
        Stop := Pred(Height - BorderBottom);
        SetAPen(RP, Pen);
        Move(RP, Pred(Width - BorderRight), Succ(x + BorderTop));
        Draw(RP, Succ(BorderLeft), Stop - x);
        Pen := (Pen + 1) mod 4;
        Inc(x);
        end;
        m := GetMsg(UserPort);
        if m <> Nil then
        Exit;
    end;
    end;
end;

begin
    { Note that the startup code of all FPC programs depends on
      Intuition, so if we got to this point Intuition must be
      open, so the run time library just uses the pointer that
      the startup code created.  Same with DOS, although we don't
      use that here. }

    GfxBase := OpenLibrary(GRAPHICSNAME,0);
    if GfxBase <> nil then begin

    thetags[0] := TagItem(SA_Pens,      longint(@pens));
    thetags[1] := TagItem(SA_Depth,     2);
    thetags[2] := TagItem(SA_DisplayID, HIRES_KEY);
    thetags[3] := TagItem(SA_Title,     Long(PChar('Close the Window to End This Demonstration'#0)));
    thetags[4].ti_Tag := TAG_END;

    s := OpenScreenTagList(NIL, @thetags);
    if s <> NIL then begin

    thetags[0] := TagItem(WA_IDCMP,        IDCMP_CLOSEWINDOW);
    thetags[1] := TagItem(WA_Left,         20);
    thetags[2] := TagItem(WA_Top,          50);
    thetags[3] := TagItem(WA_Width,        336);
    thetags[4] := TagItem(WA_Height,       100);
    thetags[5] := TagItem(WA_MinWidth,     50);
    thetags[6] := TagItem(WA_MinHeight,    20);
    thetags[7] := TagItem(WA_MaxWidth,     -1);
    thetags[8] := TagItem(WA_MaxHeight,    -1);
    thetags[9] := TagItem(WA_DepthGadget,  ltrue);
    thetags[10] := TagItem(WA_DragBar,      -1);
    thetags[11] := TagItem(WA_CloseGadget,  -1);
    thetags[12] := TagItem(WA_SizeGadget,   -1);
    thetags[13] := TagItem(WA_SmartRefresh, -1);
    thetags[14] := TagItem(WA_Activate,     -1);
    thetags[15] := TagItem(WA_Title,        Long(PChar('Feel Free to Re-Size the Window'#0)));
    thetags[16] := TagItem(WA_CustomScreen, Long(s));
    thetags[17].ti_Tag := TAG_END;

    w := OpenWindowTagList(NIL, @thetags);
    IF w <> NIL THEN begin

        DoDrawing(w^.RPort);
        Forbid;
        repeat
            m := GetMsg(w^.UserPort);
        until m = nil;
        CloseWindow(w);
        Permit;
        end else
        writeln('Could not open the window');
        CloseScreen(s);
    end else
        writeln('Could not open the screen.');
    CloseLibrary(GfxBase);
    end else writeln('no graphics.library');
end.

