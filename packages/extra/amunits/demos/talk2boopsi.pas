PROGRAM Talk2Boopsi;

{ This example creates a Boopsi prop gadget and integer string gadget, connecting them so they }
{ update each other when the user changes their value.  The example program only initializes   }
{ the gadgets and puts them on the window; it doesn't have to interact with them to make them  }
{ talk to each other.                                                                          }

uses Exec, Intuition, Utility;

{$I tagutils.inc}

VAR
   w      : pWindow;
   mymsg  : pIntuiMessage;
   prop,
   int    : pGadget;
   done   : BOOLEAN;
   dummy  : Word;
   temp   : Longint;
   thetags : array[0..11] of tTagItem;
   prop2intmap : array[0..1] of tTagItem;
   int2propmap : array[0..1] of tTagItem;

CONST

   vers  : PChar = '$VER: Talk2boopsi 37.1';

    PROPGADGET_ID       = 1;
   INTGADGET_ID        = 2;
   PROPGADGETWIDTH     = 10;
   PROPGADGETHEIGHT    = 80;
   INTGADGETHEIGHT     = 18;
   VISIBLE             = 10;
   TOTAL               = 100;
   INITIALVAL          = 25;
   MINWINDOWWIDTH      = 80;
   MINWINDOWHEIGHT     = (PROPGADGETHEIGHT + 70);
   MAXCHARS            = 3;

PROCEDURE CleanUp(Why : STRING; err: Word);
BEGIN
    IF prop <> NIL THEN DisposeObject(prop);
    IF int <> NIL THEN DisposeObject(int);
    IF w <> NIL THEN CloseWindow(w);
    IF Why <> '' THEN WriteLN(Why);
    Halt(err);
END;

BEGIN

    done := FALSE;

    prop2intmap[0] := TagItem(PGA_Top, STRINGA_LongVal);
    prop2intmap[1].ti_Tag := TAG_END;

    int2propmap[0] := TagItem(STRINGA_LongVal, PGA_Top);
    int2propmap[1].ti_Tag := TAG_END;

    thetags[0] := TagItem(WA_Flags,     WFLG_DEPTHGADGET + WFLG_DRAGBAR +
                               WFLG_CLOSEGADGET + WFLG_SIZEGADGET + WFLG_ACTIVATE);
    thetags[1] := TagItem(WA_IDCMP,     IDCMP_CLOSEWINDOW);
    thetags[2] := TagItem(WA_Width,     MINWINDOWWIDTH + 10);
    thetags[3] := TagItem(WA_Height,    MINWINDOWHEIGHT + 10);
    thetags[4] := TagItem(WA_MinWidth,  MINWINDOWWIDTH);
    thetags[5] := TagItem(WA_MinHeight, MINWINDOWHEIGHT);
    thetags[6].ti_Tag := TAG_END;

    w := OpenWindowTagList(NIL,@thetags);

    IF w=NIL THEN CleanUp('No window',20);

    thetags[0] := TagItem(GA_ID,       PROPGADGET_ID);
    thetags[1] := TagItem(GA_Top,      (w^.BorderTop) + 5);
    thetags[2] := TagItem(GA_Left,     (w^.BorderLeft) + 5);
    thetags[3] := TagItem(GA_Width,    PROPGADGETWIDTH);
    thetags[4] := TagItem(GA_Height,   PROPGADGETHEIGHT);
    thetags[5] := TagItem(ICA_MAP,     Longint(@prop2intmap));
    thetags[6] := TagItem(PGA_Total,   TOTAL);
    thetags[7] := TagItem(PGA_Top,     INITIALVAL);
    thetags[8] := TagItem(PGA_Visible, VISIBLE);
    thetags[9] := TagItem(PGA_NewLook, 1); { true }
    thetags[10].ti_Tag := TAG_END;

    prop := NewObjectA(NIL, PChar('propgclass'#0),@thetags);

    IF prop = NIL THEN CleanUp('No propgadget',20);


    thetags[0] := TagItem(GA_ID,      INTGADGET_ID);
    thetags[2] := TagItem(GA_Top,     (w^.BorderTop) + 5);
    thetags[3] := TagItem(GA_Left,    (w^.BorderLeft) + PROPGADGETWIDTH + 10);
    thetags[4] := TagItem(GA_Width,   MINWINDOWWIDTH -
                                  (w^.BorderLeft + w^.BorderRight +
                                  PROPGADGETWIDTH + 15));
    thetags[5] := TagItem(GA_Height,  INTGADGETHEIGHT);

    thetags[6] := TagItem(ICA_MAP,    Longint(@int2propmap));
    thetags[7] := TagItem(ICA_TARGET, Longint(prop));
    thetags[8] := TagItem(GA_Previous, Longint(prop));

    thetags[9] := TagItem(STRINGA_LongVal,  INITIALVAL);
    thetags[10] := TagItem(STRINGA_MaxChars, MAXCHARS);
    thetags[11].ti_Tag := TAG_END;

    int := NewObjectA(NIL, PChar('strgclass'#0),@thetags);

    thetags[0] := TagItem(ICA_TARGET, Longint(int));
    thetags[1].ti_Tag := TAG_END;

    temp := SetGadgetAttrsA(prop, w, NIL,@thetags);

    IF int = NIL THEN CleanUp('No INTEGER gadget',20);

    dummy := AddGList(w, prop, -1, -1, NIL);
    RefreshGList(prop, w, NIL, -1);

    WHILE (NOT done) DO BEGIN
        mymsg := pIntuiMessage(WaitPort(W^.UserPort));
        mymsg := pIntuiMessage(GetMsg(W^.UserPort));
        IF mymsg^.IClass = IDCMP_CLOSEWINDOW THEN done := True;
        ReplyMsg(pMessage(mymsg));
    END;

    dummy := RemoveGList(w, prop, -1);
    CleanUp('',0);
END.





