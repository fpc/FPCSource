{
  System independent low-level video interface for OS/2

  $Id$

}

uses
{$IFDEF PPC_FPC}
 DosCalls, VioCalls;
{$ELSE}
 {$IFDEF PPC_VIRTUAL}
 Os2Base;
 {$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
type
    cardinal = longint;
{$ENDIF}

const
    InitVideoCalled: boolean = false;
    LastCursorType: word = crUnderline;
    EmptyCell: cardinal = $0720;
    OrigScreen: PVideoBuf = nil;
    OrigScreenSize: cardinal = 0;

{$IFDEF PPC_VIRTUAL}
type
    TVioCursorInfo = VioCursorInfo;
    TVioModeInfo = VioModeInfo;
    TVioIntensity = VioIntensity;
{$ENDIF}

var OrigCurType: TVioCursorInfo;
    OrigVioMode: TVioModeInfo;
    OrigHighBit: TVioIntensity;
    OrigCurRow: word;
    OrigCurCol: word;
    CellHeight: byte;
    OldVideoBuf: PVideoBuf;

procedure TargetEntry;

var P: PVideoModeList;
    PScr: pointer;

begin
{Remember original video mode, cursor type and high bit behaviour setting}
    OrigVioMode.cb := SizeOf (OrigVioMode);
    VioGetMode (OrigVioMode, 0);
    VioGetCurType (OrigCurType, 0);
    VioGetCurPos (OrigCurRow, OrigCurCol, 0);
    with OrigHighBit do
        begin
            cb := 6;
            rType := 2;
        end;
    VioGetState (OrigHighBit, 0);
{Register the curent video mode in Modes if not there yet}
    with OrigVioMode do
        begin
            P := Modes;
            while (P <> nil) and ((P^.Row <> Row) or (P^.Col <> Col)
                                      or (P^.Color <> (Color >= Colors_16))) do
                P := P^.Next;
            if P = nil then
{Assume we have at least 16 colours available in "colour" modes}
                RegisterVideoMode (Col, Row, Color >= Colors_16,
{$IFDEF FPC}
                                                 @DefaultVideoModeSelector, 0);
{$ELSE}
                                                  DefaultVideoModeSelector, 0);
{$ENDIF}
        end;
{Get the address of the original videobuffer and size.}
    if VioGetBuf (PScr, PWord (@OrigScreenSize)^, 0) = 0 then
        begin
{$IFDEF BIT_32}
 {$IFDEF PPC_VIRTUAL}
            SelToFlat (PScr);
 {$ELSE}
            PScr := SelToFlat (TFarPtr (PScr));
 {$ENDIF}
{$ENDIF}
            GetMem (OrigScreen, OrigScreenSize);
            Move (PScr^, OrigScreen^, OrigScreenSize);
        end;
end;

procedure TargetExit;

begin
end;


procedure CheckCellHeight;

var OldCD, CD: TVioCursorInfo;

begin
    VioGetCurType (OldCD, 0);
    Move (OldCD, CD, SizeOf (CD));
    with CD do
        begin
            Attr := 0;
            yStart := word (-90);
            cEnd := word (-100);
        end;
    VioSetCurType (CD, 0);
    VioGetCurType (CD, 0);
    CellHeight := CD.cEnd;
    VioSetCurType (OldCD, 0);
end;


procedure RegisterVideoModes;
begin
{ BW modes are rejected on my (colour) configuration. I can't imagine
  OS/2 running on MCGA anyway... ;-)

    RegisterVideoMode (40, 25, False, @DefaultVideoModeSelector, 0);
    RegisterVideoMode (80, 25, False, @DefaultVideoModeSelector, 0);
}
{$IFDEF FPC}
    RegisterVideoMode (40, 25, True, @DefaultVideoModeSelector, 0);
    RegisterVideoMode (80, 25, True, @DefaultVideoModeSelector, 0);
    RegisterVideoMode (80, 30, True, @DefaultVideoModeSelector, 0);
    RegisterVideoMode (80, 43, True, @DefaultVideoModeSelector, 0);
    RegisterVideoMode (80, 50, True, @DefaultVideoModeSelector, 0);
{$ELSE}
    RegisterVideoMode (40, 25, True, DefaultVideoModeSelector, 0);
    RegisterVideoMode (80, 25, True, DefaultVideoModeSelector, 0);
    RegisterVideoMode (80, 30, True, DefaultVideoModeSelector, 0);
    RegisterVideoMode (80, 43, True, DefaultVideoModeSelector, 0);
    RegisterVideoMode (80, 50, True, DefaultVideoModeSelector, 0);
{$ENDIF}

{ The following modes wouldn't work on plain VGA; is it useful to check
  for their availability on the program startup?

    RegisterVideoMode (132, 25, True, @DefaultVideoModeSelector, 0);
    RegisterVideoMode (132, 30, True, @DefaultVideoModeSelector, 0);
    RegisterVideoMode (132, 43, True, @DefaultVideoModeSelector, 0);
    RegisterVideoMode (132, 50, True, @DefaultVideoModeSelector, 0);
}
end;


procedure SetHighBitBlink (Blink: boolean);

var VI: TVioIntensity;

begin
    with VI do
        begin
            cb := 6;
            rType := 2;
            fs := byte (not (Blink));
        end;
    VioSetState (VI, 0);
end;


procedure InitVideo;

var MI: TVioModeInfo;

begin
    if InitVideoCalled then
        FreeMem (OldVideoBuf, VideoBufSize);
    OldVideoBuf := nil;
    InitVideoCalled := true;
    VideoBufSize := 0;
    MI.cb := SizeOf (MI);
    VioGetMode (MI, 0);
    with MI do
        begin
            ScreenWidth := Col;
            ScreenHeight := Row;
            ScreenColor := Color >= Colors_16;
        end;
    VioGetCurPos (CursorY, CursorX, 0);
    LowAscii := true;
    SetCursorType (LastCursorType);
{Get the address of the videobuffer.}
{$IFDEF PPC_VIRTUAL}
    if VioGetBuf (pointer (VideoBuf), PWord (@VideoBufSize)^, 0) = 0 then
        begin
            SelToFlat (pointer (VideoBuf));
{$ELSE}
    if VioGetBuf (VideoBuf, PWord (@VideoBufSize)^, 0) = 0 then
        begin
 {$IFDEF BIT_32}
            VideoBuf := SelToFlat (TFarPtr (VideoBuf));
 {$ENDIF}
{$ENDIF}
            SetHighBitBlink (true);
            GetMem (OldVideoBuf, VideoBufSize);
            Move (VideoBuf^, OldVideoBuf^, VideoBufSize);
        end
    else
        ErrorHandler (errVioInit, nil);
end;


procedure SetCursorPos (NewCursorX, NewCursorY: word);

begin
    if VioSetCurPos (NewCursorY, NewCursorX, 0) = 0 then
        begin
            CursorX := NewCursorX;
            CursorY := NewCursorY;
        end
    else
        {Do not set an error code; people should fix invalid NewCursorX
         or NewCursorY values when designing, there is no need for detecting
         these errors at runtime.}
        RunError (225);
end;


function GetCursorType: word;

var CD: TVioCursorInfo;

begin
    VioGetCurType (CD, 0);    {Never fails, because handle is default handle.}
    with CD do
        begin
            CursorLines := Succ (cEnd) - yStart;
            if Attr = word (-1) then
                GetCursorType := crHidden
            else
{Because the cursor's start and end lines are returned, we'll have
 to guess heuristically what cursor type we have.}
                if CursorLines = 0 then
{Probably this does not occur, but you'll never know.}
                    GetCursorType := crHidden
                else if CursorLines <= Succ (CellHeight div 4) then
                    GetCursorType := crUnderline
                else if CursorLines <= Succ (CellHeight div 2) then
                    GetCursorType := crHalfBlock
                else
                    GetCursorType := crBlock;
        end;
end;


procedure SetCursorType (NewType: word);

var CD: TVioCursorInfo;

begin
    VioGetCurType (CD, 0);
    with CD do
        begin
            case NewType of
                crHidden: Attr := word (-1);
                crUnderline:
                    begin
                        Attr := 0;
                        yStart := word (-90);
                        cEnd := word (-100);
                    end;
                crHalfBlock:
                    begin
                        Attr := 0;
                        yStart := word (-50);
                        cEnd := word (-100);
                    end;
                crBlock:
                    begin
                        Attr := 0;
                        yStart := 0;
                        cEnd := word (-100);
                    end;
            end;
            VioSetCurType (CD, 0);
            VioGetCurType (CD, 0);
            CursorLines := Succ (cEnd) - yStart;
        end;
end;


procedure DoneVideo;

var PScr: pointer;
    ScrSize: cardinal;

begin
    if InitVideoCalled then
        begin
            LastCursorType := GetCursorType;
            ClearScreen;
{Restore original settings}
            VioSetMode (OrigVioMode, 0);
            CheckCellHeight;
{Set CursorX and CursorY}
            SetCursorPos (0, 0);
            VioSetState (OrigHighBit, 0);
            VioSetCurType (OrigCurType, 0);
            VioSetCurPos (OrigCurRow, OrigCurCol, 0);
            FreeMem (OldVideoBuf, VideoBufSize);
            OldVideoBuf := nil;
            VideoBufSize := 0;
            InitVideoCalled := false;
            if (OrigScreenSize <> 0) and (OrigScreen <> nil) then
                begin
                    ScrSize := 0;
                    if (VioGetBuf (PScr, PWord (@ScrSize)^, 0) = 0)
                                            and (ScrSize = OrigScreenSize) then
                        begin
{$IFDEF BIT_32}
 {$IFDEF PPC_VIRTUAL}
                            SelToFlat (PScr);
 {$ELSE}
                            PScr := SelToFlat (TFarPtr (PScr));
 {$ENDIF}
{$ENDIF}
                            Move (OrigScreen^, PScr^, OrigScreenSize);
                            VioShowBuf (0, ScrSize, 0);
                        end;
                end;
        end;
end;


function GetCapabilities: word;

begin
    GetCapabilities := $3F;
end;


function DefaultVideoModeSelector (const VideoMode: TVideoMode; Params: longint): boolean;

var OldMI, MI: TVioModeInfo;

begin
    OldMI.cb := SizeOf (OldMI);
    if VioGetMode (OldMI, 0) <> 0 then
        DefaultVideoModeSelector := false
    else
        begin
            with MI do
                begin
                    cb := 8;
                    fbType := 1;
                    if VideoMode.Color then
                        Color := Colors_16
                    else
                        Color := Colors_2;
                    Col := VideoMode.Col;
                    Row := VideoMode.Row;
                end;
            if VioSetMode (MI, 0) = 0 then
{$IFDEF PPC_VIRTUAL}
                if VioGetBuf (pointer (VideoBuf),
                                            PWord (@VideoBufSize)^, 0) = 0 then
                    begin
                        SelToFlat (pointer (VideoBuf));
{$ELSE}
                if VioGetBuf (VideoBuf, PWord (@VideoBufSize)^, 0) = 0 then
                    begin
 {$IFDEF BIT_32}
                        VideoBuf := SelToFlat (TFarPtr (VideoBuf));
 {$ENDIF}
{$ENDIF}
                        DefaultVideoModeSelector := true;
                        SetHighBitBlink (true);
                        CheckCellHeight;
                        SetCursorType (LastCursorType);
                        ClearScreen;
                    end
                else
                    begin
                        DefaultVideoModeSelector := false;
                        VioSetMode (OldMI, 0);
{$IFDEF PPC_VIRTUAL}
                        VioGetBuf (pointer (VideoBuf),
                                                    PWord (@VideoBufSize)^, 0);
                        SelToFlat (pointer (VideoBuf));
{$ELSE}
                        VioGetBuf (VideoBuf, PWord (@VideoBufSize)^, 0);
 {$IFDEF BIT_32}
                        VideoBuf := SelToFlat (TFarPtr (VideoBuf));
 {$ENDIF}
{$ENDIF}
                        SetHighBitBlink (true);
                        CheckCellHeight;
                        SetCursorType (LastCursorType);
                        ClearScreen;
                    end
            else
                begin
                    DefaultVideoModeSelector := false;
{$IFDEF PPC_VIRTUAL}
                    VioGetBuf (pointer (VideoBuf), PWord (@VideoBufSize)^, 0);
                    SelToFlat (pointer (VideoBuf));
{$ELSE}
                    VioGetBuf (VideoBuf, PWord (@VideoBufSize)^, 0);
 {$IFDEF BIT_32}
                    VideoBuf := SelToFlat (TFarPtr (VideoBuf));
 {$ENDIF}
{$ENDIF}
                    SetHighBitBlink (true);
                    SetCursorType (LastCursorType);
                end;
        end;
end;


procedure ClearScreen;

begin
    VioScrollDn (0, 0, word (-1), word (-1), word (-1), PWord (@EmptyCell)^, 0);
    Move (VideoBuf^, OldVideoBuf^, VideoBufSize);
end;


{$IFDEF PPC_FPC}
 {$ASMMODE INTEL}
{$ENDIF}

procedure UpdateScreen (Force: boolean);

{$IFDEF BIT_32}
var SOfs: cardinal;
{$ELSE}
var SOfs: word;
{$ENDIF}

begin
    if LockUpdateScreen = 0 then
        begin
            if not (Force) then
                begin
{$IFDEF BIT_32}
                    asm
                        mov esi, VideoBuf
                        mov edi, OldVideoBuf
                        mov ecx, VideoBufSize
                        shr ecx
                        shr ecx
                        repe
                        cmpsd
                        mov SOfs, ecx
                        or ecx, ecx
                        jz @no_update
                        mov Force, 1
@no_update:
                    end;
                    Inc (SOfs);
                    SOfs := VideoBufSize - (SOfs shl 2);
{$ELSE}
                    asm
                        push ds
                        lds si, VideoBuf
                        les di, OldVideoBuf
                        mov cx, word ptr VideoBufSize
                        shr cx
                        repe
                        cmpsw
                        mov SOfs, cx
                        or cx, cx
                        jz @no_update
                        mov Force, 1
@no_update:
                        pop ds
                    end;
                    Inc (SOfs);
                    SOfs := VideoBufSize - (SOfs shl 1);
{$ENDIF}
                end else
                    SOfs := 0;
            if Force then
                begin
                    VioShowBuf (SOfs, VideoBufSize - SOfs, 0);
                    Move (VideoBuf^, OldVideoBuf^, VideoBufSize);
                end;
        end;
end;


{

  $Log$
  Revision 1.10  2000-10-11 20:10:04  hajny
    * compatibility enhancements

  Revision 1.9  2000/10/11 05:28:29  hajny
    * really a faster version now ;-)

  Revision 1.8  2000/10/10 20:28:18  hajny
    * screen updates speeded up

  Revision 1.7  2000/10/08 18:40:58  hajny
    * SetCursorType corrected

  Revision 1.6  2000/10/08 14:13:19  hajny
    * ClearScreen correction, screen restored on exit

  Revision 1.5  2000/10/04 11:53:31  pierre
   Add TargetEntry and TargetExit (merged)

  Revision 1.4  2000/09/26 18:15:29  hajny
    + working with VP/2 already (not FPC yet)!

  Revision 1.3  2000/09/24 19:53:03  hajny
    * OS/2 implementation almost finished, not debugged yet

  Revision 1.2  2000/07/13 11:32:26  michael
  + removed logs

}
