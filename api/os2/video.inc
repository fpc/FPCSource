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

const
    InitVideoCalled: boolean = false;
    OrigEmpty: boolean = false;
    LastCursorType: word = crUnderline;
    EmptyCell: cardinal = $0720;

{$IFDEF PPC_VIRTUAL}
type
    TVioCursorInfo = VioCursorInfo;
    TVioModeInfo = VioModeInfo;
    TVioIntensity = VioIntensity;
{$ENDIF}

var OrigCurType: TVioCursorInfo;
    OrigVioMode: TVioModeInfo;
    OrigHighBit: TVioIntensity;
    CellHeight: byte;


{$ASMMODE ATT}

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
    RegisterVideoMode (40, 25, True, @DefaultVideoModeSelector, 0);
    RegisterVideoMode (80, 25, True, @DefaultVideoModeSelector, 0);
    RegisterVideoMode (80, 30, True, @DefaultVideoModeSelector, 0);
    RegisterVideoMode (80, 43, True, @DefaultVideoModeSelector, 0);
    RegisterVideoMode (80, 50, True, @DefaultVideoModeSelector, 0);

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

var P: PVideoModeList;
    MI: TVioModeInfo;

begin
    InitVideoCalled := true;
    VideoBufSize := 0;
    MI.cb := SizeOf (MI);
    VioGetMode (MI, 0);
    if OrigEmpty then
        begin
{Remember original video mode, cursor type and high bit behaviour setting}
            Move (MI, OrigVioMode, SizeOf (OrigVioMode));
            VioGetCurType (OrigCurType, 0);
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
                                                 @DefaultVideoModeSelector, 0);
                end;
        end;
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
    if VioGetBuf (VideoBuf, PWord (VideoBufSize)^, 0) = 0 then
        begin
            VideoBuf := SelToFlat (TFarPtr (VideoBuf));
            SetHighBitBlink (true);
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
                        yStart := word (-90);
                        cEnd := word (-100);
                    end;
                crHalfBlock:
                    begin
                        yStart := word (-50);
                        cEnd := word (-100);
                    end;
                crBlock:
                    begin
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
            VideoBufSize := 0;
            InitVideoCalled := false;
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
                    fbType := 0;
                    if VideoMode.Color then
                        Color := Colors_16
                    else
                        Color := Colors_2;
                    Col := VideoMode.Col;
                    Row := VideoMode.Row;
                end;
            if VioSetMode (MI, 0) = 0 then
                if VioGetBuf (VideoBuf, PWord (VideoBufSize)^, 0) = 0 then
                    begin
                        VideoBuf := SelToFlat (TFarPtr (VideoBuf));
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
                        VioGetBuf (VideoBuf, PWord (VideoBufSize)^, 0);
                        VideoBuf := SelToFlat (TFarPtr (VideoBuf));
                        SetHighBitBlink (true);
                        CheckCellHeight;
                        SetCursorType (LastCursorType);
                        ClearScreen;
                    end
            else
                begin
                    DefaultVideoModeSelector := false;
                    VioGetBuf (VideoBuf, PWord (VideoBufSize)^, 0);
                    VideoBuf := SelToFlat (TFarPtr (VideoBuf));
                    SetHighBitBlink (true);
                    SetCursorType (LastCursorType);
                end;
        end;
end;


procedure ClearScreen;

begin
    VioScrollDown (0, 0, word (-1), word (-1), 0, PWord (@EmptyCell)^, 0);
end;


procedure UpdateScreen (Force: boolean);
begin
    VioShowBuf (0, VideoBufSize, 0);
end;


{

  $Log$
  Revision 1.3  2000-09-24 19:53:03  hajny
    * OS/2 implementation almost finished, not debugged yet

  Revision 1.2  2000/07/13 11:32:26  michael
  + removed logs
 
}
