{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Video unit for OS/2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Video;

interface

{$i videoh.inc}

implementation

uses
  DosCalls, VioCalls;

{$i video.inc}


const
    InitVideoCalled: boolean = false;
    LastCursorType: word = crUnderline;
    EmptyCell: cardinal = $0720;
    OrigScreen: PVideoBuf = nil;
    OrigScreenSize: cardinal = 0;

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
                                                @DefaultVideoModeSelector, 0);
        end;
{Get the address of the original videobuffer and size.}
    if VioGetBuf (PScr, PWord (@OrigScreenSize)^, 0) = 0 then
        begin
            PScr := SelToFlat (TFarPtr (PScr));
            GetMem (OrigScreen, OrigScreenSize);
            Move (PScr^, OrigScreen^, OrigScreenSize);
        end;
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
    if VioGetBuf (VideoBuf, PWord (@VideoBufSize)^, 0) = 0 then
        begin
            VideoBuf := SelToFlat (TFarPtr (VideoBuf));
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
                            PScr := SelToFlat (TFarPtr (PScr));
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
                if VioGetBuf (VideoBuf, PWord (@VideoBufSize)^, 0) = 0 then
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
                        VioGetBuf (VideoBuf, PWord (@VideoBufSize)^, 0);
                        VideoBuf := SelToFlat (TFarPtr (VideoBuf));
                        SetHighBitBlink (true);
                        CheckCellHeight;
                        SetCursorType (LastCursorType);
                        ClearScreen;
                    end
            else
                begin
                    DefaultVideoModeSelector := false;
                    VioGetBuf (VideoBuf, PWord (@VideoBufSize)^, 0);
                    VideoBuf := SelToFlat (TFarPtr (VideoBuf));
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


{$ASMMODE INTEL}

procedure UpdateScreen (Force: boolean);

var SOfs, CLen: cardinal;

begin
    if LockUpdateScreen = 0 then
        begin
            if not (Force) then
                begin
                    asm
                        cld
                        mov esi, VideoBuf
                        mov edi, OldVideoBuf
                        mov eax, VideoBufSize
                        mov ecx, eax
                        shr ecx
                        shr ecx
                        repe
                        cmpsd
                        je @no_update
                        inc cx
                        mov SOfs, ecx
                        mov Force, 1
                        std
                        mov edi, eax
                        mov esi, VideoBuf
                        add eax, esi
                        sub eax, 4
                        mov esi, eax
                        mov eax, OldVideoBuf
                        add eax, edi
                        sub eax, 4
                        mov edi, eax
                        repe
                        cmpsd
                        inc ecx
                        shl ecx
                        shl ecx
                        mov CLen, ecx
                        cld
@no_update:
                    end;
                    SOfs := VideoBufSize - (SOfs shl 2);
                end else
                    begin
                        SOfs := 0;
                        CLen := VideoBufSize;
                    end;
            if Force then
                begin
                    VioShowBuf (SOfs, CLen, 0);
                    Move (VideoBuf^ [SOfs div SizeOf (TVideoCell)],
                            OldVideoBuf^ [SOfs div SizeOf (TVideoCell)], CLen);
                end;
        end;
end;

initialization
  RegisterVideoModes;
  TargetEntry;

finalization
  UnRegisterVideoModes;
end.
{
  $Log$
  Revision 1.1  2001-01-13 11:03:58  peter
    * API 2 RTL commit

}

