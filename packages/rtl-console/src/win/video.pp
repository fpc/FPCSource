{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Video unit for Win32/Win64

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Video;
interface

{$i videoh.inc}

procedure VideoSetConsoleOutHandle (NewHandle: THandle);

implementation

uses
  windows,dos,graphemebreakproperty,eastasianwidth,charset;

{$i video.inc}

const
    LastCursorType: word = crUnderline;
    OrigScreen: PVideoBuf = nil;
    OrigScreenSize: cardinal = 0;
    ConsoleOutDeviceName: string[8] = 'CONOUT$'#0;

var ConsoleInfo : TConsoleScreenBufferInfo;
    ConsoleCursorInfo : TConsoleCursorInfo;

    OrigCP: cardinal;
    OrigConsoleCursorInfo : TConsoleCursorInfo;
    OrigConsoleInfo : TConsoleScreenBufferInfo;
    NoConsoleOnStart: boolean;
    NewConsoleHandleAllocated:  boolean;
    ConsoleOutHandle: THandle;

    LineBuf: array of TCharInfo;

procedure SysInitVideo;
var
  SecAttr: TSecurityAttributes;
begin
  ScreenColor:=true;
  if NoConsoleOnStart then
   begin
    if not (AllocConsole) then
     begin
      WriteLn ('Error: No console available and console creation failed!');
      RunError (103);
     end;
{Reopen StdOut/StdErr/StdIn}
    OrigCP := GetACP;
    with SecAttr do
     begin 
      nLength := SizeOf (TSecurityAttributes);
      SecAttr.bInheritHandle := true;
      SecAttr.lpSecurityDescriptor := nil;
     end;
    ConsoleOutHandle := CreateFile (@ConsoleOutDeviceName [1], Generic_Read or Generic_Write, File_Share_Write, @SecAttr, Open_Existing, File_Attribute_Normal, 0);
    if ConsoleOutHandle = Invalid_Handle_Value then
     begin
      WriteLn ('Error: Console output not possible!');
      RunError (103);
     end
    else
     NewConsoleHandleAllocated := true;
    GetConsoleScreenBufferInfo (ConsoleOutHandle, ConsoleInfo);
    GetConsoleCursorInfo (ConsoleOutHandle, ConsoleCursorInfo);
   end
  else
   begin
    GetConsoleScreenBufferInfo(ConsoleOutHandle, OrigConsoleInfo);
    GetConsoleCursorInfo(ConsoleOutHandle, OrigConsoleCursorInfo);
    OrigCP := GetConsoleCP;
    ConsoleInfo:=OrigConsoleInfo;
    ConsoleCursorInfo:=OrigConsoleCursorInfo;
   end;
  {
    About the ConsoleCursorInfo record: There are 3 possible
    structures in it that can be regarded as the 'screen':
    - dwsize   : contains the cols & row in current screen buffer.
    - srwindow : Coordinates (relative to buffer) of upper left
                 & lower right corners of visible console.
    - dmMaximumWindowSize : Maximal size of Screen buffer.
    The first implementation of video used srWindow. After some
    bug-reports, this was switched to dwMaximumWindowSize.
  }
  with ConsoleInfo.dwMaximumWindowSize do
    begin
    ScreenWidth:=X;
    ScreenHeight:=Y;
    end;
  { TDrawBuffer only has FVMaxWidth elements
    larger values lead to crashes }
  if ScreenWidth> FVMaxWidth then
    ScreenWidth:=FVMaxWidth;
  CursorX:=ConsoleInfo.dwCursorPosition.x;
  CursorY:=ConsoleInfo.dwCursorPosition.y;
  if not ConsoleCursorInfo.bvisible then
    CursorLines:=0
  else
    CursorLines:=ConsoleCursorInfo.dwSize;
end;



procedure VideoSetConsoleOutHandle (NewHandle: THandle);
begin
  if NewHandle <> ConsoleOutHandle then
   begin
    if NewConsoleHandleAllocated then
     begin
      CloseHandle (ConsoleOutHandle);
      NewConsoleHandleAllocated := false;
     end;
    ConsoleOutHandle := NewHandle;
   end;
end;



procedure SysDoneVideo;
begin
  if NoConsoleOnStart then
   begin
    CloseHandle (ConsoleOutHandle);
    NewConsoleHandleAllocated := false;
    ConsoleOutHandle := Invalid_Handle_Value;
    FreeConsole;
   end
  else
   begin
    SetConsoleScreenBufferSize (ConsoleOutHandle, OrigConsoleInfo.dwSize);
    SetConsoleWindowInfo (ConsoleOutHandle, true, OrigConsoleInfo.srWindow);
    SetConsoleCursorInfo(ConsoleOutHandle, OrigConsoleCursorInfo);
    SetConsoleCP(OrigCP);
   end;
  SetLength(LineBuf,0);
end;


function SysGetCapabilities: Word;
begin
  SysGetCapabilities:=cpColor or cpChangeCursor;
end;


procedure SysSetCursorPos(NewCursorX, NewCursorY: Word);
var
  pos : COORD;
begin
   pos.x:=NewCursorX;
   pos.y:=NewCursorY;
   SetConsoleCursorPosition(ConsoleOutHandle,pos);
   CursorX:=pos.x;
   CursorY:=pos.y;
end;


function SysGetCursorType: Word;
begin
   GetConsoleCursorInfo(ConsoleOutHandle,ConsoleCursorInfo);
   if not ConsoleCursorInfo.bvisible then
     SysGetCursorType:=crHidden
   else
     case ConsoleCursorInfo.dwSize of
        1..30:
          SysGetCursorType:=crUnderline;
        31..70:
          SysGetCursorType:=crHalfBlock;
        71..100:
          SysGetCursorType:=crBlock;
     end;
end;


procedure SysSetCursorType(NewType: Word);
begin
   GetConsoleCursorInfo(ConsoleOutHandle,ConsoleCursorInfo);
   if newType=crHidden then
     ConsoleCursorInfo.bvisible:=false
   else
     begin
        ConsoleCursorInfo.bvisible:=true;
        case NewType of
           crUnderline:
             ConsoleCursorInfo.dwSize:=10;

           crHalfBlock:
             ConsoleCursorInfo.dwSize:=50;

           crBlock:
             ConsoleCursorInfo.dwSize:=99;
        end
     end;
   SetConsoleCursorInfo(ConsoleOutHandle,ConsoleCursorInfo);
end;

function SysVideoModeSelector (const VideoMode: TVideoMode): boolean;

var MI: Console_Screen_Buffer_Info;
    C: Coord;
    SR: Small_Rect;

begin
  if not (GetConsoleScreenBufferInfo (ConsoleOutHandle, MI)) then
    SysVideoModeSelector := false
  else
    begin
      with MI do
        begin
          C.X := VideoMode.Col;
          C.Y := VideoMode.Row;
        end;
      with SR do
        begin
          Top := 0;
          Left := 0;
          { First, we need to make sure we reach the minimum window size
            to always fit in the new buffer after changing buffer size. }
          Right := MI.srWindow.Right - MI.srWindow.Left;
          if VideoMode.Col <= Right then
            Right := Pred (VideoMode.Col);
          Bottom := MI.srWindow.Bottom - MI.srWindow.Top;
          if VideoMode.Row <= Bottom then
            Bottom := Pred (VideoMode.Row);
        end;
      if SetConsoleWindowInfo (ConsoleOutHandle, true, SR) then
        if SetConsoleScreenBufferSize (ConsoleOutHandle, C) then
          begin
            with SR do
              begin
                { Now, we can resize the window to the final size. }
                Right := Pred (VideoMode.Col);
                Bottom := Pred (VideoMode.Row);
              end;
            if SetConsoleWindowInfo (ConsoleOutHandle, true, SR) then
              begin
                SysVideoModeSelector := true;
                SetCursorType (LastCursorType);
                ClearScreen;
              end
            else
              begin
                SysVideoModeSelector := false;
                SetConsoleScreenBufferSize (ConsoleOutHandle, MI.dwSize);
                SetConsoleWindowInfo (ConsoleOutHandle, true, MI.srWindow);
                SetCursorType (LastCursorType);
              end
          end
        else
          begin
            SysVideoModeSelector := false;
            SetConsoleWindowInfo (ConsoleOutHandle, true, MI.srWindow);
            SetCursorType (LastCursorType);
          end
      else
        SysVideoModeSelector := false;
    end;
end;

Const
  SysVideoModeCount = 6;
  SysVMD : Array[0..SysVideoModeCount-1] of TVideoMode = (
   (Col: 40; Row: 25; Color: True),
   (Col: 80; Row: 25; Color: True),
   (Col: 80; Row: 30; Color: True),
   (Col: 80; Row: 43; Color: True),
   (Col: 80; Row: 50; Color: True),
   (Col: 80; Row: 25; Color: True) // Reserved for TargetEntry
  );


Function SysSetVideoMode (Const Mode : TVideoMode) : Boolean;

Var
  I : Integer;

begin
  I:=SysVideoModeCount-1;
  SysSetVideoMode:=False;
  While (I>=0) and Not SysSetVideoMode do
    If (Mode.col=SysVMD[i].col) and
       (Mode.Row=SysVMD[i].Row) and
       (Mode.Color=SysVMD[i].Color) then
      SysSetVideoMode:=True
    else
      Dec(I);
  If SysSetVideoMode then
    begin
    if SysVideoModeSelector(Mode) then
      begin
      ScreenWidth:=SysVMD[I].Col;
      ScreenHeight:=SysVMD[I].Row;
      ScreenColor:=SysVMD[I].Color;
      end else SysSetVideoMode := false;
    end;
end;

Function SysGetVideoModeData (Index : Word; Var Data : TVideoMode) : boolean;

begin
  SysGetVideoModeData:=(Index<=high(SysVMD));
  If SysGetVideoModeData then
    Data:=SysVMD[Index];
end;

Function SysGetVideoModeCount : Word;

begin
  SysGetVideoModeCount:=SysVideoModeCount;
end;

procedure SysClearScreen;
begin
  UpdateScreen(true);
end;

procedure SysUpdateScreen(Force: Boolean);
var
   BufSize,
   BufCoord    : COORD;
   WriteRegion : SMALL_RECT;
   BufCounter  : Longint;
   LineCounter,
   ColCounter  : Longint;
   smallforce  : boolean;
   x1,y1,x2,y2 : longint;
begin
  if force then
   smallforce:=true
  else
   SmallForce:=CompareByte(EnhancedVideoBuf[0],OldEnhancedVideoBuf[0],Length(EnhancedVideoBuf)*SizeOf(TEnhancedVideoCell))<>0;
  if SmallForce then
   begin
      BufSize.X := ScreenWidth;
      BufSize.Y := ScreenHeight;

      BufCoord.X := 0;
      BufCoord.Y := 0;
      with WriteRegion do
        begin
           Top :=0;
           Left :=0;
           Bottom := ScreenHeight-1;
           Right := ScreenWidth-1;
        end;
      BufCounter := 0;
      x1:=ScreenWidth+1;
      x2:=-1;
      y1:=ScreenHeight+1;
      y2:=-1;
      SetLength(LineBuf,ScreenHeight*ScreenWidth);
      for LineCounter := 1 to ScreenHeight do
        begin
           for ColCounter := 1 to ScreenWidth do
             begin
               if EnhancedVideoBuf[BufCounter]<>OldEnhancedVideoBuf[BufCounter] then
                 begin
                   OldEnhancedVideoBuf[BufCounter]:=EnhancedVideoBuf[BufCounter];
                   if ColCounter<x1 then
                     x1:=ColCounter;
                   if ColCounter>x2 then
                     x2:=ColCounter;
                   if LineCounter<y1 then
                     y1:=LineCounter;
                   if LineCounter>y2 then
                     y2:=LineCounter;
                 end;
               if Length(EnhancedVideoBuf[BufCounter].ExtendedGraphemeCluster) = 1 then
                 LineBuf[BufCounter].UniCodeChar := EnhancedVideoBuf[BufCounter].ExtendedGraphemeCluster[1]
               else
                 LineBuf[BufCounter].UniCodeChar := ' ';
               { If (WordRec(VideoBuf^[BufCounter]).Two and $80)<>0 then
                 LineBuf^[BufCounter].Attributes := $100+WordRec(VideoBuf^[BufCounter]).Two
               else }
               LineBuf[BufCounter].Attributes := EnhancedVideoBuf[BufCounter].Attribute;

               Inc(BufCounter);
             end; { for }
        end; { for }
      BufSize.X := ScreenWidth;
      BufSize.Y := ScreenHeight;

      with WriteRegion do
        begin
           if force then
             begin
               Top := 0;
               Left :=0;
               Bottom := ScreenHeight-1;
               Right := ScreenWidth-1;
               BufCoord.X := 0;
               BufCoord.Y := 0;
             end
           else
             begin
               Top := y1-1;
               Left :=x1-1;
               Bottom := y2-1;
               Right := x2-1;
               BufCoord.X := x1-1;
               BufCoord.Y := y1-1;
             end;
        end;
      {
      writeln('X1: ',x1);
      writeln('Y1: ',y1);
      writeln('X2: ',x2);
      writeln('Y2: ',y2);
      }
      WriteConsoleOutputW(ConsoleOutHandle, @LineBuf[0], BufSize, BufCoord, WriteRegion)
   end;
end;

Const
  SysVideoDriver : TVideoDriver = (
    InitDriver : nil;
    InitEnhancedDriver: @SysInitVideo;
    DoneDriver : @SysDoneVideo;
    UpdateScreen : @SysUpdateScreen;
    UpdateScreenArea: nil;
    ClearScreen : @SysClearScreen;
    SetVideoMode : @SysSetVideoMode;
    GetVideoModeCount : @SysGetVideoModeCount;
    GetVideoModeData : @SysGetVideoModeData;
    SetCursorPos : @SysSetCursorPos;
    GetCursorType : @SysGetCursorType;
    SetCursorType : @SysSetCursorType;
    GetCapabilities : @SysGetCapabilities;
    GetActiveCodePage : Nil;
    ActivateCodePage : Nil;
    GetSupportedCodePageCount : Nil;
    GetSupportedCodePage : Nil;
  );

procedure TargetEntry;
var
  C: Coord;
  SR: Small_Rect;
  VioMode: TConsoleScreenBufferInfo;
  SecAttr: TSecurityAttributes;
begin
  NewConsoleHandleAllocated := false;
  FillChar (VioMode, SizeOf (VioMode), 0);
  ConsoleOutHandle := GetStdHandle (Std_Output_Handle);
{MSDN: If an application does not have associated standard handles, such as a service running on an
 interactive desktop, and has not redirected them, the return value is NULL.}
  if (ConsoleOutHandle = 0) or (ConsoleOutHandle = Invalid_Handle_Value) then
   NoConsoleOnStart := true
  else
   if not (GetConsoleScreenBufferInfo (ConsoleOutHandle, VioMode)) then
    begin
{ StdOut may be redirected, let's try to access the console using a new handle }
     with SecAttr do
      begin 
       nLength := SizeOf (TSecurityAttributes);
       SecAttr.bInheritHandle := true;
       SecAttr.lpSecurityDescriptor := nil;
      end;
     ConsoleOutHandle := CreateFile (@ConsoleOutDeviceName [1], Generic_Read or Generic_Write, File_Share_Write, @SecAttr, Open_Existing, File_Attribute_Normal, 0);
     if ConsoleOutHandle = Invalid_Handle_Value then
      NoConsoleOnStart := true
     else
      NewConsoleHandleAllocated := true;
     if not (GetConsoleScreenBufferInfo (ConsoleOutHandle, VioMode)) then
      begin
       NoConsoleOnStart := true;
       CloseHandle (ConsoleOutHandle);
       ConsoleOutHandle := Invalid_Handle_Value;
       NewConsoleHandleAllocated := false;
      end;
    end;
  if not (NoConsoleOnStart) then
   begin
    with VioMode do
     begin
      OrigScreenSize := max(dwMaximumWindowSize.X,dwSize.X) * max(dwMaximumWindowSize.Y,dwSize.Y) * SizeOf (Char_Info);
      if OrigScreenSize > 0 then
       begin
      { Register the curent video mode in reserved slot in System Modes}
        SysVMD[SysVideoModeCount-1].Col:=dwMaximumWindowSize.X;
        SysVMD[SysVideoModeCount-1].Row:=dwMaximumWindowSize.Y;
        SysVMD[SysVideoModeCount-1].Color:=true;
        GetMem (OrigScreen, OrigScreenSize);
       end;
     end;
    if OrigScreenSize > 0 then
     begin
      with C do
       begin
        X := 0;
        Y := 0;
       end;
      with SR do
       begin
        Top := 0;
        Left := 0;
        Right := Pred (VioMode.dwSize.X);
        Bottom := Pred (VioMode.dwSize.Y);
       end;
      if not (ReadConsoleOutput (ConsoleOutHandle, OrigScreen, VioMode.dwSize, C, SR)) then
       begin
        FreeMem (OrigScreen, OrigScreenSize);
        OrigScreen := nil;
        OrigScreenSize := 0;
       end;
     end;
   end;
end;


initialization
  SetVideoDriver(SysVideoDriver);
  TargetEntry;

finalization
  if (OrigScreenSize <> 0) and (OrigScreen <> nil) then
    begin
      FreeMem (OrigScreen, OrigScreenSize);
      OrigScreen := nil;
      OrigScreenSize := 0;
    end;
  if NewConsoleHandleAllocated then
   CloseHandle (ConsoleOutHandle);
end.
