{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Nils Sjoholm and Carl Eric Codere
    Copyright (c) 2019 by Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit crt;

interface

{$i crth.inc}

implementation

uses
  exec, amigados, conunit, intuition, agraphics;

var
  MaxCols, MaxRows: LongInt;

const
  CD_CURRX = 1;
  CD_CURRY = 2;
  CD_MAXX  = 3;
  CD_MAXY  = 4;
  // Special Character for commands to console
  CSI = Chr($9b);

var
  // Pens for Front/Backcolors (must be 0-7)
  RedPen: LongInt = -1;
  FreeRed: Boolean = False;
  GreenPen: LongInt = -1;
  FreeGreen: Boolean = False;
  // multiple keys
  LastKeys: string = '';

function SendActionPacket(Port: PMsgPort; Arg: BPTR): LongInt;
var
  ReplyPort: PMsgPort;
  Packet: PStandardPacket;
  Ret: NativeInt;
begin
  SendActionPacket := 0;
  ReplyPort := CreateMsgPort;
  if not Assigned(ReplyPort) then
    Exit;

  Packet := AllocMem(SizeOf(TStandardPacket));

  if not Assigned(Packet) then
  begin
    DeleteMsgPort(ReplyPort);
    Exit;
  end;

  Packet^.sp_Msg.mn_Node.ln_Name := @(Packet^.sp_Pkt);
  Packet^.sp_Pkt.dp_Link := @(Packet^.sp_Msg);
  Packet^.sp_Pkt.dp_Port := ReplyPort;
  Packet^.sp_Pkt.dp_Type := ACTION_DISK_INFO;
  Packet^.sp_Pkt.dp_Arg1 := NativeInt(Arg);

  PutMsg(Port, PMessage(Packet));
  WaitPort(ReplyPort);
  GetMsg(ReplyPort);

  Ret := Packet^.sp_Pkt.dp_Res1;

  FreeMem(Packet);
  DeleteMsgPort(ReplyPort);

  SendActionPacket := Ret;
end;

function OpenInfo: PInfoData;
var
  Port: PMsgPort;
  Info: PInfoData;
  Bptr1: BPTR;
begin
  Info := PInfoData(AllocMem(SizeOf(TInfoData)));

  if Assigned(Info) then
  begin
    {$ifdef AmigaOS4}
    Port := PFileHandle(BADDR(DosInput()))^.fh_MsgPort;
    {$else}
    Port := PFileHandle(BADDR(DosInput()))^.fh_Type;
    {$endif}
    //GetConsoleTask;
    Bptr1  := MKBADDR(Info);

    if Assigned(Port) then
    begin
      if SendActionPacket(Port, Bptr1) = 0 then
        Port := nil;
    end;

    if Port = nil then
    begin
      FreeMem(Info);
      Info := nil;
    end;
  end;
  OpenInfo := Info;
end;

procedure CloseInfo(var Info: PInfoData);
begin
  if Assigned(Info) then
  begin
    FreeMem(Info);
    Info := nil;
  end;
end;

function ConData(Modus: Byte): Integer;
var
  Info:  PInfoData;
  TheUnit: PConUnit;
  Pos: Longint;
begin
  pos := 1;
  Info := OpenInfo;

  if Assigned(Info) then
  begin
    TheUnit := PConUnit((PIoStdReq(Info^.id_InUse))^.io_Unit);
    case modus of
      CD_CURRX: pos := TheUnit^.cu_XCP;
      CD_CURRY: pos := TheUnit^.cu_YCP;
      CD_MAXX: pos := TheUnit^.cu_XMax;
      CD_MAXY: pos := TheUnit^.cu_YMax;
    end;
    CloseInfo(Info);
  end;
  ConData := Pos + 1;
end;

function WhereX: TCrtCoord;
begin
  WhereX := Byte(ConData(CD_CURRX)) - WindMinX;
end;

function RealX: Byte;
begin
  RealX := Byte(ConData(CD_CURRX));
end;

function RealY: Byte;
begin
  RealY := Byte(ConData(CD_CURRY));
end;

function WhereY: TCrtCoord;
begin
  WhereY := Byte(ConData(CD_CURRY)) - WindMinY;
end;

function ScreenCols: Integer;
begin
  Screencols := ConData(CD_MAXX);
end;

function ScreenRows: Integer;
begin
  ScreenRows := ConData(CD_MAXY);
end;

procedure RealGotoXY(x, y: Integer);
begin
  Write(CSI, y, ';', x, 'H');
end;

procedure GotoXY(x, y: TCrtCoord);
begin
  if y + WindMinY - 2 >= WindMaxY then
    y := WindMaxY - WindMinY + 1;
  if x + WindMinX - 2 >= WindMaxX then
    x := WindMaxX - WindMinX + 1;
  Write(CSI, y + WindMinY, ';', x + WindMinX, 'H');
end;

procedure CursorOff;
begin
  Write(CSI,'0 p');
end;

procedure CursorOn;
begin
  Write(CSI,' p');
end;

procedure ClrScr;
begin
  Write(Chr($0c));
end;

function WaitForKey: string;
var
  OutP: BPTR; // Output file handle
  Res: Char; // Char to get fropm console
  Key: string; // result
begin
  Key := '';
  OutP := DosOutput();
  SetMode(OutP, 1); // change to Raw Mode
  // Special for AROS
  // AROS always sends a #184, #185 or #0, ignore them
  repeat
    Res := #0;
    DosRead(OutP, @Res, 1);
    if not (Ord(Res) in [184, 185, 0]) then
      Break;
    Delay(1);
  until False;
  // get the key
  Key := Res;
  // Check if Special OP
  if Res = CSI then
  begin
    repeat
      Res := #0;
      DosRead(OutP, @Res, 1);
      if Ord(Res) in [184, 185, 0] then // just to make sure on AROS that it ends when nothing left
        Break;
      if Ord(Res) = 126 then // end marker
        Break;
      Key := Key + Res; // add to final string
      // stop on cursor, they have no end marker...
      case Ord(Res) of
        64..69,83,84: Break;
      end;
    until False;
  end;
  // set result
  WaitForKey := Key;
  // set back mode to CON:
  SetMode(OutP, 0);
end;

type
  TKeyMap = record
    con: string;
    c1: Char;
    c2: Char;
  end;
const
  KeyMapping: array[0..17] of TKeyMap =
    ((con: #155'0'; c1: #0; c2:#59;), // F1
     (con: #155'1'; c1: #0; c2:#60;), // F2
     (con: #155'2'; c1: #0; c2:#61;), // F3
     (con: #155'3'; c1: #0; c2:#62;), // F4
     (con: #155'4'; c1: #0; c2:#63;), // F5
     (con: #155'5'; c1: #0; c2:#64;), // F6
     (con: #155'6'; c1: #0; c2:#65;), // F7
     (con: #155'7'; c1: #0; c2:#66;), // F8
     (con: #155'8'; c1: #0; c2:#67;), // F9
     (con: #155'9'; c1: #0; c2:#68;), // F10
     (con: #155'20'; c1: #0; c2:#133;), // F11
     (con: #155'21'; c1: #0; c2:#134;), // F12

     (con: #155'41'; c1: #0; c2:#73;), // Page Up
     (con: #155'42'; c1: #0; c2:#81;), // Page Down

     (con: #155'A'; c1: #0; c2:#72;), // Cursor Up
     (con: #155'B'; c1: #0; c2:#80;), // Cursor Down
     (con: #155'C'; c1: #0; c2:#77;), // Cursor Right
     (con: #155'D'; c1: #0; c2:#75;)  // Cursor Left
     );

function ReadKey: Char;
var
  Res: string;
  i: Integer;
begin
  // we got a key to sent
  if Length(LastKeys) > 0 then
  begin
    ReadKey := LastKeys[1];
    Delete(LastKeys, 1, 1);
    Exit;
  end;
  Res := WaitForKey;
  // Search for Map Key
  for i := 0 to High(KeyMapping) do
  begin
    if KeyMapping[i].Con = Res then
    begin
      ReadKey := KeyMapping[i].c1;
      if KeyMapping[i].c2 <> #0 then
        LastKeys := KeyMapping[i].c2;
      Exit;
    end;
  end;
  ReadKey := Res[1];
end;


// Wait for Key, does not work for AROS currently
// because WaitForChar ALWAYS returns even no key is pressed, but this
// is clearly an AROS bug
function KeyPressed : Boolean;
var
  OutP: BPTR;
begin
  if Length(LastKeys) > 0 then
  begin
    KeyPressed := True;
    Exit;
  end;
  OutP := DosOutput();
  SetMode(OutP, 1);
  // Wait one millisecond for the key (-1 = timeout)
  {$if defined(AROS)}
  KeyPressed := WaitForChar(OutP, 1) <> 0;
  {$else}
  KeyPressed := WaitForChar(OutP, 1);
  {$endif}
  SetMode(OutP, 0);
end;

function ConvertColor(Color: Byte): Byte;
begin
  Color := Color and $f; // make sure we are in the 0..7 range
  // make some color mappings
  case Color of
     White: ConvertColor := 2;
     Black: ConvertColor := 1;
     Blue: ConvertColor := 3;
     LightGray: ConvertColor := 0;
     Red: ConvertColor := RedPen;
     Green: ConvertColor := GreenPen;
  else
    ConvertColor := Color;
  end;
end;

function ConvertColorBack(Color: Byte): Byte;
begin
  Color := Color and $f;
  case Color of
     2 : ConvertColorBack := White;
     1: ConvertColorBack := Black;
     3: ConvertColorBack := Blue;
     0: ConvertColorBack := LightGray;
  else
    if Color = RedPen then ConvertColorBack := Red else
    if color = GreenPen then ConvertColorBack := Green else
    ConvertColorBack := Color;
  end;
end;

procedure TextColor(color : byte);
begin
  Color := ConvertColor(Color);
  TextAttr := (TextAttr and $70) or Color;
  Write(CSI, '3', color, 'm');
end;

procedure TextBackground(color : byte);
begin
  Color := ConvertColor(Color);
  Textattr:=(textattr and $8f) or ((Color and $7) shl 4);
  Write(CSI, '4', color, 'm');
end;

function GetTextBackground: Byte;
var
  Info: PInfoData;
  Pen: Byte;
begin
  pen := 1;
  Info := OpenInfo;
  if Assigned(Info)then
  begin
    Pen := PConUnit((PIoStdReq(Info^.id_InUse))^.io_Unit)^.cu_BgPen;
    Pen := ConvertColorBack(Pen);
    CloseInfo(Info);
  end;
  GetTextBackground := Pen;
end;

function GetTextColor: Byte;
var
  Info: PInfoData;
  Pen: Byte;
begin
  Pen := 1;
  Info := OpenInfo;
  if Assigned(info) then
  begin
    Pen := PConUnit((PIoStdReq(Info^.id_InUse))^.io_Unit)^.cu_FgPen;
    Pen := ConvertColorBack(Pen);
    CloseInfo(Info);
  end;
  GetTextColor := Pen;
end;

procedure Window(X1,Y1,X2,Y2: Byte);
begin
  if x1 < 1 then
    x1 := 1;
  if y1 < 1 then
    y1 := 1;
  if (x2 > ScreenCols) or (y2 > ScreenRows) or (x1 > x2) or (y1 > y2) then
    Exit;
  WindMinX := x1 - 1;
  WindMinY := y1 - 1;
  WindMaxX := x2 - 1;
  WindMaxY := y2 - 1;
  GotoXY(1, 1);
end;


procedure DelLine;
begin
  Write(CSI,'X');
end;

procedure ClrEol;
begin
  Write(CSI,'K');
end;

procedure InsLine;
begin
  Write(CSI,'1 L');
end;

procedure CursorBig;
begin
end;

procedure LowVideo;
begin
end;

procedure HighVideo;
begin
end;

procedure NoSound;
begin
end;

procedure Sound(hz: Word);
begin
end;

procedure NormVideo;
begin
end;

procedure AssignCrt(var F: Text);
begin
end;

procedure Delay(ms: Word);
var
  Dummy: Longint;
begin
  dummy := Trunc((ms / 1000.0) * 50.0);
  DOSDelay(dummy);
end;

procedure TextMode(Mode: word);
begin
  LastMode := Mode;
  Mode := Mode and $ff;
  MaxCols := ScreenCols;
  MaxRows := ScreenRows;
  WindMinX := 0;
  WindMinY := 0;
  WindMaxX := MaxCols - 1;
  WindMaxY := MaxRows - 1;
end;

function GetClosestPen(r,g,b: Byte): ShortInt;
var
  i: Byte;
  cm: PColorMap;
  AR, AG, AB: Byte;
  Col: LongInt;
  MinDist, Dist: LongInt;
begin
  GetClosestPen := -1;
  cm := IntuitionBase^.ActiveScreen^.ViewPort.ColorMap;
  MinDist := MaxInt;
  for i := 2 to 7 do
  begin
    Col := GetRGB4(CM, i);
    if Col = -1 then
      Continue;
    AR := (Col shr 8) and $F;
    AR := AR or (AR shl 4);
    AG := (Col shr 4) and $F;
    AG := AG or (AR shl 4);
    AB := (Col shr 0) and $F;
    AB := AB or (AR shl 4);
    Dist := Abs(AR-r) + Abs(AG-g) + Abs(AB-b);
    if Dist < MinDist then
    begin
      GetClosestPen := i;
      MinDist := Dist;
    end;
  end;
end;

initialization
  // Init Colors, (until now only Red and Green)
  RedPen := ObtainPen(IntuitionBase^.ActiveScreen^.ViewPort.ColorMap, 7, $FFFFFFFF, 0, 0, 0);
  FreeRed := RedPen >= 0;
  if not FreeRed then
    RedPen := GetClosestPen($ff,00,00);
  //
  GreenPen := ObtainPen(IntuitionBase^.ActiveScreen^.ViewPort.ColorMap, 6, 0, $FFFFFFFF, 0, 0);
  FreeGreen := GreenPen >= 0;
  if not FreeRed then
    GreenPen := GetClosestPen(00,$ff,00);

  // load system variables to temporary variables to save time
  MaxCols := ScreenCols;
  MaxRows := ScreenRows;
  // Set the initial text attributes
  // Text background
  Textattr:=(textattr and $8f) or ((GetTextBackGround and $7) shl 4);
  // Text foreground
  TextAttr := (TextAttr and $70) or GetTextColor;
  // set output window
  WindMaxX := MaxCols - 1;
  WindMaxY := MaxRows - 1;

finalization
  if FreeRed then
    ReleasePen(IntuitionBase^.ActiveScreen^.ViewPort.ColorMap, RedPen);
  if FreeGreen then
    ReleasePen(IntuitionBase^.ActiveScreen^.ViewPort.ColorMap, GreenPen);
  write(CSI,'0m');
  CursorOn;
end.
