{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by Nils Sjoholm

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}



unit Crt;
Interface

Const
{ Controlling consts }
  Flushing=false;                       {if true then don't buffer output}
  ScreenWidth  = 80;
  ScreenHeight = 25;

{ CRT modes }
  BW40          = 0;            { 40x25 B/W on Color Adapter }
  CO40          = 1;            { 40x25 Color on Color Adapter }
  BW80          = 2;            { 80x25 B/W on Color Adapter }
  CO80          = 3;            { 80x25 Color on Color Adapter }
  Mono          = 7;            { 80x25 on Monochrome Adapter }
  Font8x8       = 256;          { Add-in for ROM font }

{ Mode constants for 3.0 compatibility }
  C40           = CO40;
  C80           = CO80;

{
  When using this color constants on the Amiga
  you can bet that they don't work as expected.
  You never know what color the user has on
  his Amiga. Perhaps we should do a check of
  the number of bitplanes (for number of colors)

  The normal 4 first pens for an Amiga are

  0 LightGrey
  1 Black
  2 White
  3 Blue

}

{ Foreground and background color constants  }
  Black         = 1;  { normal pen for amiga }
  Blue          = 3;  { windowborder color   }
  Green         = 15;
  Cyan          = 7;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 0;  { canvas color         }

{ Foreground color constants }
  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 2;  { third color on amiga }

{ Add-in for blinking }
  Blink         = 128;

{Other Defaults}

  TextAttr   : Byte = $07;
  LastMode   : Word = 3;
  WindMin    : Word = $0;
  WindMax    : Word = $184f;
var
  { CheckBreak have to make this one to a function for Amiga }
  CheckEOF,
  CheckSnow,
  DirectVideo: Boolean;

Procedure AssignCrt(Var F: Text);
Function  KeyPressed: Boolean;
Function  ReadKey: Char;
Procedure TextMode(Mode: Integer);
Procedure Window(X1, Y1, X2, Y2: Integer);
Procedure GoToXy(X: Integer; Y: Integer);
Function  WhereX: Integer;
Function  WhereY: Integer;
Procedure ClrScr;
Procedure ClrEol;
Procedure InsLine;
Procedure DelLine;
Procedure TextColor(Color: Byte);
Procedure TextBackground(Color: Byte);
Procedure LowVideo;
Procedure HighVideo;
Procedure NormVideo;
Procedure Delay(DTime: Word);
Procedure Sound(Hz: Word);
Procedure NoSound;

{ Extra functions }

Procedure CursorOn;
Procedure CursorOff;
Function CheckBreak: Boolean;

Implementation

{
  The definitions of TextRec and FileRec are in separate files.
}
{$i textrec.inc}
{$i filerec.inc}

Type

    pInfoData = ^tInfoData;
    tInfoData = packed record
        id_NumSoftErrors        : Longint;      { number of soft errors on disk }
        id_UnitNumber           : Longint;      { Which unit disk is (was) mounted on }
        id_DiskState            : Longint;      { See defines below }
        id_NumBlocks            : Longint;      { Number of blocks on disk }
        id_NumBlocksUsed        : Longint;      { Number of block in use }
        id_BytesPerBlock        : Longint;
        id_DiskType             : Longint;      { Disk Type code }
        id_VolumeNode           : Longint;         { BCPL pointer to volume node }
        id_InUse                : Longint;      { Flag, zero if not in use }
    end;

{ *  List Node Structure.  Each member in a list starts with a Node * }

  pNode = ^tNode;
  tNode = packed Record
    ln_Succ,                { * Pointer to next (successor) * }
    ln_Pred  : pNode;       { * Pointer to previous (predecessor) * }
    ln_Type  : Byte;
    ln_Pri   : Shortint;    { * Priority, for sorting * }
    ln_Name  : PChar;       { * ID string, null terminated * }
  End;  { * Note: Integer aligned * }

{ normal, full featured list }

    pList = ^tList;
    tList = packed record
    lh_Head     : pNode;
    lh_Tail     : pNode;
    lh_TailPred : pNode;
    lh_Type     : Byte;
    l_pad       : Byte;
    end;

    pMsgPort = ^tMsgPort;
    tMsgPort = packed record
    mp_Node     : tNode;
    mp_Flags    : Byte;
    mp_SigBit   : Byte;      { signal bit number    }
    mp_SigTask  : Pointer;   { task to be signalled (TaskPtr) }
    mp_MsgList  : tList;     { message linked list  }
    end;

    pMessage = ^tMessage;
    tMessage = packed record
    mn_Node       : tNode;
    mn_ReplyPort  : pMsgPort;   { message reply port }
    mn_Length     : Word;       { message len in bytes }
    end;

    pIOStdReq = ^tIOStdReq;
    tIOStdReq = packed record
    io_Message  : tMessage;
    io_Device   : Pointer;      { device node pointer  }
    io_Unit     : Pointer;      { unit (driver private)}
    io_Command  : Word;         { device command }
    io_Flags    : Byte;
    io_Error    : Shortint;     { error or warning num }
    io_Actual   : Longint;      { actual number of bytes transferred }
    io_Length   : Longint;      { requested number bytes transferred}
    io_Data     : Pointer;      { points to data area }
    io_Offset   : Longint;      { offset for block structured devices }
    end;

    pIntuiMessage = ^tIntuiMessage;
    tIntuiMessage = packed record
        ExecMessage     : tMessage;
        Class_          : Longint;
        Code            : Word;
        Qualifier       : Word;
        IAddress        : Pointer;
        MouseX,
        MouseY          : Word;
        Seconds,
        Micros          : Longint;
        IDCMPWindow     : Pointer;
        SpecialLink     : pIntuiMessage;
    end;

    pWindow = ^tWindow;
    tWindow = packed record
        NextWindow      : pWindow;      { for the linked list in a screen }
        LeftEdge,
        TopEdge         : Integer;      { screen dimensions of window }
        Width,
        Height          : Integer;      { screen dimensions of window }
        MouseY,
        MouseX          : Integer;      { relative to upper-left of window }
        MinWidth,
        MinHeight       : Integer;      { minimum sizes }
        MaxWidth,
        MaxHeight       : Word;         { maximum sizes }
        Flags           : Longint;      { see below for defines }
        MenuStrip       : Pointer;      { the strip of Menu headers }
        Title           : PChar;        { the title text for this window }
        FirstRequest    : Pointer;      { all active Requesters }
        DMRequest       : Pointer;      { double-click Requester }
        ReqCount        : Integer;      { count of reqs blocking Window }
        WScreen         : Pointer;      { this Window's Screen }
        RPort           : Pointer;      { this Window's very own RastPort }
        BorderLeft,
        BorderTop,
        BorderRight,
        BorderBottom    : Shortint;
        BorderRPort     : Pointer;
        FirstGadget     : Pointer;
        Parent,
        Descendant      : pWindow;
        Pointer_        : Pointer;      { sprite data }
        PtrHeight       : Shortint;     { sprite height (not including sprite padding) }
        PtrWidth        : Shortint;     { sprite width (must be less than or equal to 16) }
        XOffset,
        YOffset         : Shortint;     { sprite offsets }
        IDCMPFlags      : Longint;      { User-selected flags }
        UserPort,
        WindowPort      : pMsgPort;
        MessageKey      : pIntuiMessage;
        DetailPen,
        BlockPen        : Byte;         { for bar/border/gadget rendering }
        CheckMark       : Pointer;
        ScreenTitle     : PChar;        { if non-null, Screen title when Window is active }
        GZZMouseX       : Integer;
        GZZMouseY       : Integer;
        GZZWidth        : Integer;
        GZZHeight       : Word;
        ExtData         : Pointer;
        UserData        : Pointer;      { general-purpose pointer to User data extension }
        WLayer          : Pointer;
        IFont           : Pointer;
        MoreFlags       : Longint;
    end;


    pConUnit = ^tConUnit;
    tConUnit = packed record
        cu_MP   : tMsgPort;
        cu_Window       : Pointer;      { (WindowPtr) intuition window bound to this unit }
        cu_XCP          : Integer;      { character position }
        cu_YCP          : Integer;
        cu_XMax         : Integer;      { max character position }
        cu_YMax         : Integer;
        cu_XRSize       : Integer;      { character raster size }
        cu_YRSize       : Integer;
        cu_XROrigin     : Integer;      { raster origin }
        cu_YROrigin     : Integer;
        cu_XRExtant     : Integer;      { raster maxima }
        cu_YRExtant     : Integer;
        cu_XMinShrink   : Integer;      { smallest area intact from resize process }
        cu_YMinShrink   : Integer;
        cu_XCCP         : Integer;      { cursor position }
        cu_YCCP         : Integer;
        cu_KeyMapStruct : Pointer;
        cu_TabStops     : Array [0..80-1] of Word;
        cu_Mask         : Shortint;
        cu_FgPen        : Shortint;
        cu_BgPen        : Shortint;
        cu_AOLPen       : Shortint;
        cu_DrawMode     : Shortint;
        cu_AreaPtSz     : Shortint;
        cu_AreaPtrn     : Pointer;      { cursor area pattern }
        cu_Minterms     : Array [0..7] of Byte; { console minterms }
        cu_Font         : Pointer;      { (TextFontPtr) }
        cu_AlgoStyle    : Byte;
        cu_TxFlags      : Byte;
        cu_TxHeight     : Word;
        cu_TxWidth      : Word;
        cu_TxBaseline   : Word;
        cu_TxSpacing    : Word;
        cu_Modes        : Array [0..(22+7) div 8 - 1] of Byte;
        cu_RawEvents    : Array [0..($15+7) div 8 - 1] of Byte;
    end;

const


   CD_CURRX =  1;
   CD_CURRY =  2;
   CD_MAXX  =  3;
   CD_MAXY  =  4;

   CSI      = chr($9b);

   SIGBREAKF_CTRL_C = 4096;

function AllocVec( size, reqm : Longint ): Pointer;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  size,d0
       MOVE.L  reqm,d1
       MOVE.L  _ExecBase, A6
       JSR -684(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;


function DoPkt(ID : pMsgPort;
               Action, Param1, Param2,
               Param3, Param4, Param5 : Longint) : Longint;
begin
   asm
       MOVEM.L d2/d3/d4/d5/d6/d7/a6,-(A7)
       MOVE.L  ID,d1
       MOVE.L  Action,d2
       MOVE.L  Param1,d3
       MOVE.L  Param2,d4
       MOVE.L  Param3,d5
       MOVE.L  Param4,d6
       MOVE.L  Param5,d7
       MOVE.L  _DOSBase,A6
       JSR -240(A6)
       MOVEM.L (A7)+,d2/d3/d4/d5/d6/d7/a6
       MOVE.L  d0,@RESULT
   end;
end;

procedure FreeVec( memory : Pointer );
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  memory,a1
       MOVE.L  _ExecBase,A6
       JSR -690(A6)
       MOVE.L  (A7)+,A6
   end;
end;


function GetConsoleTask : pMsgPort;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  _DOSBase,A6
       JSR -510(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;


function GetMsg(port : pMsgPort): pMessage;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  port,a0
       MOVE.L  _ExecBase,A6
       JSR -372(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function ModifyIDCMP(window : pWindow;
                     IDCMPFlags : Longint) : Boolean;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  window,a0
       MOVE.L  IDCMPFlags,d0
       MOVE.L  _IntuitionBase,A6
       JSR -150(A6)
       MOVE.L  (A7)+,A6
       TST.L   d0
       bne     @success
       bra     @end
   @success:
       move.b  #1,d0
   @end:
       move.b  d0,@RESULT
   end;
end;

procedure ReplyMsg(mess : pMessage);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  mess,a1
       MOVE.L  _ExecBase,A6
       JSR -378(A6)
       MOVE.L  (A7)+,A6
   end;
end;


function WaitPort(port : pMsgPort): pMessage;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  port,a0
       MOVE.L  _ExecBase,A6
       JSR -384(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

procedure Delay_(ticks : Longint);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  ticks,d1
       MOVE.L  _DOSBase,A6
       JSR -198(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function SetSignal(newSignals, signalMask : Longint) : Longint;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  newSignals,d0
       MOVE.L  signalMask,d1
       MOVE.L  _ExecBase,A6
       JSR -306(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function OpenInfo : pInfoData;
var
   port     :  pMsgPort;
   info     :  pInfoData;
   bptr, d4, d5, d6, d7 :  Longint;
begin
   info  := pInfoData(AllocVec(SizeOf(tInfoData), 1));

   if info <> nil then begin
      port  := GetConsoleTask;
      bptr  := Longint(info) shr 2;

      if port <> nil then begin
         if DoPkt(port, $19, bptr, d4, d5, d6, d7) <> 0 then info := pInfoData(bptr shl 2)
         else port := nil;
      end;

      if port = nil then begin
         FreeVec(info);
         info := nil;
      end;
   end;

   OpenInfo := info;
end;

procedure CloseInfo(var info : pInfoData);
begin
   if info <> nil then begin
      FreeVec(info);
      info := nil;
   end;
end;

function ConData(modus : byte) : integer;
var
   info  :  pInfoData;
   theunit  :  pConUnit;
   pos   :  Longint;
begin
   pos   := 1;
   info  := OpenInfo;

   if info <> nil then begin
      theunit  := pConUnit((pIoStdReq(info^.id_InUse))^.io_Unit);

      case modus of
         CD_CURRX :  pos   := theunit^.cu_XCP;
         CD_CURRY :  pos   := theunit^.cu_YCP;
         CD_MAXX  :  pos   := theunit^.cu_XMax;
         CD_MAXY  :  pos   := theunit^.cu_YMax;
      end;

      CloseInfo(info);
   end;

   ConData := pos + 1;
end;

function WhereX : integer;
begin
   WhereX := ConData(CD_CURRX);
end;

function WhereY : integer;
begin
   WhereY := ConData(CD_CURRY);
end;

function maxx : integer;
begin
   maxx := ConData(CD_MAXX);
end;

function maxy : integer;
begin
   maxy := ConData(CD_MAXY);
end;

procedure GotoXY(x, y : integer);
var
   mx, my : integer;
begin
   mx := maxx;
   my := maxy;

   if x < 1 then x := wherex
   else if x > mx then x := mx;

   if y < 1 then y := wherey
   else if y > my then y := my;

   Write(CSI, y, ';', x, 'H');
end;

procedure CursorOff;
begin
   Write(CSI,'0 p');
end;

procedure CursorOn;
begin
   Write(CSI,'1 p');
end;

procedure ClrScr;
begin
   Write(Chr($0c));
end;

function ReadKey : char;
const
   IDCMP_VANILLAKEY = $00200000;
   IDCMP_RAWKEY     = $00000400;
var
   info  :  pInfoData;
   win   :  pWindow;
   imsg  :  pIntuiMessage;
   msg   :  pMessage;
   key   :  char;
   idcmp, vanil   :  Longint;
begin
   key   := #0;
   info  := OpenInfo;

   if info <> nil then begin
      win   := pWindow(pConUnit((pIoStdReq(info^.id_InUse))^.io_Unit)^.cu_Window);
      idcmp := win^.IDCMPFlags;
      vanil := IDCMP_VANILLAKEY or IDCMP_RAWKEY;

      ModifyIDCMP(win, (idcmp or vanil));

      repeat
         msg   := WaitPort(win^.UserPort);
         imsg  := pIntuiMessage(GetMsg(win^.UserPort));

         if (imsg^.Class_ = IDCMP_VANILLAKEY) or (imsg^.Class_ = IDCMP_RAWKEY) then key := char(imsg^.Code);

         ReplyMsg(pMessage(imsg));
      until key <> #0;

      repeat
         msg   := GetMsg(win^.UserPort);

         if msg <> nil then ReplyMsg(msg);
      until msg = nil;

      ModifyIDCMP(win, idcmp);

      CloseInfo(info);
   end;

   ReadKey := key;
end;

function KeyPressed : Boolean;
const
   IDCMP_VANILLAKEY = $00200000;
   IDCMP_RAWKEY     = $00000400;
var
   info  :  pInfoData;
   win   :  pWindow;
   imsg  :  pIntuiMessage;
   msg   :  pMessage;
   idcmp, vanil   :  Longint;
   ispressed : Boolean;
begin
   ispressed := False;
   info  := OpenInfo;

   if info <> nil then begin
      win   := pWindow(pConUnit((pIoStdReq(info^.id_InUse))^.io_Unit)^.cu_Window);
      idcmp := win^.IDCMPFlags;
      vanil := IDCMP_VANILLAKEY or IDCMP_RAWKEY;

      ModifyIDCMP(win, (idcmp or vanil));

      msg   := WaitPort(win^.UserPort);
      imsg  := pIntuiMessage(GetMsg(win^.UserPort));

      if (imsg^.Class_ = IDCMP_VANILLAKEY) or (imsg^.Class_ = IDCMP_RAWKEY) then ispressed := true;

      ReplyMsg(pMessage(imsg));

      repeat
         msg   := GetMsg(win^.UserPort);

         if msg <> nil then ReplyMsg(msg);
      until msg = nil;

      ModifyIDCMP(win, idcmp);

      CloseInfo(info);
   end;

   KeyPressed := ispressed;
end;

procedure TextColor(color : byte);
begin
   Write(CSI, '3', color, 'm');
end;

procedure TextBackground(color : byte);
begin
   Write(CSI, '4', color, 'm');
end;

procedure window(X1,Y1,X2,Y2 : Integer);
begin
end;

procedure assigncrt(var f : text);
begin
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

procedure cursorbig;
begin
end;

procedure lowvideo;
begin
end;

procedure highvideo;
begin
end;

procedure nosound;
begin
end;

procedure sound(hz : word);
begin
end;

procedure delay(DTime : Word);
var
    dummy : Longint;
begin
    dummy := trunc((real(DTime) / 1000.0) * 50.0);
    Delay_(dummy);
end;

function CheckBreak : boolean;
begin
   if (SetSignal(0, 0) and SIGBREAKF_CTRL_C) = SIGBREAKF_CTRL_C then
      CheckBreak := true
   else
      CheckBreak := false;
end;

procedure textmode(mode : integer);
begin
end;

procedure normvideo;
begin
end;

end.





