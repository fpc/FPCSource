{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1997 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit Crt;

INTERFACE

    const
       { screen modes }
       bw40 = 0;
       co40 = 1;
       bw80 = 2;
       co80 = 3;
       mono = 7;
       font8x8 = 256;

       { screen color, fore- and background }
       black = 0;
       blue = 1;
       green = 2;
       cyan = 3;
       red = 4;
       magenta = 5;
       brown = 6;
       lightgray = 7;

       { only foreground }
       darkgray = 8;
       lightblue = 9;
       lightgreen = 10;
       lightcyan = 11;
       lightred = 12;
       lightmagenta = 13;
       yellow = 14;
       white = 15;

       { blink flag }
       blink = $80;

    var
       { for compatibility }
       checkbreak,checkeof,checksnow : boolean;

       { works in another way than in TP }
       { true: cursor is set with direct port access }
       { false: cursor is set with a bios call       }
       directvideo : boolean;

       lastmode : word; { screen mode}
       textattr : byte; { current text attribute }
       windmin : word; { upper right corner of the CRT window }
       windmax : word; { lower left corner of the CRT window }

    function keypressed : boolean;
    function readkey : char;
    procedure gotoxy(x,y : integer);
    procedure window(left,top,right,bottom : byte);
    procedure clrscr;
    procedure textcolor(color : byte);
    procedure textbackground(color : byte);
    procedure assigncrt(var f : text);
    function wherex : integer;
    function wherey : integer;
    procedure delline;
    procedure delline(line : byte);
    procedure clreol;
    procedure insline;
    procedure cursoron;
    procedure cursoroff;
    procedure cursorbig;
    procedure lowvideo;
    procedure highvideo;
    procedure nosound;
    procedure sound(hz : word);
    procedure delay(ms : longint);
    procedure textmode(mode : integer);
    procedure normvideo;

  implementation

Type

{$PACKRECORDS 4}
{ returned by Info(), must be on a 4 byte boundary }

    pInfoData = ^tInfoData;
    tInfoData = record
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
  tNode = Record
    ln_Succ,                { * Pointer to next (successor) * }
    ln_Pred  : pNode;       { * Pointer to previous (predecessor) * }
    ln_Type  : Byte;
    ln_Pri   : Shortint;    { * Priority, for sorting * }
    ln_Name  : PChar;       { * ID string, null terminated * }
  End;  { * Note: Integer aligned * }

{$PACKRECORDS NORMAL}

{ normal, full featured list }

    pList = ^tList;
    tList = record
    lh_Head     : pNode;
    lh_Tail     : pNode;
    lh_TailPred : pNode;
    lh_Type     : Byte;
    l_pad       : Byte;
    end;

    pMsgPort = ^tMsgPort;
    tMsgPort = record
    mp_Node     : tNode;
    mp_Flags    : Byte;
    mp_SigBit   : Byte;      { signal bit number    }
    mp_SigTask  : Pointer;   { task to be signalled (TaskPtr) }
    mp_MsgList  : tList;     { message linked list  }
    end;

    pMessage = ^tMessage;
    tMessage = record
    mn_Node       : tNode;
    mn_ReplyPort  : pMsgPort;   { message reply port }
    mn_Length     : Word;       { message len in bytes }
    end;

    pIOStdReq = ^tIOStdReq;
    tIOStdReq = record
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
    tIntuiMessage = record
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
    tWindow = record
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
    tConUnit = record
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


function AllocVec( size, reqm : Longint ): Pointer; Assembler;
asm
    MOVE.L  A6,-(A7)
    MOVE.L  _ExecBase,A6
    MOVE.L  size,d0
    MOVE.L  reqm,d1
    JSR -684(A6)
    MOVE.L  (A7)+,A6
end;

function DoPkt(ID : pMsgPort;
               Action, Param1, Param2,
               Param3, Param4, Param5 : Longint) : Longint; Assembler;
asm
    MOVEM.L d2/d3/d4/d5/d6/d7/a6,-(A7)
    MOVE.L  _DOSBase,A6
    MOVE.L  ID,d1
    MOVE.L  Action,d2
    MOVE.L  Param1,d3
    MOVE.L  Param2,d4
    MOVE.L  Param3,d5
    MOVE.L  Param4,d6
    MOVE.L  Param5,d7
    JSR -240(A6)
    MOVEM.L (A7)+,d2/d3/d4/d5/d6/d7/a6
end;

procedure FreeVec( memory : Pointer ); Assembler;
asm
    MOVE.L  A6,-(A7)
    MOVE.L  _ExecBase,A6
    MOVE.L  memory,a1
    JSR -690(A6)
    MOVE.L  (A7)+,A6
end;
                                      

function GetConsoleTask : pMsgPort; Assembler;
asm
    MOVE.L  A6,-(A7)
    MOVE.L  _DOSBase,A6
    JSR -510(A6)
    MOVE.L  (A7)+,A6
end;
                            

function GetMsg(port : pMsgPort): pMessage; Assembler;
asm
    MOVE.L  A6,-(A7)
    MOVE.L  _ExecBase,A6
    MOVE.L  port,a0
    JSR -372(A6)
    MOVE.L  (A7)+,A6
end;
                                
function ModifyIDCMP(window : pWindow;
                     IDCMPFlags : Longint) : Boolean; Assembler;
asm
    MOVE.L  A6,-(A7)
    MOVE.L  _IntuitionBase,A6
    MOVE.L  window,a0
    MOVE.L  IDCMPFlags,d0
    JSR -150(A6)
    MOVE.L  (A7)+,A6
    TST.L   d0
    SNE     d0
end;
                                                    
procedure ReplyMsg(mess : pMessage); Assembler;
asm
    MOVE.L  A6,-(A7)
    MOVE.L  _ExecBase,A6
    MOVE.L  mess,a1
    JSR -378(A6)
    MOVE.L  (A7)+,A6
end;
                               

function WaitPort(port : pMsgPort): pMessage; Assembler;
asm
    MOVE.L  A6,-(A7)
    MOVE.L  _ExecBase,A6
    MOVE.L  port,a0
    JSR -384(A6)
    MOVE.L  (A7)+,A6
end;
                        
procedure Delay_(ticks : Integer); Assembler;
asm
    MOVE.L  A6,-(A7)
    MOVE.L  _DOSBase,A6
    MOVE.L  ticks,d1
    JSR -198(A6)
    MOVE.L  (A7)+,A6
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

function wherex : integer;
begin
   wherex := ConData(CD_CURRX);
end;

function wherey : integer;
begin
   wherey := ConData(CD_CURRY);
end;

function maxx : integer;
begin
   maxx := ConData(CD_MAXX);
end;

function maxy : integer;
begin
   maxy := ConData(CD_MAXY);
end;

procedure gotoxy(x, y : integer);
var
   mx, my : integer;
begin
   mx := maxx;
   my := maxy;
   
   if x < 1 then x := wherex
   else if x > mx then x := mx;
   
   if y < 1 then y := wherey
   else if y > my then y := my;
   
   Write($9b, y, ';', x, 'H');
end;

procedure cursoroff;
begin
   Write($9b,'0 p');
end;

procedure cursoron;
begin
   Write($9b,'1 p');
end;

procedure clrscr;
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
   idcmp, vanil   :  longint;
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
      until key <> char(0);

      repeat
         msg   := GetMsg(win^.UserPort);

         if msg <> nil then ReplyMsg(msg);
      until msg = nil;

      ModifyIDCMP(win, idcmp);

      CloseInfo(info);
   end;

   ReadKey := key;
end;

procedure textcolor(fgpen : byte);
begin
   Write($9b, '3', fgpen, 'm');
end;

procedure textbackground(bgpen : byte);
begin
   Write($9b, '4', bgpen, 'm');
end;

function keypressed : boolean;
begin
   keypressed := true;
end;

procedure window(left,top,right,bottom : byte);
begin
end;

procedure assigncrt(var f : text);
begin
end;

procedure delline;
begin
   Write($9b,'X');
end;

procedure delline(line : byte);
begin
   Write($9b,'X');
end;

procedure clreol;
begin
   Write($9b,'K');
end;

procedure insline;
begin
   Write($9b,'1 L');
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

{  MsDos have 1000 ticks per second
   and Amiga only 50, so we have to
   do some calcs here.
   The min value this procedure will
   handle is 20, (less you will get 0)
   this will be 1 tick in Amiga. If
   you want to use amigados delay just
   use Delay_.   }
procedure delay(ms : longint);
var
    dummy : integer;
begin
    dummy := trunc((real(ms) / 1000.0) * 50.0);
    Delay_(dummy);
end;

procedure textmode(mode : integer);
begin
end;

procedure normvideo;
begin
end;

end.





