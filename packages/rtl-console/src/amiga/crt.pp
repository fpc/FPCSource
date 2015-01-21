{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Nils Sjoholm and Carl Eric Codere

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit Crt;

{--------------------------------------------------------------------}
{ LEFT TO DO:                                                        }
{--------------------------------------------------------------------}
{ o Write special characters are not recognized                      }
{ o Write does not take care of window coordinates yet.              }
{ o Read does not recognize the special editing characters           }
{ o Read does not take care of window coordinates yet.               }
{ o Readkey extended scancode is not correct yet                     }
{ o Color mapping only works for 4 colours                           }
{ o ClrScr, DeleteLine, InsLine do not work with window coordinates  }
{--------------------------------------------------------------------}



Interface

Const
{ Controlling consts }
  Flushing=false;                       {if true then don't buffer output}
  ScreenWidth  = 80;
  ScreenHeight = 25;

{$i crth.inc}

Implementation

uses
  exec, amigados, conunit, intuition;

var
  maxcols,maxrows : longint;

CONST
  { This is used to make sure that readkey returns immediately }
  { if keypressed was used beforehand.                         }
  KeyPress : char = #0;
  _LVODisplayBeep = -96;

(*
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
        IClass          : Longint;
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
*)
    const

    M_LNM               = 20;           { linefeed newline mode }
    PMB_ASM     = M_LNM + 1;    { internal storage bit for AS flag }
    PMB_AWM     = PMB_ASM + 1;  { internal storage bit for AW flag }
    MAXTABS     = 80;
    IECLASS_MAX = $15;

(*
type

    pKeyMap = ^tKeyMap;
    tKeyMap = packed record
        km_LoKeyMapTypes        : Pointer;
        km_LoKeyMap             : Pointer;
        km_LoCapsable           : Pointer;
        km_LoRepeatable         : Pointer;
        km_HiKeyMapTypes        : Pointer;
        km_HiKeyMap             : Pointer;
        km_HiCapsable           : Pointer;
        km_HiRepeatable         : Pointer;
    end;



    pConUnit = ^tConUnit;
    tConUnit = packed record
        cu_MP   : tMsgPort;
        { ---- read only variables }
        cu_Window       : Pointer;      { (WindowPtr) intuition window bound to this unit }
        cu_XCP          : Integer;        { character position }
        cu_YCP          : Integer;
        cu_XMax         : Integer;        { max character position }
        cu_YMax         : Integer;
        cu_XRSize       : Integer;        { character raster size }
        cu_YRSize       : Integer;
        cu_XROrigin     : Integer;        { raster origin }
        cu_YROrigin     : Integer;
        cu_XRExtant     : Integer;        { raster maxima }
        cu_YRExtant     : Integer;
        cu_XMinShrink   : Integer;        { smallest area intact from resize process }
        cu_YMinShrink   : Integer;
        cu_XCCP         : Integer;        { cursor position }
        cu_YCCP         : Integer;

   { ---- read/write variables (writes must must be protected) }
   { ---- storage for AskKeyMap and SetKeyMap }

        cu_KeyMapStruct : tKeyMap;

   { ---- tab stops }

        cu_TabStops     : Array [0..MAXTABS-1] of Word;
                                { 0 at start, -1 at end of list }

   { ---- console rastport attributes }

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

   { ---- console MODES and RAW EVENTS switches }

        cu_Modes        : Array [0..(PMB_AWM+7) div 8 - 1] of Byte;
                                { one bit per mode }
        cu_RawEvents    : Array [0..(IECLASS_MAX+7) div 8 - 1] of Byte;
    end;
*)
const


   CD_CURRX =  1;
   CD_CURRY =  2;
   CD_MAXX  =  3;
   CD_MAXY  =  4;

   CSI      = chr($9b);

   SIGBREAKF_CTRL_C = 4096;

{function AllocVec( size, reqm : Longint ): Pointer;
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
end;}

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

function WhereX : tcrtcoord;
begin
   WhereX := Byte(ConData(CD_CURRX))-lo(windmin);
end;

function realx: byte;
begin
   RealX := Byte(ConData(CD_CURRX));
end;

function realy: byte;
begin
 RealY := Byte(ConData(CD_CURRY));
end;

function WhereY : tcrtcoord;
begin
   WhereY := Byte(ConData(CD_CURRY))-hi(windmin);
end;

function screencols : integer;
begin
   screencols := ConData(CD_MAXX);
end;

function screenrows : integer;
begin
   screenrows := ConData(CD_MAXY);
end;


 procedure Realgotoxy(x,y : integer);
 begin
       Write(CSI, y, ';', x, 'H');
 end;


 procedure gotoxy(x,y : tcrtcoord);
 begin
        if (x<1) then
          x:=1;
        if (y<1) then
          y:=1;
        if y+hi(windmin)-2>=hi(windmax) then
          y:=hi(windmax)-hi(windmin)+1;
        if x+lo(windmin)-2>=lo(windmax) then
          x:=lo(windmax)-lo(windmin)+1;
        Write(CSI, y+hi(windmin), ';', x+lo(windmin), 'H');
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
   if KeyPress <> #0 then
    Begin
      ReadKey:=KeyPress;
      KeyPress:=#0;
      exit;
    end;
   info  := OpenInfo;

   if info <> nil then begin
      win   := pWindow(pConUnit((pIoStdReq(info^.id_InUse))^.io_Unit)^.cu_Window);
      idcmp := win^.IDCMPFlags;
      vanil := IDCMP_VANILLAKEY or IDCMP_RAWKEY;

      ModifyIDCMP(win, (idcmp or vanil));

      repeat
         msg   := WaitPort(win^.UserPort);
         imsg  := pIntuiMessage(GetMsg(win^.UserPort));

         if (imsg^.IClass = IDCMP_VANILLAKEY) then
              key := char(imsg^.Code)
         else
         if (imsg^.IClass = IDCMP_RAWKEY) then
              key := char(imsg^.Code);

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
   KeyPress := #0;
   ispressed := False;
   info  := OpenInfo;

   if info <> nil then begin
      win   := pWindow(pConUnit((pIoStdReq(info^.id_InUse))^.io_Unit)^.cu_Window);
      idcmp := win^.IDCMPFlags;
      vanil := IDCMP_VANILLAKEY or IDCMP_RAWKEY;

      ModifyIDCMP(win, (idcmp or vanil));

      msg   := WaitPort(win^.UserPort);
      imsg  := pIntuiMessage(GetMsg(win^.UserPort));

      if (imsg^.IClass = IDCMP_VANILLAKEY) or (imsg^.IClass = IDCMP_RAWKEY) then
      Begin
        ispressed := true;
        KeyPress := char(imsg^.Code)
      end;

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
   TextAttr := (TextAttr and $70) or color;
   Write(CSI, '3', color, 'm');
end;

procedure TextBackground(color : byte);
begin
   Textattr:=(textattr and $8f) or ((color and $7) shl 4);
   Write(CSI, '4', color, 'm');
end;

procedure Window(X1,Y1,X2,Y2: Byte);
 begin
   if (x1<1) or (x2>screencols) or (y2>screenrows) or
     (x1>x2) or (y1>y2) then
       exit;
   windmin:=(x1-1) or ((y1-1) shl 8);
   windmax:=(x2-1) or ((y2-1) shl 8);
   gotoxy(1,1);
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

procedure delay(ms : Word);
var
    dummy : Longint;
begin
    dummy := trunc((real(ms) / 1000.0) * 50.0);
    DOSDelay(dummy);
end;

{function CheckBreak : boolean;
begin
   if (SetSignal(0, 0) and SIGBREAKF_CTRL_C) = SIGBREAKF_CTRL_C then
      CheckBreak := true
   else
      CheckBreak := false;
end;}

procedure textmode(mode : word);
begin
       lastmode:=mode;
       mode:=mode and $ff;
       windmin:=0;
       windmax:=(screencols-1) or ((screenrows-1) shl 8);
       maxcols:=screencols;
       maxrows:=screenrows;
end;

procedure normvideo;
begin
end;

function GetTextBackground : byte;
var
   info  :  pInfoData;
   pen   :  byte;
begin
   pen   := 1;
   info  := OpenInfo;

   if info <> nil then begin
      pen   := pConUnit((pIoStdReq(info^.id_InUse))^.io_Unit)^.cu_BgPen;

      CloseInfo(info);
   end;

   GetTextBackground := pen;
end;

function GetTextColor : byte;
var
   info  :  pInfoData;
   pen   :  byte;
begin
   pen   := 1;
   info  := OpenInfo;

   if info <> nil then begin
      pen   := pConUnit((pIoStdReq(info^.id_InUse))^.io_Unit)^.cu_FgPen;

      CloseInfo(info);
   end;

   GetTextColor   := pen;
end;


{*****************************************************************************
                          Read and Write routines
*****************************************************************************}
{ Problem here: Currently all these routines are not implemented because of how }
{ the console device works. Because w low level write is required to change the }
{ position of the cursor, and since the CrtWrite is assigned as the standard    }
{ write routine, a recursive call will occur                                    }

{ How to fix this:                                                              }
{  At startup make a copy of the Output handle, and then use this copy to make  }
{  low level positioning calls. This does not seem to work yet.                 }



   Function CrtWrite(var f : textrec):integer;

      var
         i,col,row : longint;
         c : char;
         buf: array[0..1] of char;

      begin
         col:=realx;
         row:=realy;
         inc(row);
         inc(col);
         for i:=0 to f.bufpos-1 do
           begin
              c:=f.buffer[i];
              case ord(c) of
                 10 : begin
                         inc(row);
                      end;
                 13 : begin
                         col:=lo(windmin)+1;
                     end;
                 8 : if col>lo(windmin)+1 then
                       begin
                          dec(col);
                       end;
                 7 : begin
                         { beep }
                         asm
                           move.l a6,d6               { save base pointer    }
                           move.l _IntuitionBase,a6   { set library base     }
                           sub.l  a0,a0
                           jsr    _LVODisplayBeep(a6)
                           move.l d6,a6               { restore base pointer }
                         end;
                      end;
              else
                 begin
                   buf[0]:=c;
                   realgotoxy(row,col);
                   {do_write(f.handle,longint(@buf[0]),1);}
                   inc(col);
                 end;
              end;
              if col>lo(windmax)+1 then
                begin
                   col:=lo(windmin)+1;
                   inc(row);
                end;
              while row>hi(windmax)+1 do
                begin
                   delline;
                   dec(row);
                end;
           end;
         f.bufpos:=0;
         realgotoxy(row-1,col-1);
         CrtWrite:=0;
      end;

   Function CrtClose(Var F: TextRec): Integer;
     Begin
       F.Mode:=fmClosed;
       CrtClose:=0;
     End;

   Function CrtOpen(Var F: TextRec): Integer;
     Begin
       If F.Mode = fmOutput Then
        CrtOpen:=0
       Else
        CrtOpen:=5;
     End;

   Function CrtRead(Var F: TextRec): Integer;
     Begin
       {f.bufend:=do_read(f.handle,longint(f.bufptr),f.bufsize);}
       f.bufpos:=0;
       CrtRead:=0;
     End;

   Function CrtInOut(Var F: TextRec): Integer;
     Begin
       Case F.Mode of
        fmInput: CrtInOut:=CrtRead(F);
        fmOutput: CrtInOut:=CrtWrite(F);
       End;
     End;

   procedure assigncrt(var f : text);
     begin
   {     TextRec(F).Mode:=fmClosed;
        TextRec(F).BufSize:=SizeOf(TextBuf);
        TextRec(F).BufPtr:=@TextRec(F).Buffer;
        TextRec(F).BufPos:=0;
        TextRec(F).OpenFunc:=@CrtOpen;
        TextRec(F).InOutFunc:=@CrtInOut;
        TextRec(F).FlushFunc:=@CrtInOut;
        TextRec(F).CloseFunc:=@CrtClose;
        TextRec(F).Name[0]:='.';
        TextRec(F).Name[1]:=#0;}
     end;


var
  old_exit : pointer;

procedure crt_exit;
begin
  { Restore default colors }
  write(CSI,'0m');
  exitproc:=old_exit;
end;


Begin
   old_exit:=exitproc;
   exitproc:=@crt_exit;
   { load system variables to temporary variables to save time }
   maxcols:=screencols;
   maxrows:=screenrows;
   { Set the initial text attributes }
   { Text background }
   Textattr:=(textattr and $8f) or ((GetTextBackGround and $7) shl 4);
   { Text foreground }
   TextAttr := (TextAttr and $70) or GetTextColor;
   { set output window }
   windmax:=(maxcols-1) or (( maxrows-1) shl 8);


   { Get a copy of the standard      }
   { output handle, and when using   }
   { direct console calls, use this  }
   { handle instead.                 }
{   assigncrt(Output);
   TextRec(Output).mode:=fmOutput;}
end.
