{****************************************************************************

    $Id$

                             DIVE example code
             Copyright (c) 1999-2000 by Karoly Balogh (aka Charlie/INQ)

****************************************************************************}

{$ASMMODE INTEL}

{ lame code, but who cares? :) please don't laugh it out. :) }

Program DiveTry;

{$I-}

Uses Os2Def,PMWin,DIVE,DOSCalls;

{ * main program * }
Const ClassName      = 'MYVIEW';
      idClientWindow = 11000;

      WinFlags       : Cardinal = fcf_TitleBar+fcf_SysMenu+fcf_SizeBorder+
                                  fcf_MinMax+fcf_TaskList+fcf_NoByteAlign;
Var Anchor, MsgQue : Cardinal;
    Message        : TQMsg;
    Frame, Client  : Cardinal;

    Picture     : Pointer;
    PicSize     : DWord;

    DIVEHandle      : HDIVE;
    FrameBuffer     : Pointer;
    DIVEImageBuffer : DWord;
    DIVEColorFormat : DWord;

    idBlitThread : DWord;

    DIVEBlitSetup   : TSetup_Blitter;

{ * The exported procedure * }
Function ClientWindowProc(Window, Msg : DWord; MP1, MP2: Pointer) : Pointer; cdecl; Export;
Var Li  : LongInt;
    PS  : DWord;
    R   : TRectL;
    P   : TPointL;
    Rgn : DWord;
    DIVEBlitSetup : TSetup_Blitter;
    DestFormat : DWord;
Begin
 ClientWindowProc:=Nil;
 Case Msg Of
   wm_Paint: Begin
               PS:=WinBeginPaint(Window,0,@r);
               WinFillRect(PS,@r,SYSCLR_WINDOW);

               Asm
                MOV EAX,'565R'
                MOV DestFormat,EAX
               End;

               With DIVEBlitSetup Do Begin
                 ulStructLen := SizeOf(DIVEBlitSetup); { * Whole record used * }
                 fInvert     := 0;  { * Not inverted * }
                 { * This is the mark for 8 bytes * }
                 fccSrcColorFormat:=DIVEColorFormat; { * Source data format * }
                 ulSrcWidth:=640;  { * Width in pels  * }
                 ulSrcHeight:=480; { * Height in pels * }
                 ulSrcPosX:=0; { * X Position of source data * }
                 ulSrcPosY:=0; { * Y Position of source data * }
                 { * This is the mark for 28 bytes * }
                 ulDitherType:=0; { * Dither type * }
                 { * 32 byte mark * }
                 fccDstColorFormat:=DestFormat; { * Destination color format   * }
                 ulDstWidth:=640; { * Destination width in pels  * }
                 ulDstHeight:=480; { * Destination height in pels * }
                 lDstPosX:=0;
                 lDstPosY:=0;
                 { * 52 byte mark * }
                 lScreenPosX:=0;
                 lScreenPosY:=0;
                 { * 60 byte mark * }
                 ulNumDstRects:=1;
                 pVisDstRects:=@r; { * This is a pointer to an array of visible rectangles. * }
                 { * 68 bytes = fully used * }
                End;

               If DIVESetupBlitter(DIVEHandle,@DIVEBlitSetup)<>DIVE_Success Then Begin
                 WinMessageBox(HWND_DESKTOP,HWND_DESKTOP,'Cannot set up DIVE blitter!',
                               'DIVE Error!',0,MB_OK Or MB_ERROR Or MB_MOVEABLE);
                End;
               {DIVEBlitImage(DIVEHandle,DIVEImageBuffer,DIVE_Buffer_Screen);}

               WinEndPaint(PS);
              End;
   Else ClientWindowProc:=WinDefWindowProc(Window,Msg,MP1,MP2);
 End;
End;

Procedure DIVEBlitThread;
Begin
End;

Function LoadFiles : Boolean;
Var RAWFile  : File;
Begin
 LoadFiles:=False;

 Assign(RAWFile,'ANGEL.RAW'); { * Opening File * }
 Reset(RAWFile,1);

 If IOResult<>0 Then Exit; { * If File Not Found, Then Exit * }

 PicSize:=FileSize(RAWFile);
 GetMem(Picture,PicSize); { * Allocating Memory * }
 BlockRead(RAWFile,Picture^,PicSize); { * Loading File * }

 If IOResult<>0 Then Exit; { * If File Corrupted, Then Exit * }

 Close(RAWFile);

 LoadFiles:=True;
End;

Begin
 { * PM Init * }
 Anchor:=WinInitialize(0);

 { It might be beneficial to set the second parameter of the following }
 { call to something large, such as 1000.  The OS/2 documentation does }
 { not recommend this, however }
 MsgQue:=WinCreateMsgQueue(Anchor,0);
 If MsgQue=0 Then Halt(254);

 { * Loading Graphics File * }
 If Not LoadFiles Then Begin
   WinMessageBox(HWND_DESKTOP,HWND_DESKTOP,'File Corrupted : ANGEL.RAW',
                 'File Loading Error!',0,MB_OK Or MB_ERROR Or MB_MOVEABLE);
   WinDestroyMsgQueue(MsgQue);
   WinTerminate(Anchor);
  End;

 { * Opening DIVE, exiting if cannot be initialized * }
 If DIVEOpen(DIVEHandle,1,Framebuffer)<>DIVE_Success Then Begin
   WinMessageBox(HWND_DESKTOP,HWND_DESKTOP,'DIVE subsystem cannot be initialized!',
                 'DIVE Error!',0,MB_OK Or MB_ERROR Or MB_MOVEABLE);
   WinDestroyMsgQueue(MsgQue);
   WinTerminate(Anchor);
  End;

 WinRegisterClass(Anchor,ClassName,Proc(@ClientWindowProc),cs_SizeRedraw,SizeOf(Pointer));
 Frame:=WinCreateStdWindow(hwnd_Desktop,0,WinFlags,ClassName,'DIVE with FPC/2 Example',
                           0,0,idClientWindow, Client);

 If (Frame<>0) Then Begin

   {$ASMMODE INTEL}
   Asm { * Tweakin' rules. :) Anyway, can i make something like this in pascal? (Eg. : DWordNum:='ABCD' ?) * }
    MOV EAX,'3BGR'
    MOV DIVEColorFormat,EAX
   End;
   If DIVEAllocImageBuffer(DIVEHandle,DIVEImageBuffer,DIVEColorFormat,640,480,0,Picture)<>DIVE_Success Then Begin
     WinMessageBox(HWND_DESKTOP,HWND_DESKTOP,'DIVE image buffer cannot be allocated!',
                   'DIVE Error!',0,MB_OK Or MB_ERROR Or MB_MOVEABLE);
     DIVEClose(DIVEHandle);
     FreeMem(Picture,PicSize);
     WinDestroyMsgQueue(MsgQue);
     WinTerminate(Anchor);
    End;

   WinSetWindowPos(Frame,0,0,WinQuerySysValue(hwnd_Desktop,sv_CyScreen)-480,
                   640,480,swp_Move+swp_Size+swp_Activate+swp_Show);
   While WinGetMsg(Anchor,Message,0,0,0) Do WinDispatchMsg(Anchor,Message);

   { * Closing DIVE * }
   If DIVEFreeImageBuffer(DIVEHandle,DIVEImageBuffer)<>DIVE_Success Then Begin
     WinMessageBox(HWND_DESKTOP,HWND_DESKTOP,'DIVE image buffer cannot be deallocated!',
                   'DIVE Error!',0,MB_OK Or MB_ERROR Or MB_MOVEABLE);
    End;
   DIVEClose(DIVEHandle);

   WinDestroyWindow(Frame);
  End;

 { * Freeing Up Allocated Memory * }
 FreeMem(Picture,PicSize);

 { * PM Close * }
 WinDestroyMsgQueue(MsgQue);
 WinTerminate(Anchor);
End.

{
  $Log$
  Revision 1.2  2000-06-18 18:34:28  hajny
   * necessary $I- added


}
