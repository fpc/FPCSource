{
    $Id: window.pas,v 1.2 2005/02/14 17:13:10 peter Exp $

    Opening a window with optional custom screen, and
    basic event-driven drawing into the window
    Free Pascal for MorphOS example

    Copyright (C) 2004 by Karoly Balogh

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ * 2004.12.10 * }

program window;

uses exec, intuition, graphics, utility;

{ * Enable this to run on custom screen. If it's not * }
{ * enabled, window will open on default pubscreen   * }
{ DEFINE CUSTOMSCREEN}


var
{$IFDEF CUSTOMSCREEN}
  myScreen  : PScreen;
{$ENDIF}
  myWindow  : PWindow;
  IMesg     : PIntuiMessage;
  Quit      : Boolean;
  LeftButton: Boolean;

  msg_Class : Cardinal;
  msg_Code  : Word;

  X_Pos     : Integer;
  Y_Pos     : Integer;

const
  ERRMSG_NOINTUI = 'Unable to open intuition.library V50!';
  ERRMSG_NOGFX   = 'Unable to open graphics.library V50!';
  ERRMSG_NOSCREEN= 'Can''t open screen!';
  ERRMSG_NOWINDOW= 'Can''t open window!';


procedure ShutDown(exitString: String; code: LongInt);
begin
  if assigned(myWindow) then CloseWindow(myWindow);
{$IFDEF CUSTOMSCREEN}
  if assigned(myScreen) then CloseScreen(myScreen);
{$ENDIF}

  { * We're using library functions built into units, so it's   * }
  { * not needed to close libs here. If you're using additional * }
  { * libs, add code to close them here.                        * }

  if exitString<>'' then writeln(exitString);
  Halt(code);
end;

procedure Init;
begin

  { * Opening the needed libs with builtin functions. * }
  if Not InitIntuitionLibrary then ShutDown(ERRMSG_NOINTUI,20);
  if Not InitGraphicsLibrary then ShutDown(ERRMSG_NOGFX,20);

{$IFDEF CUSTOMSCREEN}
  myScreen:=NIL;
  { * Opening our custom screen * }
  myScreen:=OpenScreenTags(NIL,[SA_Width,640,SA_Height,480,
                                SA_Depth,24,
                                SA_Title,DWord(PChar('Free Pascal Rules!')),
                                TAG_DONE]);
  if myScreen=NIL then ShutDown(ERRMSG_NOSCREEN,20);
{$ENDIF}

  myWindow:=NIL;
  { * We open our window here. * }
  myWindow:=OpenWindowTags(NIL,[WA_Left,0,WA_Top,0,
                                WA_Width,400,WA_Height,300,
                                WA_Title,DWord(PChar('Free Pascal Test')),
                                WA_IDCMP,(IDCMP_CLOSEWINDOW or IDCMP_MOUSEBUTTONS or
                                          IDCMP_MOUSEMOVE),
                                WA_Flags,(WFLG_SIMPLE_REFRESH or WFLG_NOCAREREFRESH or
                                          WFLG_ACTIVATE or WFLG_REPORTMOUSE or
                                          WFLG_CLOSEGADGET or WFLG_SIZEGADGET or
                                          WFLG_SIZEBBOTTOM or WFLG_GIMMEZEROZERO or
                                          WFLG_DRAGBAR),
{$IFDEF CUSTOMSCREEN}
                                WA_CustomScreen,DWord(myScreen),
{$ENDIF}
                                TAG_END]);
  if myWindow=NIL then ShutDown(ERRMSG_NOWINDOW,20);

  SetAPen(myWindow^.RPort,1);
end;


begin
  Init;
  Quit:=False;
  LeftButton:=False;

  repeat
    WaitPort(myWindow^.UserPort);
    IMesg:=PIntuiMessage(GetMsg(myWindow^.UserPort));
    if IMesg<>NIL then Begin

      { * It's recommended to copy contents of incoming * }
      { * message, and reply it as soon as possible.    * }
      msg_Class:=IMesg^.IClass;
      msg_Code :=IMesg^.Code;
      X_Pos    :=IMesg^.MouseX - myWindow^.BorderLeft;
      Y_Pos    :=IMesg^.MouseY - myWindow^.BorderTop;

      ReplyMsg(Pointer(IMesg));

      { * Handle different kind of messages here. * }
      case msg_Class of
        IDCMP_CLOSEWINDOW:
          quit:=True;
        IDCMP_MOUSEBUTTONS:
          case msg_Code of
            SELECTDOWN:
              LeftButton:=True;
            SELECTUP:
              LeftButton:=False;
          end;
        IDCMP_MOUSEMOVE:
          begin
            { * Draw when left button is pressed, and mouse is moved. * }
            if LeftButton then WritePixel(myWindow^.RPort,X_Pos,Y_Pos);
          end;
      end;
    end;
  until Quit;

  ShutDown('',0);
end.

{
  $Log: window.pas,v $
  Revision 1.2  2005/02/14 17:13:10  peter
    * truncate log

  Revision 1.1  2004/12/14 22:07:02  karoly
    * initial revision

}
