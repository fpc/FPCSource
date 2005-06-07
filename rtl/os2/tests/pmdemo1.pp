{****************************************************************************


                   Copyright (c) 1999-2000 by Florian Klaempfl

 ****************************************************************************}

{ Sample program for FPC under OS/2 }
{ Classic Hello world in PM version }

{$R pmdemo1.res}

(* PMDEMO1.RES has to be compiled from PMDEMO1.RC using "rc -r PMDEMO1.RC". *)

program pmdemo1;

  uses
     os2def,pmwin;

  var
     frame,client : cardinal;
     ab : cardinal;
     mq : cardinal;
     msg : QMSG;

  const
     frameflags : longint = FCF_TITLEBAR+FCF_SYSMENU+FCF_SIZEBORDER+
                            FCF_MINBUTTON+FCF_MAXBUTTON+FCF_SHELLPOSITION+
                            FCF_TASKLIST+FCF_MENU;

  function clientwndproc(window : cardinal;msg : longint;mp1,mp2 : pointer) :
    pointer; cdecl; export;

    const
       text = 'Hello world by OS/2 and FPC';

    var
       ps : cardinal;
       rcl : RECTL;

    begin
       {clientwndproc:=nil;      }
       case msg of
{         WM_CREATE : DosBeep(200,500);}
          WM_PAINT : begin
                        ps:=WinBeginPaint(window,0,nil);
                        WinQueryWindowRect(window,@rcl);
                        WinDrawText(ps,-1,text,@rcl,0,7,$8500);
                        WinEndPaint(ps);
                     end;
          WM_COMMAND : case lo(longint(mp1)) of
                          {101 : DosBeep(4500,1000);}
                          109 : WinPostMsg(0,WM_QUIT,nil,nil);
                          201 : WinMessageBox(cardinal(1),cardinal(1),
                             'HelloPM from FPC',
                             'About',0,MB_ICONEXCLAMATION+MB_MOVEABLE);
                       end;
       else
          clientwndproc:=WinDefWindowProc(window,msg,mp1,mp2);
       end;
    end;

 begin
    ab:=WinInitialize(0);
    mq:=WinCreateMsgQueue(ab,0);
    WinRegisterClass(ab,'HELLOPM',proc(@clientwndproc),4,0);
    frame:=WinCreateStdWindow(cardinal(1),WS_VISIBLE,@frameflags,'HELLOPM',
      'PMDemo 1',WS_VISIBLE,0,1,@client);
    while WinGetMsg(ab,@msg,0,0,0) do
      WinDispatchMsg(ab,@msg);
    WinDestroyWindow(frame);
    WinDestroyMsgQueue(mq);
    WinTerminate(ab);
 end.
