{****************************************************************************

                   Copyright (c) 1999-2000 by Florian Kl„mpfl

 ****************************************************************************}

{ Generisches OS/2-Programm  }

program generic;

  uses
     os2def,pmwin,bsedos;

  function clientwndproc(window : HWND;msg : longint;mp1,mp2 : MParam) :
    MResult;export;

    var
       ps : HPS;
       rcl : RECTL;

    begin
       clientwndproc:=nil;
       case msg of
          WM_CREATE : ;
          WM_PAINT : ;
          WM_COMMAND : ;
          else clientwndproc:=WinDefWindowProc(window,msg,mp1,mp2);
       end;
    end;

  var
     frame,client : HWND;
     ab : HAB;
     mq : HMQ;
     msg : QMSG;

  const
     frameflags : longint = FCF_TITLEBAR+
                            FCF_SYSMENU+
                    FCF_SIZEBORDER+
                            FCF_MINBUTTON+
                            FCF_MAXBUTTON+
                            FCF_SHELLPOSITION+
                            FCF_TASKLIST+
                            FCF_MENU;

     winclass = 'GENERIC';
     wintitle = '';

 begin
    ab:=WinInitialize(0);
    mq:=WinCreateMsgQueue(ab,0);
    WinRegisterClass(ab,winclass,@clientwndproc,4,0);
    frame:=WinCreateStdWindow(HWND(1),WS_VISIBLE,@frameflags,winclass,
      wintitle,WS_VISIBLE,0,1,@client);
    while (WinGetMsg(ab,@msg,0,0,0)<>0) do
      WinDispatchMsg(ab,@msg);
    WinDestroyWindow(frame);
    WinDestroyMsgQueue(mq);
    WinTerminate(ab);
 end.
