{****************************************************************************

                   Copyright (c) 1993,94 by Florian Kl„mpfl

 ****************************************************************************}

{ Demonstrationsprogramm zu FPKPascal unter OS/2  }
{ traditionelles Hello world-Porgamm (PM-Version) }

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

  function clientwndproc(window : cardinal;msg : longint;mp1,mp2 : cardinal) :
    cardinal;export;
  
    const
       text = 'Hello world by OS/2 and FPKPascal';
       
    var
       ps : cardinal;
       rcl : RECTL;

    begin
       {clientwndproc:=nil;      }
       case msg of
         {WM_CREATE : DosBeep(200,500);}
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
                             'HelloPM von FPKPascal',
                             'šber',0,MB_ICONEXCLAMATION+MB_MOVEABLE);
                       end;
       else
          clientwndproc:=WinDefWindowProc(window,msg,mp1,mp2);    
       end;                                                        
    end;

 begin
    ab:=WinInitialize(0);
    mq:=WinCreateMsgQueue(ab,0);
    WinRegisterClass(ab,'HELLOPM',@clientwndproc,4,0);    
    frame:=WinCreateStdWindow(cardinal(1),WS_VISIBLE,@frameflags,'HELLOPM',
      'PMDemo 1',WS_VISIBLE,0,1,@client);
    while (WinGetMsg(ab,@msg,0,0,0)<>0) do
      WinDispatchMsg(ab,@msg);
    WinDestroyWindow(frame);
    WinDestroyMsgQueue(mq);
    WinTerminate(ab);
 end.
{
  $Log$
  Revision 1.1  1999-06-02 16:01:35  hajny
    * changes by Ramon Bosque

}
