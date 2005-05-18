{

    Spawning and messaging another DOS process
    Free Pascal for MorphOS example
    (dirty, but actually does work... sometimes... :)

    Copyright (C) 2004 by Karoly Balogh

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ * Thanks fly to Sigbjorn 'CISC' Skjaeret for hints and * }
{ * Michal 'kiero' Wozniak for example code.             * }
{ * 2004.12.10                                           * }

{$MODE FPC}
program process;

uses exec, utility, doslib;

type
  pMyMsg = ^tMyMsg;
  tMyMsg = Record
    mm_MsgNode : tMessage;
    mm_Command : DWord;
  end;

var
  ThMsg       : tMyMsg;
  ThStartupMsg: tMyMsg;
  ThChildPort : pMsgPort;
  ThPort      : pMsgPort;
  ThReplyPort : pMsgPort;
  ThProc      : pProcess;

const
  SUBPROCESS_NAME : PChar = 'FPC subprocess';

const
  TCMD_HELLO = 1;
  TCMD_WORLD = 2;
  TCMD_SPACE = 3;
  TCMD_EXCL  = 4;
  TCMD_NEWL  = 5;
  TCMD_QUIT  = $FF;


procedure ShutDown(Err: String);
begin
 if assigned(ThReplyPort) then DeleteMsgPort(ThReplyPort);
 if assigned(ThPort) then DeleteMsgPort(ThPort);

 if Err<>'' then begin
   writeln(Err);
   halt(1);
 end else
   halt(0);
end;

{ * This is our subtask procedure * }
{ * Our subtask do exists until this procedure exits. * }
procedure MyProcess;
var
  thisThread: pProcess;
  startupMsg: pMyMsg;
  mainMsg   : pMyMsg;
  mainPort  : pMsgPort;
  finish    : Boolean;
begin
 { * Getting startupmsg * }
 NewGetTaskAttrs(NIL,@startupMsg,sizeof(startupMsg^),
                 TASKINFOTYPE_STARTUPMSG,[TAG_DONE]);
 startupMsg^.mm_Command:=0;

 { * Getting taskport * }
 NewGetTaskAttrs(NIL,@mainPort,sizeof(mainPort^),
                 TASKINFOTYPE_TASKMSGPORT,[TAG_DONE]);

 finish:=False;
 repeat
   mainMsg:=pMyMsg(GetMsg(mainPort));
   if mainMsg<>NIL then begin
     { * Using write in such an example is not really elegant * }
     { * since write is not reentrant yet, so if more tasks   * }
     { * use it in the same time, it will make troubles.      * }
     { * but it does what we want now.                        * }
     Case mainMsg^.mm_Command Of
       TCMD_HELLO: write('Hello');
       TCMD_WORLD: write('World');
       TCMD_SPACE: write(' ');
       TCMD_EXCL : write('!');
       TCMD_NEWL : writeln;
       TCMD_QUIT : finish:=True;
     end;
     Inc(startupMsg^.mm_Command);
     ReplyMsg(pMessage(mainMsg));
   end;
   { * Polling for messages... * }
   { * It's possible to use WaitPort() of course, but * }
   { * you probably want to do some stuff in the background * }
   { * so it's more useful to poll then. Replace Delay() * }
   { * with your code, or more, add your code after it. * }
   Delay(1);
 until finish;


 { * We MUST NOT reply StartupMsg!          * }
 { * It will be replied by exec internally. * }
end;

{ * This is a helper proc, makes sending * }
{ * of command messages more easy.       * }
procedure SendMsg(msgID : DWord);
begin
  with ThMsg do begin
    with mm_MsgNode do begin
      mn_Node.ln_Type:=NT_MESSAGE;
      mn_Length:=SizeOf(tMyMsg);
      mn_ReplyPort:=ThPort;
    end;
    mm_Command:=msgID;
  end;
  PutMsg(ThChildPort,pMessage(@ThMsg));
end;


begin
 ThReplyPort:=CreateMsgPort;
 ThPort:=CreateMsgPort;
 if (ThReplyPort=NIL) or (ThPort=NIL) then
   ShutDown('Can''t create message ports.');

 { * Setting up StartupMsg * }
 with ThStartupMsg do begin
   with mm_MsgNode do begin
     mn_Node.ln_Type:=NT_MESSAGE;
     mn_Length:=SizeOf(tMyMsg);
     mn_ReplyPort:=ThReplyPort;
   end;
 end;

 ThProc:=CreateNewProcTags([NP_CodeType    , CODETYPE_PPC,
                            NP_Entry       , DWord(@MyProcess),
                            NP_Name        , DWord(SUBPROCESS_NAME),
                            NP_StartupMsg  , DWord(@ThStartupMsg),
                            NP_TaskMsgPort , DWord(@ThChildPort),
                            { * such stacksize is overkill for our current * }
                            { * subtask, but more complex things may actually * }
                            { * require even more... * }
                            NP_PPCStackSize, 32768,
                            TAG_DONE]);
 if ThProc=NIL then ShutDown('Can''t create subprocess!');

 SendMsg(TCMD_HELLO);
 WaitPort(ThPort); GetMsg(ThPort);

 SendMsg(TCMD_SPACE);
 WaitPort(ThPort); GetMsg(ThPort);

 SendMsg(TCMD_WORLD);
 WaitPort(ThPort); GetMsg(ThPort);

 SendMsg(TCMD_EXCL);
 WaitPort(ThPort); GetMsg(ThPort);

 SendMsg(TCMD_NEWL);
 WaitPort(ThPort); GetMsg(ThPort);

 SendMsg(TCMD_QUIT);
 WaitPort(ThPort); GetMsg(ThPort);

 { * Wait our subprocess to exit... * }
 WaitPort(ThReplyPort); GetMsg(ThReplyPort);
 writeln('Subtask got ',ThStartupMsg.mm_Command,' message(s).');

 ShutDown('');
end.
