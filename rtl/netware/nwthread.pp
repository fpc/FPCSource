{
    $Id$
    This file is part of the Free Pascal run time library
    for Netware.
    Copyright (c) 1999-2003 by the Free Pascal development team.
		
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
			
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
				    
**********************************************************************}

unit nwthread;
interface

{$mode objfpc}


  const
    ThreadsNlm = 'threads';
    Lib0Nlm    = 'lib0';
    NlmLibNlm  = 'nlmlib';

  Type
    PLong = ^longint;

{$PACKRECORDS C}



  { values for __action_code used with ExitThread()  }

  const
     TSR_THREAD = -1;
     EXIT_THREAD = 0;
     EXIT_NLM = 1;
  { values for __mode used with spawnxx()  }
     P_WAIT = 0;
     P_NOWAIT = 1;
     P_OVERLAY = 2;
     P_NOWAITO = 4;
     P_SPAWN_IN_CURRENT_DOMAIN = 8;
     NO_CONTEXT = 0;
     USE_CURRENT_CONTEXT = 1;
  { stack defines  }
     MIN_STACKSIZE = 16384;
     DEFAULT_STACKSIZE = 16384;

  type
  
     PWorkToDo = ^TWorkToDo;
    
     TProcedure    = procedure; cdecl;
     TThreadFunc   = procedure (param1:pointer); cdecl;
     TWorkToDoProc = procedure (data:pointer; workToDo:PWorkToDo); cdecl;
     TCleanup      = procedure (para1:longint); cdecl;

  
     PAESProcessStructure = ^TAESProcessStructure;
     TAESProcessStructure = record
          ALink              : PAESProcessStructure;
          AWakeUpDelayAmount : longint;
          AWakeUpTime        : longint;
          AProcessToCall     : procedure (para1:pointer);cdecl;
          ARTag              : longint;
          AOldLink           : longint;
       end;


     PWorkToDoStructure = ^TWorkToDoStructure;
     TWorkToDoStructure = record
          Link            : PWorkToDoStructure;
          workProcedure   : TProcedure;
          WorkResourceTag : longint;
          PollCountAmount : longint;
          PollCountWhen   : longint;
          userProcedure   : TProcedure;
          dataPtr         : pointer;
          destThreadGroup : longint;
       end;
     TWorkToDo = TWorkToDoStructure;
     

     
  { custom data area variables...  }

    var
       threadCustomDataPtr : pointer;cvar;external;
       threadCustomDataSize : longint;cvar;external;
       threadGroupCustomDataPtr : pointer;cvar;external;
       threadGroupCustomDataSize : longint;cvar;external;
    
    
  function AtUnload(func:Tprocedure):longint;                       cdecl;external ThreadsNlm name 'AtUnload';
  function BeginThread(func:TThreadFunc; 
                       stackP:pointer; 
		       stackSize:dword; 
		       arg:pointer):longint;                        cdecl;external ThreadsNlm name 'BeginThread';
  function BeginThreadGroup(func:TThreadFunc; 
                            stackP:pointer; 
			    stackSize:dword; 
			    arg:pointer):longint;                   cdecl;external ThreadsNlm name 'BeginThreadGroup';
  function Breakpoint(arg:longint):longint;                         cdecl;external Lib0Nlm name 'Breakpoint';
  procedure CancelNoSleepAESProcessEvent(EventNode:PAESProcessStructure);cdecl;external ThreadsNlm name 'CancelNoSleepAESProcessEvent';
  procedure CancelSleepAESProcessEvent  (EventNode:PAESProcessStructure);cdecl;external ThreadsNlm name 'CancelSleepAESProcessEvent';
  function ClearNLMDontUnloadFlag(NLMID:longint):longint;                cdecl;external ThreadsNlm name 'ClearNLMDontUnloadFlag';
  procedure delay(milliseconds:dword);                                   cdecl;external ThreadsNlm name 'delay';
  function EnterCritSec:longint;                                         cdecl;external ThreadsNlm name 'EnterCritSec';
  function ExitCritSec:longint;                                          cdecl;external ThreadsNlm name 'ExitCritSec';
  procedure ExitThread(action_code     :longint; 
                       termination_code:longint);                        cdecl;external ThreadsNlm name 'ExitThread';

  function FindNLMHandle(NLMFileName:Pchar):dword;                       cdecl;external ThreadsNlm name 'FindNLMHandle';
  function getcmd(cmdLine:Pchar):Pchar;                                  cdecl;external ThreadsNlm name 'getcmd';
  function GetNLMHandle:dword;                                           cdecl;external ThreadsNlm name 'GetNLMHandle';
  function GetNLMID:longint;                                             cdecl;external ThreadsNlm name 'GetNLMID';
  function GetNLMIDFromNLMHandle(NLMHandle:longint):longint;             cdecl;external ThreadsNlm name 'GetNLMIDFromNLMHandle';
  function GetNLMIDFromThreadID(threadID:longint;fileName:Pchar):longint;cdecl;external ThreadsNlm name 'GetNLMIDFromThreadID';
  function GetNLMNameFromNLMID(NLMID:longint; 
                               fileName:Pchar; 
			       description:Pchar):longint;               cdecl;external ThreadsNlm name 'GetNLMNameFromNLMID';
  function GetNLMNameFromNLMHandle(NLMHandle:longint; 
                                   LDFileName:Pchar; 
				   LDName:Pchar):longint;                cdecl;external ThreadsNlm name 'GetNLMNameFromNLMHandle';
  function GetThreadContextSpecifier(threadID:longint):longint;          cdecl;external ThreadsNlm name 'GetThreadContextSpecifier';
  function GetThreadGroupID:longint;                                     cdecl;external ThreadsNlm name 'GetThreadGroupID';
  function __GetThreadIDFromPCB(PCB:longint):longint;                    cdecl;external Lib0Nlm name '__GetThreadIDFromPCB';
  function GetThreadHandicap(threadID:longint):longint;                  cdecl;external ThreadsNlm name 'GetThreadHandicap';
  function GetThreadID:longint;                                          cdecl;external ThreadsNlm name 'GetThreadID';
  function GetThreadName(threadID:longint; tName:Pchar):longint;         cdecl;external ThreadsNlm name 'GetThreadName';
  function MapNLMIDToHandle(NLMID:longint):longint;                      cdecl;external ThreadsNlm name 'MapNLMIDToHandle';
  function PopThreadCleanup(execute:longint):TCLEANUP;                   cdecl;external ThreadsNlm name 'PopThreadCleanup';
  function PopThreadGroupCleanup(execute:longint):TCLEANUP;              cdecl;external ThreadsNlm name 'PopThreadGroupCleanup';
  function PushThreadCleanup(func:TCLEANUP):longint;                     cdecl;external ThreadsNlm name 'PushThreadCleanup';
  function PushThreadGroupCleanup(func:TCLEANUP):longint;                cdecl;external ThreadsNlm name 'PushThreadGroupCleanup';
  function RenameThread(threadID:longint; newName:Pchar):longint;        cdecl;external ThreadsNlm name 'RenameThread';
  function ResumeThread(threadID:longint):longint;                       cdecl;external ThreadsNlm name 'ResumeThread';
  function ReturnNLMVersionInfoFromFile(pathName:pchar; 
                                        majorVersion:PLONG; 
					minorVersion:PLONG; 
					revision:PLONG; 
					year:PLONG; 
                                        month:PLONG; 
					day:PLONG; 
					copyrightString:pchar; 
					description:pchar):longint;      cdecl;external NlmLibNlm name 'ReturnNLMVersionInfoFromFile';
  function ReturnNLMVersionInfoFromFile(pathName:pchar; 
                                        var majorVersion,minorVersion,revision:longint;
					var year,month,day:longint; 
					copyrightString:pchar; 
					description:pchar):longint;      cdecl;external NlmLibNlm name 'ReturnNLMVersionInfoFromFile';

  function ReturnNLMVersionInformation(NLMHandle:longint; 
                                       majorVersion,minorVersion,revision,year,month,day:PLONG; 
                                       copyrightString:pchar; description:pchar):longint;cdecl;external NlmLibNlm name 'ReturnNLMVersionInformation';
  function ReturnNLMVersionInformation(NLMHandle:longint; 
                                       var majorVersion,minorVersion,revision,year,month,day:longint; 
                                       copyrightString:pchar; description:pchar):longint;cdecl;external NlmLibNlm name 'ReturnNLMVersionInformation';

  procedure ScheduleNoSleepAESProcessEvent(EventNode:PAESProcessStructure);cdecl;external ThreadsNlm name 'ScheduleNoSleepAESProcessEvent';
  procedure ScheduleSleepAESProcessEvent(EventNode:PAESProcessStructure);  cdecl;external ThreadsNlm name 'ScheduleSleepAESProcessEvent';


  function ScheduleWorkToDo(ProcedureToCall:TWorkToDoProc; 
                            workData       :pointer; 
			    workToDo       :PWorkToDo):longint;            cdecl;external ThreadsNlm name 'ScheduleWorkToDo';
  function SetNLMDontUnloadFlag(NLMID:longint):longint;                    cdecl;external ThreadsNlm name 'SetNLMDontUnloadFlag';
  function SetNLMID(newNLMID:longint):longint;                             cdecl;external ThreadsNlm name 'SetNLMID';
  function SetThreadContextSpecifier(threadID,
                                     contextSpecifier:longint):longint;    cdecl;external ThreadsNlm name 'SetThreadContextSpecifier';
  function SetThreadGroupID(newThreadGroupID:longint):longint;             cdecl;external ThreadsNlm name 'SetThreadGroupID';
  procedure SetThreadHandicap(threadID, handicap:longint);                 cdecl;external ThreadsNlm name 'SetThreadHandicap';
  function spawnlp(mode:longint; 
                   path,arg0:Pchar; 
		   args:array of const):longint;                           cdecl;external ThreadsNlm name 'spawnlp';
  function spawnlp(mode:longint; 
                   path,arg0:Pchar):longint;                               cdecl;external ThreadsNlm name 'spawnlp';
  function spawnvp(mode:longint; 
                   path,argv:PPchar):longint;                              cdecl;external ThreadsNlm name 'spawnvp';
  function SuspendThread(threadID:longint):longint;                        cdecl;external ThreadsNlm name 'SuspendThread';
  procedure ThreadSwitch;                                                  cdecl;external ThreadsNlm name 'ThreadSwitch';
  procedure ThreadSwitchLowPriority;                                       cdecl;external ThreadsNlm name 'ThreadSwitchLowPriority';
  procedure ThreadSwitchWithDelay;                                         cdecl;external ThreadsNlm name 'ThreadSwitchWithDelay';

implementation


end.

{
  $Log$
  Revision 1.1  2003-02-16 17:45:08  armin
  * added nwsnut, nwconio and nwthreads for netware

  
}