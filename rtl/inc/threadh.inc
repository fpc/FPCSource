{
    $Id$
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2000 by the Free Pascal development team

    This File contains the OS indenpendend declartions for multi
    threading support in FPC

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
   TThreadFunc = function(parameter : pointer) : longint;

{*****************************************************************************
                         Multithread Handling
*****************************************************************************}

function BeginThread(sa : Pointer;stacksize : dword;
  ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;
  var ThreadId : DWord) : DWord;
{ Delphi uses a longint for threadid }
function BeginThread(sa : Pointer;stacksize : dword;
  ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;
  var ThreadId : Longint) : DWord;

{ add some simplfied forms which make lifer easier and porting }
{ to other OSes too ...                                        }
function BeginThread(ThreadFunction : tthreadfunc) : DWord;
function BeginThread(ThreadFunction : tthreadfunc;p : pointer) : DWord;
function BeginThread(ThreadFunction : tthreadfunc;p : pointer; var ThreadId : DWord) : DWord;
function BeginThread(ThreadFunction : tthreadfunc;p : pointer; var ThreadId : Longint) : DWord;

procedure EndThread(ExitCode : DWord);
procedure EndThread;

{ this allows to do a lot of things in MT safe way }
{ it is also used to make the heap management      }
{ thread safe                                      }
procedure InitCriticalSection(var cs : TRTLCriticalSection);
procedure DoneCriticalsection(var cs : TRTLCriticalSection);
procedure EnterCriticalsection(var cs : TRTLCriticalSection);
procedure LeaveCriticalsection(var cs : TRTLCriticalSection);

{
  $Log$
  Revision 1.8  2002-10-14 19:39:17  peter
    * threads unit added for thread support

  Revision 1.7  2002/09/07 15:07:46  peter
    * old logs removed and tabs fixed

  Revision 1.6  2002/07/28 20:43:48  florian
    * several fixes for linux/powerpc
    * several fixes to MT

}
