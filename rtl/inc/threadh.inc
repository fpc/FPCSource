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

{$ifdef MT}
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
function BeginThread(ThreadFunction : tthreadfunc;p : pointer;
  var ThreadId : DWord) : DWord;
function BeginThread(ThreadFunction : tthreadfunc;p : pointer;
  var ThreadId : Longint) : DWord;

procedure EndThread(ExitCode : DWord);
procedure EndThread;

{ this allows to do a lot of things in MT safe way }
{ it is also used to make the heap management      }
{ thread safe                                      }
procedure InitCriticalSection(var cs : TRTLCriticalSection);
procedure DoneCriticalsection(var cs : TRTLCriticalSection);
procedure EnterCriticalsection(var cs : TRTLCriticalSection);
procedure LeaveCriticalsection(var cs : TRTLCriticalSection);

{$endif MT}

{
  $Log$
  Revision 1.6  2002-07-28 20:43:48  florian
    * several fixes for linux/powerpc
    * several fixes to MT

  Revision 1.5  2001/10/23 21:51:03  peter
    * criticalsection renamed to rtlcriticalsection for kylix compatibility

  Revision 1.4  2001/01/26 16:37:54  florian
  *** empty log message ***

  Revision 1.3  2001/01/24 21:47:18  florian
    + more MT stuff added

  Revision 1.2  2001/01/05 17:35:50  florian
    * the info about exception frames is stored now on the stack
      instead on the heap

  Revision 1.1  2001/01/01 19:06:59  florian
    + initial release
}
