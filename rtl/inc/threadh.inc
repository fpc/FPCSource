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


{*****************************************************************************
                         Multithread Handling
*****************************************************************************}
function BeginThread(sa : Pointer;stacksize : dword;
  ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;
  var ThreadId : DWord) : DWord;

{ add some simplfied forms which make lifer easier and porting }
{ to other OSes too ...                                        }
function BeginThread(ThreadFunction : tthreadfunc) : DWord;
function BeginThread(ThreadFunction : tthreadfunc;p : pointer) : DWord;
function BeginThread(ThreadFunction : tthreadfunc;p : pointer;
  var ThreadId : DWord) : DWord;

procedure EndThread(ExitCode : DWord);
procedure EndThread;

{ this allows to do a lot of things in MT safe way }
{ it is also used to make the heap management      }
{ thread safe                                      }
procedure InitCriticalsection(var cs : tcriticalsection);
procedure DoneCriticalsection(var cs : tcriticalsection);
procedure EnterCriticalsection(var cs : tcriticalsection);
procedure LeaveCriticalsection(var cs : tcriticalsection);
{
  $Log$
  Revision 1.1  2001-01-01 19:06:59  florian
    + initial release

}