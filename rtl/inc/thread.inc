{
    $Id$
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2000 by the Free Pascal development team

    OS independent thread functions/overloads

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                           Threadvar initialization
*****************************************************************************}


{*****************************************************************************
                            Overloaded functions
*****************************************************************************}

    function BeginThread(sa : Pointer;stacksize : dword;
                         ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;
                         var ThreadId : Longint) : DWord;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,p,creationFlags,Dword(THreadId));
      end;


    function BeginThread(ThreadFunction : tthreadfunc) : DWord;
      var
        dummy : dword;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,nil,0,dummy);
      end;


    function BeginThread(ThreadFunction : tthreadfunc;p : pointer) : DWord;
      var
        dummy : dword;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,p,0,dummy);
      end;


    function BeginThread(ThreadFunction : tthreadfunc;p : pointer;var ThreadId : DWord) : DWord;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,p,0,ThreadId);
      end;


    function BeginThread(ThreadFunction : tthreadfunc;p : pointer;var ThreadId : Longint) : DWord;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,p,0,Dword(ThreadId));
      end;


    procedure EndThread;
      begin
        EndThread(0);
      end;


{
  $Log$
  Revision 1.1  2002-10-14 19:39:17  peter
    * threads unit added for thread support

}
