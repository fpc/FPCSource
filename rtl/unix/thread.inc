{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by the Free Pascal development team.

    Multithreading implementation for Linux

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef MT}

    const
      DefaultStackSize = 16384;
      threadvarblocksize : dword = 0;

    type
      pthreadinfo = ^tthreadinfo;
      tthreadinfo = record
        f : tthreadfunc;
        p : pointer;
      end;

    var
      dataindex : pointer;

    procedure init_threadvar(var offset : dword;size : dword);[public,alias: 'FPC_INIT_THREADVAR'];
      begin
        offset:=threadvarblocksize;
        inc(threadvarblocksize,size);
      end;


    function relocate_threadvar(offset : dword) : pointer;[public,alias: 'FPC_RELOCATE_THREADVAR'];
      begin
        Relocate_ThreadVar := DataIndex + Offset;
      end;


    procedure AllocateThreadVars;
      begin
        { we've to allocate the memory from system  }
        { because the FPC heap management uses      }
        { exceptions which use threadvars but       }
        { these aren't allocated yet ...            }
        { allocate room on the heap for the thread vars }
        DataIndex:=Pointer(Sys_mmap(0,threadvarblocksize,3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0));
        FillChar(DataIndex^,threadvarblocksize,0);
      end;


    procedure ReleaseThreadVars;
      begin
        Sys_munmap(Longint(dataindex),threadvarblocksize);
      end;


    procedure InitThread;
      begin
        ResetFPU;
        { we don't need to set the data to 0 because we did this with }
        { the fillchar above, but it looks nicer                      }

        { ExceptAddrStack and ExceptObjectStack are threadvars       }
        { so every thread has its on exception handling capabilities }
        InitExceptions;
        InOutRes:=0;
        // ErrNo:=0;
      end;


    procedure DoneThread;
      begin
        { release thread vars }
        ReleaseThreadVars;
      end;


    function ThreadMain(param : pointer) : longint;cdecl;
      var
        ti : tthreadinfo;
      begin
{$ifdef DEBUG_MT}
        writeln('New thread started, initialising ...');
{$endif DEBUG_MT}
        AllocateThreadVars;
        InitThread;
        ti:=pthreadinfo(param)^;
        dispose(pthreadinfo(param));
{$ifdef DEBUG_MT}
        writeln('Jumping to thread function');
{$endif DEBUG_MT}
        ThreadMain:=ti.f(ti.p);
      end;


    function BeginThread(sa : Pointer;stacksize : dword;
                         ThreadFunction : tthreadfunc;p : pointer;
                         creationFlags : dword; var ThreadId : DWord) : DWord;
      var
        ti : pthreadinfo;
        FStackPointer : pointer;
        Flags : longint;
      begin
{$ifdef DEBUG_MT}
        writeln('Creating new thread');
{$endif DEBUG_MT}
        IsMultithread:=true;
        { the only way to pass data to the newly created thread }
        { in a MT safe way, is to use the heap                  }
        new(ti);
        ti^.f:=ThreadFunction;
        ti^.p:=p;
{$ifdef DEBUG_MT}
        writeln('Starting new thread');
{$endif DEBUG_MT}
        Flags := CLONE_VM + CLONE_FS + CLONE_FILES + CLONE_SIGHAND + SIGCHLD;
        { Setup stack }
        Getmem(pointer(FStackPointer),StackSize);
        inc(FStackPointer,StackSize);
        { Clone }
        ThreadID:=Clone(@ThreadMain,pointer(FStackPointer),Flags,ti);
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


    procedure EndThread(ExitCode : DWord);
      begin
        DoneThread;
        Sys_Exit(ExitCode);
      end;


    procedure EndThread;
      begin
        EndThread(0);
      end;

    procedure InitCriticalSection(var cs : TRTLCriticalSection);
      begin
      end;

    procedure DoneCriticalSection(var cs : TRTLCriticalSection);
      begin
      end;


    procedure EnterCriticalSection(var cs : TRTLCriticalSection);
      begin
      end;

    procedure LeaveCriticalSection(var cs : TRTLCriticalSection);
      begin
      end;

{$endif MT}

{
  $Log$
  Revision 1.3  2002-09-07 16:01:28  peter
    * old logs removed and tabs fixed

}
