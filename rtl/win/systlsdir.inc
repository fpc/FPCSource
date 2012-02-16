{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2008 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    FPC Pascal system unit part shared by win32/win64.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ TLS Directory code }

{$ifdef FPC_USE_TLS_DIRECTORY}
{ Process TLS callback function }
{ This is only useful for executables
  for DLLs, DLL_Entry gets called. PM }
{$ifdef FPC_INSSIDE_SYSINIT}
Const
  DLL_PROCESS_ATTACH = 1;
  DLL_THREAD_ATTACH = 2;
  DLL_PROCESS_DETACH = 0;
  DLL_THREAD_DETACH = 3;

var
   TlsKey : dword; external name '_FPC_TlsKey';

type
  TTlsDirectory=packed record
    data_start, data_end : pointer;
    index_pointer, callbacks_pointer : pointer;
    zero_fill_size : dword;
    flags : dword;
  end;


function TlsGetValue(dwTlsIndex : DWord) : pointer; stdcall;
  external 'kernel32' name 'TlsGetValue';

procedure InitSystemThreads; external name '_FPC_InitSystemThreads';
procedure SysAllocateThreadVars; external name '_FPC_SysAllocateThreadVars';
procedure InitHeap; external name '_FPC_InitHeap';
{$endif FPC_INSSIDE_SYSINIT}

procedure Exec_Tls_callback(Handle : pointer; reason : Dword; Reserved : pointer);
  stdcall; [public,alias:'_FPC_Tls_Callback'];
  begin
     if IsLibrary then
       Exit;
     case reason of
       { For executables, DLL_PROCESS_ATTACH is called *before* the entry point,
         and DLL_PROCESS_DETACH is called *after* RTL shuts down and calls ExitProcess.
         It isn't a good idea to handle resources of the main thread at these points.
         InitSystemThreads is necessary however, because if some statically loaded
         DLL creates a thread, it will invoke DLL_THREAD_ATTACH before anything else is
         initialized.
         TODO: The problem is that InitSystemThreads depends (in case of Win32)
         on EntryInformation which is not available at this point.
         Solving it properly needs to move this routine
         to sysinit unit or something like that.
         Exec_Tls_Callback is now part of sysinit unit for win32
         and the EntryInformation is a constant which sholud prevent troubles }
       DLL_PROCESS_ATTACH:
         begin
           InitHeap;
           InitSystemThreads;
         end;

       DLL_THREAD_ATTACH :
         begin
         {  !!! SysInitMultithreading must NOT be called here. Windows guarantees that
            the main thread invokes PROCESS_ATTACH, not THREAD_ATTACH. So this always
            executes in non-main thread. SysInitMultithreading() here will cause
            initial threadvars to be copied to TLS of non-main thread, and threadvars
            of the main thread will be reinitialized upon the next access with zeroes,
            ending up in a delayed failure which is very hard to debug.
            Fortunately this nasty scenario can happen only when the first non-main thread
            was created outside of RTL (Sergei).
         }
           { Allocate Threadvars  }
           SysAllocateThreadVars;

           { NS : no idea what is correct to pass here - pass dummy value for now }
           { passing a dummy is ok, the correct value is read from the coff header of SysInstance (FK) }
           InitThread($1000000); { Assume everything is idempotent there, as the thread could have been created with BeginThread... }
         end;
       DLL_THREAD_DETACH :
         begin
           if TlsGetValue(TLSKey)<>nil then
             DoneThread; { Assume everything is idempotent there }
         end;
     end;
  end;


{ Mingw tlssup.c source code has
  _CRTALLOC(".CRT$XLA") PIMAGE_TLS_CALLBACK __xl_a = 0;
  _CRTALLOC(".CRT$XLZ") PIMAGE_TLS_CALLBACK __xl_z = 0;
  and the callback pointer is set to:
  (&__xl_a+1), (+1 meaning =+sizeof(pointer))
  I am not sure this can be compatible with
}

const
  FreePascal_TLS_callback : pointer = @Exec_Tls_callback;
    public name '__FPC_tls_callbacks' section '.CRT$XLFPC';
  FreePascal_end_of_TLS_callback : pointer = nil;
    public name '__FPC_end_of_tls_callbacks' section '.CRT$XLZZZ';
var
  tls_callbacks : pointer; external name '___crt_xl_start__';
  tls_data_start : pointer; external name '___tls_start__';
  tls_data_end : pointer; external name '___tls_end__';

  _tls_index : dword; cvar; external;

const
  _tls_used : TTlsDirectory = (
    data_start : @tls_data_start;
    data_end : @tls_data_end;
    index_pointer : @_tls_index;
    callbacks_pointer : @tls_callbacks;
    zero_fill_size : 0;
    flags : 0;
  ); cvar; public;

{$ifdef win64}
  { This is a hack to support external linking.
    All released win64 versions of GNU binutils miss proper prefix handling
    when searching for _tls_used and expect two leading underscores.
    The issue has been fixed in binutils snapshots, but not released yet.

    TODO: This should be removed as soon as next version of binutils (>2.21) is
    released and we upgrade to it. }
    __tls_used : TTlsDirectory = (
      data_start : @tls_data_start;
      data_end : @tls_data_end;
      index_pointer : @_tls_index;
      callbacks_pointer : @tls_callbacks;
      zero_fill_size : 0;
      flags : 0;
    ); cvar; public;
{$endif win64}
{$endif FPC_USE_TLS_DIRECTORY}

