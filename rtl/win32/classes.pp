{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for win32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}

{ determine the type of the resource/form file }
{$define Win16Res}

unit Classes;

interface

uses
  rtlconsts,
  sysutils,
  types,
{$ifdef FPC_TESTGENERICS}
  fgl,
{$endif}
  typinfo,
  windows;

type
  TWndMethod = procedure(var msg : TMessage) of object;

function MakeObjectInstance(Method: TWndMethod): Pointer;
procedure FreeObjectInstance(ObjectInstance: Pointer);

function AllocateHWnd(Method: TWndMethod): HWND;
procedure DeallocateHWnd(Wnd: HWND);

{$i classesh.inc}

implementation

uses
  sysconst;

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

type
  PMethodWrapperTrampoline = ^TMethodWrapperTrampoline;
  PWrapperBlock = ^TWrapperBlock;

  TMethodWrapperTrampoline = packed record
    Call : byte;
    CallOffset : PtrInt;
    Jmp : byte;
    JmpOffset : PtrInt;
    case Integer of
      0: (Next: PMethodWrapperTrampoline; Block : PWrapperBlock);
      1: (Method: TWndMethod);
  end;

  TWrapperBlock = packed record
    Next : PWrapperBlock;
    UsageCount : Longint;
    Trampolines : array[0..0] of TMethodWrapperTrampoline;
  end;

var
  WrapperBlockList : PWrapperBlock;
  TrampolineFreeList : PMethodWrapperTrampoline;
  CritObjectInstance : TCriticalSection;

function TrampolineWndProc(Window: HWND; Message, WParam: WPARAM;LParam: LPARAM): HRESULT; stdcall; assembler;
asm
  // build up tmessage structure
  pushl $0
  movl (%eax),%ecx
  pushl LPARAM
  pushl WPARAM
  pushl Message
  // msg
  leal (%esp),%edx
  // load self
  movl 4(%eax),%eax
  // call method
  call %ecx
  addl $12,%esp
  // load result
  popl %eax
end;


function get_method_offset : Pointer;assembler;nostackframe;
  asm
    movl    (%esp),%eax
    addl    $5,%eax
  end;


const
  SizeOfPage = 4096;


function MakeObjectInstance(Method: TWndMethod): Pointer;
  var
    NewBlock : PWrapperBlock;
    Trampoline : PMethodWrapperTrampoline;
  begin
    EnterCriticalSection(CritObjectInstance);
    try
      if not(assigned(TrampolineFreeList)) then
        begin
          NewBlock:=VirtualAlloc(nil,SizeOfPage,MEM_COMMIT,PAGE_EXECUTE_READWRITE);
          NewBlock^.UsageCount:=0;
          NewBlock^.Next:=WrapperBlockList;
          WrapperBlockList:=NewBlock;
          Trampoline:=@NewBlock^.Trampolines;
          while pointer(Trampoline)+sizeof(Trampoline)<pointer(NewBlock)+SizeOfPage do
            begin
              Trampoline^.Next:=TrampolineFreeList;
              Trampoline^.Block:=NewBlock;
              TrampolineFreeList:=Trampoline;
              inc(Trampoline);
            end;
        end;
      Trampoline:=TrampolineFreeList;
      TrampolineFreeList:=TrampolineFreeList^.Next;
//      inc(Trampoline^.Block^.UsageCount);
      Trampoline^.Call:=$e8;
      Trampoline^.CallOffset:=pointer(@get_method_offset)-pointer(@Trampoline^.Call)-5;
      Trampoline^.Jmp:=$e9;
      Trampoline^.JmpOffset:=pointer(@TrampolineWndProc)-pointer(@Trampoline^.Jmp)-5;
      Trampoline^.Method:=Method;
      Result:=Trampoline;
    finally
      LeaveCriticalSection(CritObjectInstance);
    end;
  end;


procedure FreeObjectInstance(ObjectInstance: Pointer);
  begin
    EnterCriticalSection(CritObjectInstance);
    try
      // block gets overwritten by method dec(PMethodWrapperTrampoline(ObjectInstance)^.Block^.UsageCount);
      PMethodWrapperTrampoline(ObjectInstance)^.Next:=TrampolineFreeList;
      TrampolineFreeList:=PMethodWrapperTrampoline(ObjectInstance);
    finally
      LeaveCriticalSection(CritObjectInstance);
    end;
  end;


procedure DeleteInstBlockList;
  var
    hp : PWrapperBlock;
  begin
    EnterCriticalSection(CritObjectInstance);
    try
      while assigned(WrapperBlockList) do
        begin
          hp:=WrapperBlockList^.Next;
          if VirtualFree(WrapperBlockList,4096,MEM_DECOMMIT) then
            VirtualFree(WrapperBlockList,0,MEM_RELEASE);
          WrapperBlockList:=hp;
        end;
    finally
      LeaveCriticalSection(CritObjectInstance);
    end;
  end;


function AllocateHWnd(Method: TWndMethod): HWND;
  begin
    { dummy }
    runerror(217);
    Result:=0;
  end;


procedure DeallocateHWnd(Wnd: HWND);
  begin
    { dummy }
    runerror(217);
  end;


initialization
  WrapperBlockList:=nil;
  TrampolineFreeList:=nil;
  InitCriticalSection(CritObjectInstance);
  CommonInit;

finalization
  CommonCleanup;
  DeleteInstBlockList;
  DoneCriticalSection(CritObjectInstance);
end.
