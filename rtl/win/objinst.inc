{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998-2026 by the Free Pascal development team
    MakeObjectInstance and AllocateHWnd for the Windows classes unit.
    See the file COPYING.FPC, included in this distribution, for details about the copyright.
}

{$if defined(CPUI386) or defined(CPUX86_64)}

const
  SizeOfPage = 4096;

type
  PObjectInstance = ^TObjectInstance;
  PInstanceBlock = ^TInstanceBlock;

{$ifdef CPUI386}
  // call get_method_offset; jmp TrampolineWndProc; followed by the method
  TObjectInstance = packed record
    Call : byte;
    CallOffset : PtrInt;
    Jmp : byte;
    JmpOffset : PtrInt;
    case Integer of
      0: (Next: PObjectInstance);
      1: (Method: TWndMethod);
  end;
{$endif CPUI386}

{$ifdef CPUX86_64}
  // mov rcx,rdx; mov rdx,r8; mov r8,r9; mov r9,Instance; jmp [rip] Target; followed by the method
  TObjectInstance = packed record
    MovArgs : array[0..8] of byte;
    MovR9 : array[0..1] of byte;
    Instance : PObjectInstance;
    JmpRip : array[0..5] of byte;
    Target : Pointer;
    case Integer of
      0: (Next: PObjectInstance);
      1: (Method: TWndMethod);
  end;
{$endif CPUX86_64}

  TInstanceBlock = packed record
    Next : PInstanceBlock;
    Instances : array[0..(SizeOfPage-SizeOf(Pointer)) div SizeOf(TObjectInstance)-1] of TObjectInstance;
  end;

var
  InstanceBlockList : PInstanceBlock;
  InstanceFreeList : PObjectInstance;
  CritObjectInstance : TCriticalSection;

{$ifdef CPUI386}
// Window procedure reached from the trampoline, with eax pointing to the method.
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


// Return the address of the method following the trampoline that called this routine.
function get_method_offset : Pointer;assembler;nostackframe;
  asm
    movl    (%esp),%eax
    addl    $5,%eax
  end;


// Fill the code of a trampoline for aMethod.
procedure InitObjectInstance(aInstance: PObjectInstance; aMethod: TWndMethod);

  begin
    aInstance^.Call:=$e8;
    aInstance^.CallOffset:=pointer(@get_method_offset)-pointer(@aInstance^.Call)-5;
    aInstance^.Jmp:=$e9;
    aInstance^.JmpOffset:=pointer(@TrampolineWndProc)-pointer(@aInstance^.Jmp)-5;
    aInstance^.Method:=aMethod;
  end;
{$endif CPUI386}


{$ifdef CPUX86_64}
// Window procedure reached from the trampoline, with the window handle dropped and the instance as last argument.
function ObjectInstanceWndProc(aMsg: UINT; aWParam: WPARAM; aLParam: LPARAM; aInstance: PObjectInstance): LRESULT;

  var
    lMsg : TMessage;
  begin
    lMsg.Msg:=aMsg;
    lMsg.WParam:=aWParam;
    lMsg.LParam:=aLParam;
    lMsg.Result:=0;
    aInstance^.Method(lMsg);
    Result:=lMsg.Result;
  end;


// Fill the code of a trampoline for aMethod.
procedure InitObjectInstance(aInstance: PObjectInstance; aMethod: TWndMethod);

  const
    MovArgsCode : array[0..8] of byte = (
      $48,$89,$D1,  { mov rcx,rdx }
      $4C,$89,$C2,  { mov rdx,r8 }
      $4D,$89,$C8); { mov r8,r9 }
    MovR9Code : array[0..1] of byte = ($49,$B9);
    JmpRipCode : array[0..5] of byte = ($FF,$25,0,0,0,0);
  begin
    aInstance^.MovArgs:=MovArgsCode;
    aInstance^.MovR9:=MovR9Code;
    aInstance^.Instance:=aInstance;
    aInstance^.JmpRip:=JmpRipCode;
    aInstance^.Target:=@ObjectInstanceWndProc;
    aInstance^.Method:=aMethod;
  end;
{$endif CPUX86_64}


function MakeObjectInstance(Method: TWndMethod): Pointer;

  var
    lBlock : PInstanceBlock;
    lInstance : PObjectInstance;
    i : Integer;
  begin
    EnterCriticalSection(CritObjectInstance);
    try
      if not(assigned(InstanceFreeList)) then
        begin
          lBlock:=VirtualAlloc(nil,SizeOfPage,MEM_COMMIT,PAGE_EXECUTE_READWRITE);
          if lBlock=nil then
            OutOfMemoryError;
          lBlock^.Next:=InstanceBlockList;
          InstanceBlockList:=lBlock;
          for i:=High(lBlock^.Instances) downto Low(lBlock^.Instances) do
            begin
              lBlock^.Instances[i].Next:=InstanceFreeList;
              InstanceFreeList:=@lBlock^.Instances[i];
            end;
        end;
      lInstance:=InstanceFreeList;
      InstanceFreeList:=lInstance^.Next;
      InitObjectInstance(lInstance,Method);
      FlushInstructionCache(GetCurrentProcess,lInstance,SizeOf(TObjectInstance));
      Result:=lInstance;
    finally
      LeaveCriticalSection(CritObjectInstance);
    end;
  end;


procedure FreeObjectInstance(ObjectInstance: Pointer);

  begin
    if ObjectInstance=nil then
      exit;
    EnterCriticalSection(CritObjectInstance);
    try
      PObjectInstance(ObjectInstance)^.Next:=InstanceFreeList;
      InstanceFreeList:=PObjectInstance(ObjectInstance);
    finally
      LeaveCriticalSection(CritObjectInstance);
    end;
  end;


// Release all pages allocated by MakeObjectInstance.
procedure DeleteInstBlockList;

  var
    lNext : PInstanceBlock;
  begin
    EnterCriticalSection(CritObjectInstance);
    try
      while assigned(InstanceBlockList) do
        begin
          lNext:=InstanceBlockList^.Next;
          VirtualFree(InstanceBlockList,0,MEM_RELEASE);
          InstanceBlockList:=lNext;
        end;
      InstanceFreeList:=nil;
    finally
      LeaveCriticalSection(CritObjectInstance);
    end;
  end;


// Prepare the object instance administration.
procedure InitObjectInstances;

  begin
    InstanceBlockList:=nil;
    InstanceFreeList:=nil;
    InitCriticalSection(CritObjectInstance);
  end;


// Free the object instance administration.
procedure DoneObjectInstances;

  begin
    DeleteInstBlockList;
    DoneCriticalSection(CritObjectInstance);
  end;

{$else}

function MakeObjectInstance(Method: TWndMethod): Pointer;

  begin
    runerror(211);
    MakeObjectInstance:=nil;
  end;


procedure FreeObjectInstance(ObjectInstance: Pointer);

  begin
    runerror(211);
  end;


// Prepare the object instance administration.
procedure InitObjectInstances;

  begin
  end;


// Free the object instance administration.
procedure DoneObjectInstances;

  begin
  end;

{$endif}


const
  UtilWindowClassName : PWideChar = 'TPUtilWindow';


function AllocateHWnd(Method: TWndMethod): HWND;

  var
    lClass : TWndClassW;
    lRegistered : Boolean;
  begin
    lRegistered:=GetClassInfoW(HInstance,UtilWindowClassName,@lClass);
    if not lRegistered or (Pointer(lClass.lpfnWndProc)<>Pointer(@DefWindowProcW)) then
      begin
        if lRegistered then
          UnregisterClassW(UtilWindowClassName,HInstance);
        FillChar(lClass,SizeOf(lClass),0);
        lClass.lpfnWndProc:=@DefWindowProcW;
        lClass.hInstance:=HInstance;
        lClass.lpszClassName:=UtilWindowClassName;
        RegisterClassW(@lClass);
      end;
    Result:=CreateWindowExW(WS_EX_TOOLWINDOW,UtilWindowClassName,nil,WS_POPUP,0,0,0,0,0,0,HInstance,nil);
    if (Result<>0) and Assigned(Method) then
      SetWindowLongPtrW(Result,GWL_WNDPROC,LONG_PTR(MakeObjectInstance(Method)));
  end;


procedure DeallocateHWnd(Wnd: HWND);

  var
    lInstance : Pointer;
  begin
    lInstance:=Pointer(GetWindowLongPtrW(Wnd,GWL_WNDPROC));
    DestroyWindow(Wnd);
    if lInstance<>Pointer(@DefWindowProcW) then
      FreeObjectInstance(lInstance);
  end;
