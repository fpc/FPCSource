unit system;

interface

type
  HResult=word;
  LPCTSTR=^char;far;

procedure fpc_InitializeUnits;compilerproc;
procedure fpc_do_exit;compilerproc;

procedure InitTask;external 'KERNEL';
procedure WaitEvent;external 'KERNEL';
procedure InitApp;external 'USER';
procedure MessageBox(hWnd: word; lpText, lpCaption: LPCTSTR; uType: word);external 'USER';

implementation

procedure fpc_InitializeUnits;[public,alias:'FPC_INITIALIZEUNITS'];compilerproc;
begin
  MessageBox(0, 'Hello, world!', 'yo', 0);
end;

procedure fpc_do_exit;[public,alias:'FPC_DO_EXIT'];compilerproc;
begin
  asm
    mov ax, 4c00h
    int 21h
  end;
end;

end.
