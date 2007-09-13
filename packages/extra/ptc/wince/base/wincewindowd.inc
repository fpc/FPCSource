Type
  TWinCEWndProc = Function(Ahwnd : HWND; AuMsg : UINT; AwParam : WPARAM; AlParam : LPARAM) : LRESULT Of Object;

  TWinCEWindow = Class(TObject)
  Private
    FWindow : HWND;
    FClassName : WideString;
    FClassHInstance : HINST;
  Public
    Constructor Create(Const AClassName, ATitle : WideString;
                       AExStyle, AStyle : DWord;
                       AShow, AX, AY, AWidth, AHeight : Integer;
                       AWndProc : TWinCEWndProc;
                       AData : Pointer = Nil);
    Destructor Destroy; Override;

    Procedure Close;
    Procedure Update;

    Property WindowHandle : HWND Read FWindow;
  End;
