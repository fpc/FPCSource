{$mode delphi}

var
  err : boolean;

Type
  {copy-paste from LibX.pas}
  XInt                           = Longint;
  XUInt                          = Longword;
  XHandle                        = Pointer;
  XFile                          = XHandle;
  XFileMode                      = Set Of (
    xFileModeRead,
    xFileModeWrite
  );
  XResult                        = XInt;

Type
  TTest = Class(TObject)
    Constructor Create(Out Result: XResult; Const Handle: XFile; Const Mode: XFileMode);
  End;

  TTest2 = Class(TTest)
    Constructor Create(Out Result: XResult; Const FileName: AnsiString; Const Rights: XUInt); Overload;
    Constructor Create(Out Result: XResult; Const FileName: AnsiString; Const Mode: XFileMode); Overload;
  End;

Constructor TTest.Create(Out Result: XResult; Const Handle: XFile; Const Mode: XFileMode);
Begin
  WriteLn('TTest Create');
End;

Constructor TTest2.Create(Out Result: XResult; Const FileName: AnsiString; Const Rights: XUInt);
Begin
  WriteLn('TTest2-1 Create');
End;

Constructor TTest2.Create(Out Result: XResult; Const FileName: AnsiString; Const Mode: XFileMode);
Begin
  WriteLn('TTest2-2 Create');
  err:=false;
End;

Var
  T : TTest;
  C : PAnsiChar;
  X : XResult;
  M : XFileMode;
Begin
  err:=true;
  C := 'Foo';
  T := TTest2.Create(X, C, M);
  if err then
    halt(1);
End.
