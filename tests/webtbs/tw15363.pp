{$mode delphi}
uses
  Classes, SysUtils; 

type
  ITest = interface ['{AAAA09DA-4019-4A5C-A450-3631A73CF288}']
    function TestIt: integer;
  end;

  TTestBE = class (TObject, ITest)
    function TestIt: integer;
    { IInterface }
    function _AddRef: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function QueryInterface(constref IID: TGUID; out Obj): HResult; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  End;

  TTest = class (TPersistent, IInterface)
    BE : TTestBE;
    protected
    { IInterface }
    function _AddRef: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function QueryInterface(constref IID: TGUID; out Obj): HResult; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  End;

function TTestBE.TestIt : integer;
Begin
  result := 1;
End;

function TTest._AddRef: Integer;
begin
  Result := -1;
end;

function TTest._Release: Integer;
begin
  Result := -1;
end;

function TTest.QueryInterface(constref IID: TGUID; out Obj): HResult;
begin
  Result := BE.QueryInterface(IID, obj);
end;

function TTestBE._AddRef: Integer;
begin
  Result := -1;
end;

function TTestBE._Release: Integer;
begin
  Result := -1;
end;

function TTestBE.QueryInterface(constref IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj)
    then Result := 0
end;




Var
  Test : TTest;
  A    : ITest;
begin
  Test    := TTest.Create;
  Test.BE := TTestBE.Create;

  // Works ok in Lazarus and Delphi
  Test.BE.GetInterface (ITest, A);
  // Works ok in Lazarus. Delphi will not compile this line
  A := Test.BE As ITest;

  // Both Delphi and Lazarus return nil ptr
  Test.GetInterface(ITest, A);

  // Works in Lazarus
  Test.QueryInterface (ITest, A);

  // Lazarus throws typecast error.
  // Works fine in delphi because delphi calls QueryInterface while Lazarus does not
  A := Test As ITest;
end.

