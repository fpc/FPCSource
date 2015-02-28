{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{ Some (delphi) applications expect that the QueryInterface method is invoked as first
  priority to query for an interface and GetInterface as 2nd priority }

uses
  sysutils;

type
  ITest = interface
     ['{E80B0A2E-96ED-4F38-A6AC-E4E0B59F27F3}']
  end;

  TTest = class(TObject, IUnknown, ITest)
  private
    refcount: integer;
  public
    function QueryInterface(constref iid : tguid;out obj) : Hresult;stdcall;
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;
  end;

var
  called: Boolean = False;

function TTest.QueryInterface(constref IID: TGUID; out Obj): Hresult; stdcall;
begin
  called := true;
  if getinterface(iid,obj) then
   result:=S_OK
  else
   result:=longint(E_NOINTERFACE);
end;

function TTest._AddRef : longint;stdcall;
begin
  Inc(refcount);
  result := refcount;
end;

function TTest._Release : longint;stdcall;
begin
  Dec(refcount);
  result := refcount;
end;

var
  r: TTest;
  i: ITest;

procedure get(out obj: ITest);
begin
  obj := r as ITest;
end;

begin
  r := TTest.Create;
  r._AddRef;

  if not supports(r, ITest, i) or not called or (r.refcount<>2) then
    Halt(1);
  called := false;
  i := nil;

  get(i);
  if (i=nil) or not called or (r.refcount<>2) then
    Halt(1);
  i := nil;

  r._Release;
end.
