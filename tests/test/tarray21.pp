program tarray21;

{$mode objfpc}{$H+}

uses
  Variants;

var
  foobar: IDispatch;

type
  TTest = class(TInterfacedObject, IDispatch)
    function GetTypeInfoCount(out count : longint) : HResult;stdcall;
    function GetTypeInfo(Index,LocaleID : longint;
      out TypeInfo): HResult;stdcall;
    function GetIDsOfNames(const iid: TGUID; names: Pointer;
      NameCount, LocaleID: LongInt; DispIDs: Pointer) : HResult;stdcall;
    function Invoke(DispID: LongInt;const iid : TGUID;
      LocaleID : longint; Flags: Word;var params;
      VarResult,ExcepInfo,ArgErr : pointer) : HResult;stdcall;
  end;

function TTest.GetTypeInfoCount(out count : longint) : HResult;stdcall;
begin
end;

function TTest.GetTypeInfo(Index,LocaleID : longint;
  out TypeInfo): HResult;stdcall;
begin
end;

function TTest.GetIDsOfNames(const iid: TGUID; names: Pointer;
  NameCount, LocaleID: LongInt; DispIDs: Pointer) : HResult;stdcall;
begin
end;

function TTest.Invoke(DispID: LongInt;const iid : TGUID;
  LocaleID : longint; Flags: Word;var params;
  VarResult,ExcepInfo,ArgErr : pointer) : HResult;stdcall;
begin
end;

procedure Test(aArr: array of Variant);
begin
  if Length(aArr) <> 3 then
    Halt(1);
  if aArr[0] <> 42 then
    Halt(2);
  if aArr[1] <> 'Test' then
    Halt(3);
  if IDispatch(aArr[2]) <> foobar then
    Halt(4);
end;

begin
  foobar := TTest.Create;
  Test([42, 'Test', foobar]);
  foobar := Nil;
end.
