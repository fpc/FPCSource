{$mode objfpc}
{$ASSERTIONS ON}

unit gvectortest;

interface

uses fpcunit, testregistry, gvector;

type vectorlli=specialize TVector<longint>;
     rec=record
      a,b:longint;
     end;
     vectorrec=specialize TVector<rec>;
     vectorintf=specialize TVector<IInterface>;

type TGVectorTest = class(TTestCase)
  Published
    procedure PushBackTest;
    procedure ResizeTest;
    procedure PopbackTest;
    procedure InsertEraseTest;
    procedure MutableTest;
    procedure EraseManagedTest;
  public
    procedure Setup;override;
  private
    data:vectorlli;
  end;

implementation

procedure TGVectorTest.PushBackTest;
var i:longint;
begin
  AssertEquals('Not IsEmpty', true, data.IsEmpty);
  for i:=0 to 10 do
    data.pushBack(i);
  for i:=0 to 10 do
    AssertEquals('Wrong data', i, data[i]);

  writeln(data[11]);

  AssertEquals('Wrong size', 11, data.size);
  AssertEquals('IsEmpty', false, data.IsEmpty);
end;

procedure TGVectorTest.ResizeTest;
var i:longint;
begin
  AssertEquals('Not IsEmpty', true, data.IsEmpty);
  data.resize(50);
  AssertEquals('IsEmpty', false, data.IsEmpty);
  for i:=0 to 49 do
    data[i]:=3*i;
  for i:=0 to 49 do
    AssertEquals('Wrong data', 3*i, data[i]);
  AssertEquals('Wrong size', 50, data.size);
end;

procedure TGVectorTest.PopbackTest;
var i:longint;
begin
  for i:=0 to 49 do begin
    data.pushBack(5*i);
    AssertEquals('Wrong end', 5*i, data.back);
    AssertEquals('Wrong front', 0, data.front);
  end;
  for i:=1 to 10 do begin
    data.popBack;
    AssertEquals('Wrong end after popback', 5*(49-i), data.back);
  end;
end;

procedure TGVectorTest.InsertEraseTest;
var i:longint;
begin
  for i:=0 to 9 do
    data.pushBack(i);
  data.insert(3,100);
  for i:=0 to 2 do
    AssertEquals('Wrong data before insert', i, data[i]);
  AssertEquals('Wrong data', 100, data[3]);
  for i:=4 to 10 do
    AssertEquals('Wrong data after insert', i-1, data[i]);
  data.erase(4);
  for i:=4 to 9 do
    AssertEquals('Wrong data after erase', i, data[i]);
  AssertEquals('Wrong data before erase', 100, data[3]);
  for i:=0 to 2 do
    AssertEquals('Wrong data before erase', i, data[i]);
end;

procedure TGVectorTest.MutableTest;
var dat:vectorrec;
begin
  dat:=vectorrec.create;
  dat.resize(2);
  dat.mutable[0]^.a:=5;
  dat.mutable[0]^.b:=7;
  AssertEquals('Wrong data', 5, dat[0].a);
  AssertEquals('Wrong data', 7, dat[0].b);
  dat.mutable[0]^.a:=45;
  dat.mutable[0]^.b:=47;
  AssertEquals('Wrong data', 45, dat[0].a);
  AssertEquals('Wrong data', 47, dat[0].b);
end;

type
  { counts its references and frees nothing when the count reaches 0, so a
    count that goes wrong is read rather than freeing memory still in use }
  TCounted = class(TObject, IInterface)
    count: longint;
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: tguid; out obj): longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;

function TCounted.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: tguid; out obj): longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  pointer(obj):=nil;
  result:=longint($80004002);
end;

function TCounted._AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  inc(count);
  result:=count;
end;

function TCounted._Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  dec(count);
  result:=count;
end;

procedure TGVectorTest.EraseManagedTest;
var intfs:vectorintf;
    a,b,c:TCounted;

  { the reference converted for the call is released when this returns }
  procedure push(o:TCounted);
  begin
    intfs.pushBack(o);
  end;

begin
  a:=TCounted.create;
  b:=TCounted.create;
  c:=TCounted.create;
  intfs:=vectorintf.create;
  push(a);
  push(b);
  intfs.erase(0);
  AssertEquals('Wrong count of erased item', 0, a.count);
  AssertEquals('Wrong count after erase', 1, b.count);
  push(c);
  AssertEquals('Wrong count after push back', 1, b.count);
  AssertEquals('Wrong count of pushed item', 1, c.count);
  intfs.free;
  AssertEquals('Wrong count after free', 0, b.count);
  AssertEquals('Wrong count of pushed item after free', 0, c.count);
  a.free;
  b.free;
  c.free;
end;

procedure TGVectorTest.Setup;
begin
  data:=vectorlli.create;
end;

initialization
  RegisterTest(TGVectorTest);
end.
