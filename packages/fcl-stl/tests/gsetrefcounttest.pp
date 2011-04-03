{$mode objfpc}

unit gsetrefcounttest;

interface

uses fpcunit, testregistry, gset, gutil;

type 
  arr = class
    a:longint;
  end;
  lll=class
    class function c(a,b: arr):boolean;
  end;

type setlli=specialize RBSet<arr,lll>;

type TGSetRefCountTest = class(TTestCase)
  Published
    procedure SetTest;
  public
    procedure Setup;override;
  private 
    data:setlli;
  end;

implementation

class function lll.c(a,b: arr):boolean;
begin
  c:=a.a<b.a;
end;

procedure TGSetRefCountTest.SetTest;
var x:arr; i:longint;
    it:setlli.pnode;
begin
  for i:=0 to 20000 do begin
    x:=arr.create;
    x.a:=i;
    {code should crash on this insert}
    data.insert(x);
  end;
  it:=data.min;
  while it<>nil do begin
    writeln(it^.data.a);
    it:=data.next(it);
  end;
end;

procedure TGSetRefCountTest.Setup;
begin
  data:=setlli.create;
end;

initialization
  RegisterTest(TGSetRefCountTest);
end.
