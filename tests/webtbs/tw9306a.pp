{$mode objfpc}

type
  IIntf = interface
    function Foo(const S: string): string;
  end;

  IIntf2 = interface(IIntf)
    function Foo(const S: string): Integer;
  end;

  TIntf = class(TInterfacedObject, IIntf)
  protected
    { IIntf }
    function Foo(const S: string): string;
  end;

  TIntf2 = class(TIntf, IIntf2)
  public
    { IIntf2 }
    function Foo(const S: string): Integer; overload;
  end;

var
  erridx : longint;

{ TIntf }

function TIntf.Foo(const S: string): string;
begin
  writeln('TIntf.Foo: ',S);
  if erridx=0 then
    erridx:=1;
  result:=S;
end;

{ TIntf2 }

function TIntf2.Foo(const S: string): Integer;
begin
  writeln('TIntf2.Foo: ',S);
  if erridx=1 then
    erridx:=2;
  result:=0;
end;

var
  i1 : IIntf;
  i2 : IIntf2;
begin
  erridx:=0;

  i1:=TIntf2.Create;
  i1.Foo('1234');

  i2:=TIntf2.Create;
  i2.Foo('1234');
  if erridx<>2 then
    begin
      writeln('Error');
      halt(1);
    end;
end.
