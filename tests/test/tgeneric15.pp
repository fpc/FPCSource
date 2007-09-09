program Project1;

{$mode objfpc}{$H+}

type

  { TStack }

  generic TStack<T> = class(TObject)
   public
    procedure Clear; virtual;
    destructor Destroy; override;
  end;

  { TIntegerStack }

  TIntegerStack = class(specialize TStack<Integer>)
   public
    procedure Clear; override;
  end;

  { TIntegerStack2 }

  TIntegerStack2 = class(specialize TStack<Integer>)
   public
    procedure Clear; override;
  end;

var
  Idx : Longint;

{ TIntegerStack }

procedure TIntegerStack.Clear;
begin
  Writeln('new clear');
  Idx:=Idx or 1;
end;

{ TIntegerStack2 }

procedure TIntegerStack2.Clear;
begin
  Writeln('new clear2');
  Idx:=Idx or 2;
end;

{ TStack }

procedure TStack.Clear;
begin
  Writeln('old clear');
end;

destructor TStack.Destroy;
begin
  Writeln('old destroy');
  Clear;
end;


var
  s: TIntegerStack;
  s2: TIntegerStack2;
begin
  Idx:=0;

  s := TIntegerStack.Create;
  Writeln(s.ClassName);
  s.Free;

  s2 := TIntegerStack2.Create;
  Writeln(s2.ClassName);
  s2.Free;

  if Idx<>3 then
    halt(1);
end.
