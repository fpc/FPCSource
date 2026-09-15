{$mode delphi}{$H+}
program tnameof2;

type
  TBird<T> = class
    Wings: T;
    procedure Fly;
  end;

  { specializing TNest in a NameOf argument parses a nested NameOf }
  TNest<T> = class
  const
    Title = NameOf(T);
  public
    procedure Fly;
  end;

  TRec = record
    function Get<T>: T;
  end;

procedure TBird<T>.Fly;
begin
end;

procedure TNest<T>.Fly;
begin
end;

function TRec.Get<T>: T;
begin
  Result:=Default(T);
end;

function Foo<T>(a: T): T;
begin
  Result:=a;
end;

procedure Check(const Got, Expected: String; Code: Integer);
begin
  if Got<>Expected then
    begin
      writeln('Error ',Code,': got "',Got,'" expected "',Expected,'"');
      halt(Code);
    end;
end;

const
  BirdName = NameOf(TBird<Boolean>);

var
  b: TBird<Integer>;
  r: TRec;
begin
  Check(BirdName,'TBird',1);
  Check(NameOf(TBird<Boolean>),'TBird',2);
  Check(NameOf(TBird<Boolean>.Fly),'Fly',3);
  Check(NameOf(TBird<TObject>.Wings),'Wings',4);
  Check(NameOf(b),'b',5);
  Check(NameOf(b.Wings),'Wings',6);
  Check(NameOf(Foo<Integer>),'Foo',7);
  Check(NameOf(TObject.Create),'Create',8);
  Check(NameOf(r.Get<Integer>),'Get',9);
  { first use of TNest<Boolean>, must be in a NameOf argument }
  Check(NameOf(TNest<Boolean>.Fly),'Fly',10);
  Check(TNest<Integer>.Title,'T',11);
  writeln('ok');
end.
