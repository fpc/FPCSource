{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
program tnameof1;

type
  TMyInt = Integer;
  TColor = (clRed, clGreen);

  TRec = record
    MyField: Integer;
    procedure DoRec;
  end;

  TBird = class
  private
    FWings: Integer;
    function GetItem(Index: Integer): Integer;
    procedure SetOnly(AValue: Integer);
  public
    class var ClassFld: Integer;
    procedure Fly(Speed: Integer);
    class procedure ClassFly;
    property Wings: Integer read FWings;
    property Items[Index: Integer]: Integer read GetItem;
    property WriteOnly: Integer write SetOnly;
  end;

  generic TGen<T> = class
    Value: T;
    function TypeParamName: String;
  end;

const
  MyConst = 3;
  MyTypedConst: Integer = 4;
  ConstName = NameOf(MyConst);

var
  GlobalVar: Integer;
  r: TRec;
  b: TBird;
  c: TColor;

procedure TRec.DoRec;
begin
end;

function TBird.GetItem(Index: Integer): Integer;
begin
  Result:=Index;
end;

procedure TBird.SetOnly(AValue: Integer);
begin
end;

procedure TBird.Fly(Speed: Integer);
var
  s: String;
begin
  s:=NameOf(FWings);
  if s<>'FWings' then
    halt(100);
  s:=NameOf(Wings);
  if s<>'Wings' then
    halt(101);
  s:=NameOf(Self.Fly);
  if s<>'Fly' then
    halt(102);
  s:=NameOf(Speed);
  if s<>'Speed' then
    halt(103);
end;

class procedure TBird.ClassFly;
begin
end;

function TGen.TypeParamName: String;
begin
  Result:=NameOf(Value);
end;

procedure Check(const Got, Expected: String; Code: Integer);
begin
  if Got<>Expected then
    begin
      writeln('Error ',Code,': got "',Got,'" expected "',Expected,'"');
      halt(Code);
    end;
end;

procedure MyProc(MyArg: Integer);
var
  LocalVar: Integer;
begin
  Check(NameOf(MyArg),'MyArg',1);
  Check(NameOf(LocalVar),'LocalVar',2);
end;

function MyFunc: Integer;
begin
  Check(NameOf(Result),'Result',3);
  Check(NameOf(MyFunc),'MyFunc',4);
  MyFunc:=1;
end;

begin
  MyProc(1);
  MyFunc;
  Check(NameOf(GLOBALVAR),'GlobalVar',10);
  Check(NameOf(MyProc),'MyProc',11);
  Check(NameOf(MyConst),'MyConst',12);
  Check(ConstName,'MyConst',13);
  Check(NameOf(MyTypedConst),'MyTypedConst',14);
  Check(NameOf(TMyInt),'TMyInt',15);
  Check(NameOf(Integer),'Integer',16);
  Check(NameOf(r),'r',17);
  Check(NameOf(r.MyField),'MyField',18);
  Check(NameOf(TRec.MyField),'MyField',19);
  Check(NameOf(TRec.DoRec),'DoRec',20);
  Check(NameOf(TBird),'TBird',21);
  Check(NameOf(b.Fly),'Fly',22);
  Check(NameOf(TBird.Fly),'Fly',23);
  Check(NameOf(TBird.ClassFly),'ClassFly',24);
  Check(NameOf(b.Wings),'Wings',25);
  Check(NameOf(TBird.Wings),'Wings',26);
  Check(NameOf(b.Items),'Items',27);
  Check(NameOf(b.WriteOnly),'WriteOnly',28);
  Check(NameOf(b.ClassFld),'ClassFld',29);
  Check(NameOf(TBird.ClassFld),'ClassFld',30);
  Check(NameOf(clGreen),'clGreen',31);
  Check(NameOf(TColor.clRed),'clRed',32);
  Check(NameOf(c),'c',33);
  Check(NameOf(System),'System',34);
  Check(NameOf(System.SizeInt),'SizeInt',35);
  Check(NameOf(specialize TGen<Integer>),'TGen',36);
  Check(NameOf(specialize TGen<TBird>.Value),'Value',37);
  Check(NameOf(TObject.Create),'Create',38);
  with r do
    Check(NameOf(MyField),'MyField',39);
  Check(NameOf(b.FWings),'FWings',40);
  b:=TBird.Create;
  b.Fly(1);
  Check(specialize TGen<Integer>.Create.TypeParamName,'Value',41);
  writeln('ok');
end.
