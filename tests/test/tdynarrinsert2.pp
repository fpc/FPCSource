{ Insert a constant into a dynamic array of a managed type: the constant is
  stored in a temp whose init code is a managed assignment, which pass_1 turns
  into a helper call. The parameter ordering of that call searches the init
  code of the temp and must not find freed nodes. }
program tdynarrinsert2;

{$mode objfpc}{$H+}

type
  TStrs = array of AnsiString;

  TFoo = class
    Items: TStrs;
    procedure Add;
  end;

var
  errors: LongInt = 0;

procedure Check(const aRow, aGot, aWant: AnsiString);
begin
  if aGot<>aWant then
    begin
      Writeln(aRow, ': got "', aGot, '" expected "', aWant, '"');
      Inc(errors);
    end;
end;

function ShowStrs(const a: TStrs): AnsiString;
var
  i: LongInt;
begin
  Result:='';
  for i:=0 to High(a) do
    Result:=Result+'['+a[i]+']';
end;

procedure TFoo.Add;
var
  i: LongInt;
begin
  for i:=0 to 1 do
    begin
      if i=5 then
        Break
      else
        Insert('',Items,Length(Items));
    end;
end;

var
  a: TStrs;
  Foo: TFoo;
begin
  a:=nil;
  Insert('',a,Length(a));
  Check('empty at end', ShowStrs(a), '[]');
  Insert('x',a,0);
  Check('const at start', ShowStrs(a), '[x][]');
  Insert('',a,1);
  Check('empty in middle', ShowStrs(a), '[x][][]');

  Foo:=TFoo.Create;
  Foo.Items:=nil;
  Insert('y',Foo.Items,0);
  Foo.Add;
  Check('field', ShowStrs(Foo.Items), '[y][][]');
  Foo.Free;

  if errors<>0 then
    Halt(1);
  Writeln('ok');
end.
