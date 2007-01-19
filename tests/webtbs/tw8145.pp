program ClassOfDifference;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

var
  l: longint;

type
  TMyObject = class
    class procedure Foo;
    class procedure Foo2; virtual;
  end;

  TMyClass = class of TMyObject;

  TMyDerivedObject = class(TMyObject)
    class procedure Foo2; override;
  end;

  TMyDerivedClass = class of TMyDerivedObject;


  TMyObject2 = class
    class procedure Foo3;
  end;
  TMyClass2 = class of TMyObject2;

  TMyDerivedObject2 = class(TMyObject2)
  end;
  TMyDerivedClass2 = class of TMyDerivedObject2;


class procedure TMyObject.Foo;
begin
  if (l <> 1) then
    halt(1);
end;

class procedure TMyObject.Foo2;
begin
  if (l <> 2) then
    halt(2);
end;


class procedure TMyDerivedObject.Foo2;
begin
  if (l <> 3) then
    halt(3);
end;


class procedure TMyObject2.Foo3;
begin
  if (l <> 4) then
    halt(4);
end;


var
  MyClassA : TMyClass = TMyObject;
  MyClassB : TMyClass = TMyDerivedObject;
  MyDerivedClass : TMyDerivedClass = TMyDerivedObject;

  MyClassA2 : TMyClass2 = TMyObject2;
  MyClassB2 : TMyClass2 = TMyDerivedObject2;
  MyDerivedClass2 : TMyDerivedClass2 = TMyDerivedObject2;

begin

  l := 1;
  TMyObject.Foo; //works in FPC and Delphi
  TMyDerivedObject.Foo; //works in FPC and Delphi

  MyClassA.Foo; //works in FPC and Delphi
  MyClassB.Foo; //works in FPC and Delphi
  MyDerivedClass.Foo; //works in FPC and Delphi
  TMyClass.Foo; //works only in Delphi
  TMyDerivedClass.Foo; //works only in Delphi

  l := 2;
  TMyObject.Foo2;
  MyClassA.Foo2;
  TMyClass.Foo2;

  l := 3;
  TMyDerivedObject.Foo2;
  TMyDerivedClass.Foo2;
  MyClassB.Foo2;


  l := 4;
  TMyObject2.Foo3; //works in FPC and Delphi
  TMyDerivedObject2.Foo3; //works in FPC and Delphi

  MyClassA2.Foo3; //works in FPC and Delphi
  MyClassB2.Foo3; //works in FPC and Delphi
  MyDerivedClass2.Foo3; //works in FPC and Delphi
  TMyClass2.Foo3; //works only in Delphi
  TMyDerivedClass2.Foo3; //works only in Delphi

end.

