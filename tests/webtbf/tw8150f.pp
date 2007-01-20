{ %fail }
{ %norun }

program WithForClassTypes;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

type
  TMyObject = class
    x: Integer;
    class procedure Foo; virtual;
    procedure Bar; virtual;
  end;

  TMyObject2 = class(TMyObject)
    class procedure Foo; override;
    procedure Bar; override;
  end;

  TMyClass = class of TMyObject;

{ TMyObject }

procedure TMyObject.Bar;
begin
  WriteLn('Bar ', Integer(Pointer(Self)),' ', x);
end;

class procedure TMyObject.Foo;
begin
  WriteLn('Foo');
end;

{ TMyObject2 }

procedure TMyObject2.Bar;
begin
  WriteLn('2Bar ', Integer(Pointer(Self)),' ', x);
end;

class procedure TMyObject2.Foo;
begin
  WriteLn('2Foo');
end;

var
  MyClass : TMyClass = TMyObject2;

begin
  with MyClass do begin
    Foo; // should work

    with Create do try // should work
      x := 3; // should work
      Bar; // should work
    finally
      Free; // should work
    end;

    Foo; // should work

// x := 1; // should not be allowed
 Bar; // should not be allowed
// Free; // should not be allowed
  end;
end.
