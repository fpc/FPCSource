program tw27185;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this };

var
  NormalClassInit: Boolean = False;
  NormalClassDone: Boolean = False;
  NestedTypeClassInit: Boolean = False;
  NestedTypeClassDone: Boolean = False;
  NestedTypeClassNestedClassInit: Boolean = False;
  NestedTypeClassNestedClassDone: Boolean = False;

Type

  { TNormalClass }

  TNormalClass = class
    public
      class constructor Create;
      class destructor Destroy;
  end;

  { TNestedTypeClass }

  TNestedTypeClass = class
    private
      type

        { TNestedClass }

        TNestedClass = class
          public
            class constructor Create;
            class destructor Destroy;
        end;

    public
      class constructor Create;
      class destructor Destroy;
  end;

{ TNestedTypeClass }

class constructor TNestedTypeClass.Create;
begin
  NestedTypeClassInit := True;
  //WriteLn('class constructor TNestedTypeClass.Create');
end;

class destructor TNestedTypeClass.Destroy;
begin
  NestedTypeClassDone := True;
  //WriteLn('class destructor TNestedTypeClass.Destroy');
end;

{ TNormalClass }

class constructor TNormalClass.Create;
begin
  NormalClassInit := True;
  //WriteLn('class constructor TNormalClass.Create');
end;

class destructor TNormalClass.Destroy;
begin
  NormalClassDone := False;
  //WriteLn('class destructor TNormalClass.Destroy');
end;

{ TNestedTypeClass.TNestedClass }

class constructor TNestedTypeClass.TNestedClass.Create;
begin
  NestedTypeClassNestedClassInit := True;
  //WriteLn('class constructor TNestedTypeClass.TNestedClass.Create');
end;

class destructor TNestedTypeClass.TNestedClass.Destroy;
begin
  NestedTypeClassNestedClassDone := True;
  //WriteLn('class destructor TNestedTypeClass.TNestedClass.Destroy');
end;

begin
  if not NormalClassInit then
    Halt(1);
  if not NestedTypeClassInit then
    Halt(2);
  if not NestedTypeClassNestedClassInit then
    Halt(3);
end.

