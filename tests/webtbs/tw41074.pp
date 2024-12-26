program tw41074;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF FPC}
  {$ENDIF}
  {$IFnDEF FPC}System.{$ENDIF}SysUtils,
  {$IFnDEF FPC}System.{$ENDIF}Rtti;

type
  ITestInterface = interface
    ['{12345678-1234-1234-1234-123456789012}']
    procedure DoSomething;
  end;

  TTestClass = class(TInterfacedObject, ITestInterface)
  public
    procedure DoSomething;
  end;

procedure TTestClass.DoSomething;
begin
  //Writeln('TTestClass.DoSomething called');
end;

{procedure TestType(arg: IUnknown); overload;
begin
  Writeln('Argument of type IUnknown received in overload 1');
end;

procedure TestType(arg: Pointer); overload;
begin
  Writeln('Argument of type Pointer received in overload 2');
end;

procedure TestType(arg: TObject); overload;
begin
  Writeln('Argument of type TObject received in overload 3');
end;}

var
  obj: TTestClass;
  i: IUnknown;
  tval: TValue;
begin
  try
    obj := TTestClass.Create;
    //try
      //TestType(obj); // TObject anywhere
    {finally
      obj.Free;
    end;}
    { keep instance alive in case of conversion to IUnknown }
    i := obj;

    tval := obj;
    Writeln(tval.ToString);
    if tval.Kind <> tkClass then
      Halt(1);
    {
      Delphi: (TTestClass @ 0342BDB8)
      FPC: (pointer @ 0000000001614170)
        OR
           (interface @ 00000000015F4118)
           (if there is no overloading of the := operator for Pointer at TValue)
    }

    //Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

