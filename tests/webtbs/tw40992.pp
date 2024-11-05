{ %TARGET = win32,win64,linux,darwin }

program tw40992;

{$mode objfpc}{$H+}

uses
  Classes,
  TypInfo,
  Rtti,
  Math,
  SysUtils
  {$ifndef Windows}
  , ffi.manager
  {$endif};

type
  TMyClass=class
    public
      MyField: integer;
  end;

  TTestClass=class
    public
      Info: string;
      function TestAll(arg1: TPoint; arg2: TRect; arg3: TMyClass; arg4: Integer; arg5: Pointer; arg6: Double; arg7: Boolean; arg8: string): TPoint;
  end;

function TTestClass.TestAll(arg1: TPoint; arg2: TRect; arg3: TMyClass; arg4: Integer; arg5: Pointer; arg6: Double; arg7: Boolean; arg8: string): TPoint;
begin
  WriteLn('Self Info: ', Self.Info);
  WriteLn('getted Point(', arg1.X, ', ', arg1.Y, ')');
  if arg1.X <> 123 then
    Halt(1);
  if arg1.Y <> 456 then
    Halt(2);
  WriteLn('getted Rect(', arg2.Left, ', ', arg2.Top, ', ', arg2.Right, ', ', arg2.Bottom, ')');
  if arg2.Left <> 12 then
    Halt(3);
  if arg2.Top <> 34 then
    Halt(4);
  if arg2.Right <> 56 then
    Halt(5);
  if arg2.Bottom <> 78 then
    Halt(6);
  WriteLn('my class field: ', arg3.MyField);
  if arg3.MyField <> 123456 then
    Halt(7);
  WriteLn('integer: ', arg4);
  if arg4 <> 3456789 then
    Halt(8);
  WriteLn('my class (by pointer) field: ', TMyClass(arg5).MyField);
  if TMyClass(arg5).MyField <> 123456 then
    Halt(9);
  WriteLn('double: ', arg6:0:5);
  if not SameValue(arg6, 9876.54321) then
    Halt(10);
  WriteLn('boolean: ', arg7);
  if not arg7 then
    Halt(11);
  WriteLn('string: ', arg8);
  if arg8 <> 'simple str' then
    Halt(12);

  Result := Point(1111, 2222);
end;

var
  p, pnt_arg: Pointer;
  point, pnt_ret: TPoint;
  rect: TRect;
  myClass: TMyClass;
  int: Integer;
  dbl: Double;
  bln: Boolean;
  str: string;
  testClass: TTestClass;

  val1, val2, val3, val5, val_self, val_ret: TValue;
begin
  p := @TTestClass.TestAll;

  point := TPoint.Create(123, 456);
  rect := TRect.Create(12, 34, 56, 78);

  myClass := TMyClass.Create;
  myClass.MyField:=123456;

  pnt_arg := Pointer(myClass);

  TValue.Make(@point, TypeInfo(TPoint), val1);
  TValue.Make(@rect, TypeInfo(TRect), val2);
  TValue.Make(@myClass, TypeInfo(TMyClass), val3);
  TValue.Make(@pnt_arg, TypeInfo(Pointer), val5);

  int := 3456789;
  dbl := 9876.54321;
  bln := True;
  str := 'simple str';

  testClass := TTestClass.Create;
  testClass.Info:='TestClass Information';
  TValue.Make(@testClass, TypeInfo(TTestClass), val_self);

  try
    val_ret := Rtti.Invoke(p, [val_self, val1, val2, val3, int, val5, dbl, bln, str], ccReg, TypeInfo(TPoint), False, False);
  except
    on e: ENotImplemented do begin
      Writeln('Invoke not available');
      Exit;
    end else
      raise;
  end;
  pnt_ret := TPoint(val_ret.GetReferenceToRawData^);
  WriteLn('returned Point(', pnt_ret.X, ', ', pnt_ret.Y, ')');
  if pnt_ret.X <> 1111 then
    Halt(13);
  if pnt_ret.Y <> 2222 then
    Halt(14);

  //ReadLn;
end.

