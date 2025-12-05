program tw41526;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Rtti,
  TypInfo,
  StrUtils;

type
  // Simple class
  TMyClass = class
  end;

  // Simple record
  TMyRecord = record
    A, B: Integer;
  end;

  // Simple interface
  IMyInterface = interface
    ['{6BB98F7E-4B9E-4C88-8F76-90C2E47FB3D9}']
    procedure DoSomething;
  end;

procedure CheckName(const aActual, aExpected: String);
var
  tmpactual, tmpexpected: String;
begin
  tmpactual := StringReplace(aActual, ' ', '', [rfReplaceAll]);
  tmpexpected := StringReplace(aExpected, ' ', '', [rfReplaceAll]);
  if not SameText(tmpactual, tmpexpected) then begin
    Writeln('Name mismatch');
    Writeln('   Expected: ', tmpexpected);
    Writeln('   Actual: ', tmpactual);
    ExitCode := 3;
  end;
end;

var
  Ctx: TRttiContext;
  RType: TRttiType;
  iname, sname: String;
begin
  Ctx := TRttiContext.Create;
  try
    RType := Ctx.GetType(TypeInfo(Integer));
    if RType <> nil then
      iname := RType.Name
    else begin
      Writeln('Integer RTTI not found');
      Halt(1);
    end;

    RType := Ctx.GetType(TypeInfo(string));
    if RType <> nil then
      sname := RType.Name
    else begin
      Writeln('String RTTI not found');
      Halt(2);
    end;

    // Dynamic array type (array of Integer)
    RType := Ctx.GetType(TypeInfo(TArray<Integer>));
    if RType <> nil then begin
      Writeln('TArray<Integer> Name:    ', RType.Name);
      CheckName(RType.Name, 'TArray<System.' + iname + '>');
    end else
      Writeln('TArray<Integer> RTTI not found');

    // Dynamic array type (array of string)
    RType := Ctx.GetType(TypeInfo(TArray<string>));
    if RType <> nil then begin
      Writeln('TArray<string> Name:    ', RType.Name);
      CheckName(RType.Name, 'TArray<System.' + sname + '>');
    end else
      Writeln('TArray<string> RTTI not found');

    // Dynamic array type (array of TMyClass)
    RType := Ctx.GetType(TypeInfo(TArray<TMyClass>));
    if RType <> nil then begin
      Writeln('TArray<TMyClass> Name:    ', RType.Name);
      CheckName(RType.Name, 'TArray<tw41526.TMyClass>');
    end else
      Writeln('TArray<TMyClass> RTTI not found');

    // Dynamic array type (array of TMyRecord)
    RType := Ctx.GetType(TypeInfo(TArray<TMyRecord>));
    if RType <> nil then begin
      Writeln('TArray<TMyRecord> Name:    ', RType.Name);
      CheckName(RType.Name, 'TArray<tw41526.TMyRecord>');
    end else
      Writeln('TArray<TMyRecord> RTTI not found');

    // Dynamic array type (array of IMyInterface)
    RType := Ctx.GetType(TypeInfo(TArray<IMyInterface>));
    if RType <> nil then begin
      Writeln('TArray<IMyInterface> Name:    ', RType.Name);
      CheckName(RType.Name, 'TArray<tw41526.IMyInterface>');
    end else
      Writeln('TArray<IMyInterface> RTTI not found');

  finally
    Ctx.Free;
  end;

  //Writeln;
  //Writeln('Press ENTER to exit...');
  //Readln;
end.

