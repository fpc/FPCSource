{ %skiptarget=msdos,go32v2 }
{ a dataset can choose a field's class per field definition, as Delphi's TDataSet.GetFieldClass(FieldDef):
  TFieldDef.FieldClass and TFieldDef.CreateField ask that overload, whose default asks GetFieldClass(FieldType),
  so a dataset overriding only the older overload keeps working }
program tdbfieldclassdef1;

{$mode objfpc}{$h+}

uses
  SysUtils, DB;

type
  TMyLargeintField = class(TLargeintField);
  TMyStringField = class(TStringField);

  TBareDataSet = class(TDataSet)
  protected
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
  end;

  TPerDefDataSet = class(TBareDataSet)
  protected
    function GetFieldClass(FieldDef: TFieldDef): TFieldClass; override;
  end;

  TPerTypeDataSet = class(TBareDataSet)
  protected
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
  end;

  TPlainDataSet = class(TBareDataSet);

function TBareDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := grEOF;
end;

procedure TBareDataSet.InternalClose;
begin
end;

procedure TBareDataSet.InternalHandleException;
begin
end;

procedure TBareDataSet.InternalInitFieldDefs;
begin
end;

procedure TBareDataSet.InternalOpen;
begin
end;

function TBareDataSet.IsCursorOpen: Boolean;
begin
  Result := False;
end;

function TPerDefDataSet.GetFieldClass(FieldDef: TFieldDef): TFieldClass;
begin
  if FieldDef.Name = 'W64' then
    Result := TMyLargeintField
  else
    Result := inherited GetFieldClass(FieldDef);
end;

function TPerTypeDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  if FieldType = ftString then
    Result := TMyStringField
  else
    Result := inherited GetFieldClass(FieldType);
end;

procedure Check(aOk: Boolean; const aWhat: string);
begin
  if not aOk then begin
    Writeln('failed: ', aWhat);
    Halt(1);
  end;
end;

function NameOf(aClass: TFieldClass): string;
begin
  if Assigned(aClass) then
    Result := aClass.ClassName
  else
    Result := 'nil';
end;

var
  lPerDef: TPerDefDataSet;
  lPerType: TPerTypeDataSet;
  lPlain: TPlainDataSet;
  lField: TField;
begin
  lPerDef := TPerDefDataSet.Create(nil);
  lPerDef.FieldDefs.Add('W64', ftLargeint);
  lPerDef.FieldDefs.Add('I64', ftLargeint);
  Check(lPerDef.FieldDefs[0].FieldClass = TMyLargeintField, 'per def, W64: ' + NameOf(lPerDef.FieldDefs[0].FieldClass));
  Check(lPerDef.FieldDefs[1].FieldClass = TLargeintField, 'per def, I64: ' + NameOf(lPerDef.FieldDefs[1].FieldClass));
  lField := lPerDef.FieldDefs[0].CreateField(lPerDef);
  Check(lField.ClassType = TMyLargeintField, 'per def, W64 created: ' + lField.ClassName);
  lField := lPerDef.FieldDefs[1].CreateField(lPerDef);
  Check(lField.ClassType = TLargeintField, 'per def, I64 created: ' + lField.ClassName);
  lPerDef.Free;

  lPerType := TPerTypeDataSet.Create(nil);
  lPerType.FieldDefs.Add('S', ftString, 10);
  Check(lPerType.FieldDefs[0].FieldClass = TMyStringField, 'per type, S: ' + NameOf(lPerType.FieldDefs[0].FieldClass));
  lField := lPerType.FieldDefs[0].CreateField(lPerType);
  Check(lField.ClassType = TMyStringField, 'per type, S created: ' + lField.ClassName);
  lPerType.Free;

  lPlain := TPlainDataSet.Create(nil);
  lPlain.FieldDefs.Add('S', ftString, 10);
  Check(lPlain.FieldDefs[0].FieldClass = TStringField, 'plain, S: ' + NameOf(lPlain.FieldDefs[0].FieldClass));
  lPlain.Free;
  Writeln('ok');
end.
