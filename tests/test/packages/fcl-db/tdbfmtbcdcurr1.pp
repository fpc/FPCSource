{ %skiptarget=msdos,go32v2 }
{ TFMTBCDField.AsCurrency stores the value whatever the field's scale: a field
  of fewer than four decimals used to stay empty, and a zero raised }
program tdbfmtbcdcurr1;

{$mode delphi}{$h+}

uses
  SysUtils, DB, BufDataset;

const
  cScales: array[0..3] of Integer = (0, 2, 4, 6);

procedure Check(aOk: Boolean; const aWhat: string);
begin
  if not aOk then begin
    Writeln('failed: ', aWhat);
    Halt(1);
  end;
end;

procedure Store(aField: TField; aValue: Currency);
begin
  aField.Clear;
  aField.AsCurrency := aValue;
  Check(not aField.IsNull, aField.FieldName + ' stayed empty for ' + CurrToStr(aValue));
  Check(aField.AsCurrency = aValue, aField.FieldName + ' holds ' + CurrToStr(aField.AsCurrency));
end;

var
  lData: TBufDataset;
  lDef: TFieldDef;
  lField: TField;
  lIdx: Integer;
begin
  lData := TBufDataset.Create(nil);
  try
    for lIdx := Low(cScales) to High(cScales) do begin
      lDef := lData.FieldDefs.AddFieldDef;
      lDef.Name := 'F' + IntToStr(cScales[lIdx]);
      lDef.DataType := ftFmtBCD;
      lDef.Precision := 12;
      lDef.Size := cScales[lIdx];
    end;
    lData.CreateDataset;
    lData.Open;
    lData.Append;
    for lIdx := Low(cScales) to High(cScales) do begin
      lField := lData.FieldByName('F' + IntToStr(cScales[lIdx]));
      Store(lField, 12.25);
      Store(lField, 2);
      { a field of more than a currency's four decimals keeps its representation }
      if cScales[lIdx] = 6 then
        Check(lField.AsString = '2', lField.FieldName + ' reads as ' + lField.AsString);
      Store(lField, 0);
      Store(lField, -1.5);
    end;
    lData.Post;
  finally
    lData.Free;
  end;
  Writeln('ok');
end.
