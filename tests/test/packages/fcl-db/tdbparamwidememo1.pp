{ %skiptarget=msdos,go32v2 }
{ TParam.AssignFromField and AssignToField take a wide memo's text as they do a wide string's;
  a parameter filled from a wide memo field has the type ftWideString, as one filled from a memo
  field has ftString }
program tdbparamwidememo1;

{$mode delphi}{$h+}

uses
  SysUtils, DB, BufDataset;

const
  cFirst: UnicodeString = 'a'#$0101'b';
  cSecond: UnicodeString = 'x'#$0103;

procedure Check(aOk: Boolean; const aWhat: string);
begin
  if not aOk then begin
    Writeln('failed: ', aWhat);
    Halt(1);
  end;
end;

var
  lData: TBufDataset;
  lField: TField;
  lParam: TParam;
begin
  lData := TBufDataset.Create(nil);
  lParam := TParam.Create(nil);
  try
    lData.FieldDefs.Add('W', ftWideMemo);
    lData.CreateDataset;
    lData.Open;
    lField := lData.FieldByName('W');
    Check(lField.DataType = ftWideMemo, 'the field is a wide memo');
    lData.Append;
    lField.AsWideString := cFirst;
    lData.Post;

    lParam.AssignFromField(lField);
    Check(lParam.AsWideString = cFirst, 'AssignFromField: the text');
    Check(lParam.DataType = ftWideString, 'AssignFromField: the type');

    lParam.DataType := ftWideMemo;
    lParam.Value := cSecond;
    lData.Edit;
    lParam.AssignToField(lField);
    lData.Post;
    Check(lField.AsWideString = cSecond, 'AssignToField: the text');
  finally
    lParam.Free;
    lData.Free;
  end;
  Writeln('ok');
end.
