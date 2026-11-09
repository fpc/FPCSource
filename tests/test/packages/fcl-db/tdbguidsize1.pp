{ %target=win32,win64 }
{ a TStringField descendant that sets its Size before TStringField.Create keeps it, as Delphi's:
  a TGuidField created in code holds 38 characters; a TStringField and a TWideStringField start at 20 }
program tdbguidsize1;

{$mode delphi}{$h+}

uses
  SysUtils, DB;

procedure Check(aOk: Boolean; const aWhat: string);
begin
  if not aOk then begin
    Writeln('failed: ', aWhat);
    Halt(1);
  end;
end;

var
  lGuid: TGuidField;
  lStr: TStringField;
  lWide: TWideStringField;
begin
  lGuid := TGuidField.Create(nil);
  Check(lGuid.Size = 38, 'TGuidField.Size: ' + IntToStr(lGuid.Size));
  lGuid.Free;
  lStr := TStringField.Create(nil);
  Check(lStr.Size = 20, 'TStringField.Size: ' + IntToStr(lStr.Size));
  lStr.Free;
  lWide := TWideStringField.Create(nil);
  Check(lWide.Size = 20, 'TWideStringField.Size: ' + IntToStr(lWide.Size));
  lWide.Free;
  Writeln('ok');
end.
