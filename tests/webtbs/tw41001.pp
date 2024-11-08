program test;

{$RTTI EXPLICIT METHODS([vcPublished]) PROPERTIES([vcPublished]) FIELDS([vcPublished])}

{$mode delphi}{$H+}
{$codepage utf8}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  Rtti
  { you can add units after this };

type

  TClass = class(TPersistent)
  published
    [StoredAttribute('Metadata1')]
    procedure Run1;
    [StoredAttribute('Metadata2')]
    class procedure Run2;
  end;

{ TClass }

procedure TClass.Run1;
begin
end;

class procedure TClass.Run2;
begin
end;

var
  LAttr: StoredAttribute;
  LContext: TRttiContext;
  LMethod: TRttiMethod;
begin
  LContext := TRttiContext.Create;
  for LMethod in LContext.GetType(TClass).GetMethods do
  begin
    LAttr := StoredAttribute(LMethod.GetAttribute(StoredAttribute));
    if LAttr <> nil then
      WriteLn(LMethod.Name, ': ', LAttr.Name);
  end;
  LContext.Free;
end.
