program trtti27;

{ TRttiType.GetProperties must list all properties - also inherited }

{$IFDEF DCC}
  {$APPTYPE CONSOLE}
{$ELSE}
  {$mode objfpc}{$H+}
  {$modeswitch arrayoperators}
{$ENDIF}

uses
  Classes, SysUtils, TypInfo, Rtti;

type
  TProp = record
    Name: string;
    Default: Integer;
  end;
  TProps = array of TProp;
  {$IFDEF DCC}
  TRttiPropertyArray = TArray<TRttiProperty>;
  {$ENDIF}

  { TMyObject1 }

  TMyObject1 = class(TPersistent)
  private
    FInt: integer;
  published
    property PublicInt: integer read FInt default 1;
  end;

  TMyObject2 = class(TMyObject1)
  published
    property PublicInt default 2;
  end;

procedure GetProps(const aClass: TClass; out Props: TProps);
var
  fContext: TRttiContext;
  fType: TRttiType;
  xProp: TRttiProperty;
  xProps: TRttiPropertyArray;
  I: Integer;
  P: TProp;
begin
  Props := nil;
  fContext := TRttiContext.Create;
  fType := fContext.GetType(aClass);

  xProps := fType.GetProperties;
  for I := High(xProps) downto 0 do
  begin
    xProp := xProps[I];
    if (xProp.Visibility=mvPublished) and (xProp.PropertyType.IsOrdinal) then
    begin
      P.Name := xProp.Name;
      P.Default := (xProp{$IFDEF DCC} as TRttiInstanceProperty{$ENDIF}).Default;
      Props := Props + [P];
    end;
  end;
end;


function DefaultsToString(const aClass: TClass): string;
var
  Props: TProps;
  P: TProp;
begin
  Result := '';
  GetProps(aClass, Props);
  for P in Props do
    Result := Result + IntToStr(P.Default);
end;

begin
  if DefaultsToString(TMyObject1)<>'1' then
    Halt(1);
  if DefaultsToString(TMyObject2)<>'12' then
    Halt(2);
end.
