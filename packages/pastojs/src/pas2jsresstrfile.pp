unit pas2jsresstrfile;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpJSON;

Type

  { TResourceStringsFile }
  EResourceStringsFile = Class(Exception);

  TResourceStringsFile = Class(TObject)
  Private
    FCurrentUnit: TJSONStringType;
    FStrings : TJSONObject;
    FUnit : TJSONObject;
    function GetStringsCount: Integer;
    function GetUnitCount: Integer;
    function GetUnitStringsCount: Integer;
  Protected
    property Strings : TJSONObject Read FStrings;
    property CurrUnit : TJSONObject Read FUnit;
  Public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Clear;
    Procedure ClearUnit;
    Procedure StartUnit(Const aUnitName : TJSONStringType);
    Procedure AddString(Const aName,aValue : TJSONStringType); overload;
    Procedure AddString(Const aUnit,aName,aValue : TJSONStringType); overload;
    Function toString : String; override;
    Function AsString : TJSONStringType;
    Property CurrentUnit : TJSONStringType Read FCurrentUnit;
    Property UnitCount : Integer Read GetUnitCount;
    Property StringsCount : Integer Read GetStringsCount;
    Property CurrentUnitStringsCount : Integer Read GetUnitStringsCount;
  end;


implementation

Resourcestring
   SErrNoCurrentUnit = 'No current unit.';
   SErrInvalidUnitName = 'Invalid unit name: "%s"';
   SErrInvalidStringName = 'Invalid TJSONStringType name: "%s"';

{ TResourceStringsFile }

function TResourceStringsFile.GetStringsCount: Integer;

Var
  I : Integer;

begin
  Result:=0;
  For I:=0 to FStrings.Count-1 do
    Result:=Result+TJSONObject(FStrings.Items[i]).Count;
end;

function TResourceStringsFile.GetUnitCount: Integer;
begin
  Result:=FStrings.Count;
end;

function TResourceStringsFile.GetUnitStringsCount: Integer;
begin
  if Assigned(FUnit) then
    Result:=FUnit.Count
  else
    Result:=0;
end;

constructor TResourceStringsFile.Create;
begin
  FStrings:=TJSONObject.Create;
  FUnit:=nil;
end;

destructor TResourceStringsFile.Destroy;
begin
  FUnit:=nil;
  FreeAndNil(FStrings);
  inherited Destroy;
end;

procedure TResourceStringsFile.Clear;
begin
  FStrings.Clear;
end;

procedure TResourceStringsFile.ClearUnit;
begin
  If Assigned(FUnit) then
    FUnit.Clear;
end;

procedure TResourceStringsFile.StartUnit(const aUnitName: TJSONStringType);

Var
  I : Integer;

begin
  if aUnitName=FCurrentUnit then exit;
  if not IsValidIdent(aUnitName,True,True) then
     Raise EResourceStringsFile.CreateFmt(SErrInvalidUnitName,[aUnitName]);
  I:=FStrings.IndexOfName(aUnitName);
  if (I<>-1) then
    FUnit:=FStrings.Items[i] as TJSONObject
  else
    begin
    FUnit:=TJSONObject.Create;
    FStrings.Add(aUnitName,FUnit);
    end;
  FCurrentUnit:=aUnitName;
end;

procedure TResourceStringsFile.AddString(const aName, aValue: TJSONStringType);
begin
  if not IsValidIdent(aName,False,False) then
    Raise EResourceStringsFile.CreateFmt(SErrInvalidStringName,[aName]);
  if (FUnit=Nil) then
    Raise EResourceStringsFile.Create(SErrNoCurrentUnit);
  FUnit.Add(aName,aValue);
end;

procedure TResourceStringsFile.AddString(const aUnit, aName, aValue: TJSONStringType);
begin
  StartUnit(aUnit);
  AddString(aName,aValue);
end;

function TResourceStringsFile.toString: String;
begin
  Result:=AsString;
end;

function TResourceStringsFile.AsString: TJSONStringType;
begin
  Result:=FStrings.FormatJSON();
end;

end.

