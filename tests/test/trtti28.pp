{ The categories of the RTTI directive apply in whatever order they are
  written, an empty one included. }
program trtti28;

{$mode delphi}

uses
  typinfo;

type
{$RTTI EXPLICIT METHODS([]) PROPERTIES([vcPublic]) FIELDS([vcPublic])}
  TA = class
  public
    F: LongInt;
    procedure M;
    property P: LongInt read F;
  end;
{$RTTI EXPLICIT PROPERTIES([vcPublic]) FIELDS([vcPublic]) METHODS([])}
  TB = class
  public
    F: LongInt;
    procedure M;
    property P: LongInt read F;
  end;
{$RTTI EXPLICIT FIELDS([vcPublic]) METHODS([]) PROPERTIES([vcPublic])}
  TC = class
  public
    F: LongInt;
    procedure M;
    property P: LongInt read F;
  end;
{$RTTI EXPLICIT METHODS([]) FIELDS([vcPublic]) PROPERTIES([vcPublic])}
  TD = class
  public
    F: LongInt;
    procedure M;
    property P: LongInt read F;
  end;
{$RTTI EXPLICIT FIELDS([vcPublic]) PROPERTIES([vcPublic]) METHODS([])}
  TE = class
  public
    F: LongInt;
    procedure M;
    property P: LongInt read F;
  end;
{$RTTI EXPLICIT PROPERTIES([vcPublic]) METHODS([]) FIELDS([vcPublic])}
  TF = class
  public
    F: LongInt;
    procedure M;
    property P: LongInt read F;
  end;
{$RTTI EXPLICIT FIELDS([]) METHODS([vcPublic]) PROPERTIES([])}
  TG = class
  public
    F: LongInt;
    procedure M;
    property P: LongInt read F;
  end;
{$RTTI EXPLICIT FIELDS([])}
  TH = class
  public
    F: LongInt;
    procedure M;
    property P: LongInt read F;
  end;

procedure TA.M; begin end;
procedure TB.M; begin end;
procedure TC.M; begin end;
procedure TD.M; begin end;
procedure TE.M; begin end;
procedure TF.M; begin end;
procedure TG.M; begin end;
procedure TH.M; begin end;

var
  gErrors: LongInt = 0;

procedure Check(const aWhat: string; aClass: TClass; aFields, aMethods, aProps: LongInt);
var
  lFields: PExtendedFieldInfoTable;
  lMethods: PExtendedMethodInfoTable;
  lProps: PPropListEx;
  lF, lM, lP: LongInt;
begin
  lF := GetFieldList(aClass, lFields, [vcPublic], False);
  lM := GetMethodList(aClass.ClassInfo, lMethods, [vcPublic], False);
  lP := GetPropListEx(aClass, lProps, [vcPublic]);
  if lF > 0 then FreeMem(lFields);
  if lM > 0 then FreeMem(lMethods);
  if lP > 0 then FreeMem(lProps);
  writeln(aWhat, ': fields ', lF, ', methods ', lM, ', properties ', lP);
  if (lF <> aFields) or (lM <> aMethods) or (lP <> aProps) then
    begin
      writeln('  expected fields ', aFields, ', methods ', aMethods, ', properties ', aProps);
      Inc(gErrors);
    end;
end;

begin
  Check('METHODS([]) PROPERTIES FIELDS', TA, 1, 0, 1);
  Check('PROPERTIES FIELDS METHODS([])', TB, 1, 0, 1);
  Check('FIELDS METHODS([]) PROPERTIES', TC, 1, 0, 1);
  Check('METHODS([]) FIELDS PROPERTIES', TD, 1, 0, 1);
  Check('FIELDS PROPERTIES METHODS([])', TE, 1, 0, 1);
  Check('PROPERTIES METHODS([]) FIELDS', TF, 1, 0, 1);
  Check('FIELDS([]) METHODS PROPERTIES([])', TG, 0, 1, 0);
  Check('FIELDS([]) alone', TH, 0, 0, 0);
  if gErrors <> 0 then
    Halt(1);
  writeln('ok');
end.
