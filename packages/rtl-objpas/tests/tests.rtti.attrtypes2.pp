unit tests.rtti.attrtypes2;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  Rtti
  {$ifndef Windows},
  ffi.manager
  {$endif};

{$RTTI EXPLICIT
  FIELDS([vcPublic])
  PROPERTIES([vcPublic,vcPublished])
  METHODS([vcPublic,vcPublished])}

var
  ErrorCount: Integer;

type
  TTestAttr2Record = record
    fa:integer;
    fa2:integer;
    fa3:integer;
  public
    function Offset(arg1, arg2: Integer): Integer;
    property TestIProp[i1, i2: Integer]: Integer read Offset;
    constructor Create(a1, a2: Integer); overload;
    constructor Create(rec: TTestAttr2Record); overload;
    class function StaticFunc(d: Double; p: TPoint; r: TRect): string; static;
  end;

  TTestAttr2Class = class
  private
    class var
      static_var: Integer;
    class function GetStaticProp: Integer; static;
    class procedure SetStaticProp(value: Integer); static;

    function GetIndProp(arg1, arg2: Integer): TObject;
    procedure SetIndProp(arg1, arg2: Integer; value: TObject);
  public
    fa, fa2:integer;
    property TestIProp[i: Integer; i2: Integer]: TObject read GetIndProp write SetIndProp;
    class property StaticProp: Integer read GetStaticProp write SetStaticProp;
    procedure MethodForNil(arg1, arg2: TObject);
    class function StaticMethod(str: string): Integer; static;
    constructor Create(a1, a2: Integer);
    class procedure ClassProc(var int: Integer; var str: string);
  end;

  TInherited2Class = class(TTestAttr2Class)
  end;


implementation

uses fpcunit;

procedure Check(ACondition: boolean; const AMessage: string);
begin
  TAssert.AssertTrue(AMessage,ACondition);
end;


function TTestAttr2Record.Offset(arg1, arg2: Integer): Integer;
begin
  fa := fa + arg1;
  fa2 := fa2 + arg2;
  Result := fa + fa2;
end;

constructor TTestAttr2Record.Create(a1, a2: Integer);
begin
  Check((fa = 60) and (fa2 = 80) and (fa3 = 90), 'Original TTestAttr2Record was delivered incorrectly');
  fa := a1;
  fa2 := a2;
end;

constructor TTestAttr2Record.Create(rec: TTestAttr2Record);
begin
  fa := rec.fa;
  fa2 := rec.fa2;
end;

class function TTestAttr2Record.StaticFunc(d: Double; p: TPoint; r: TRect): string;
begin
  Result := 'experiment_'+d.ToString+'_'+p.X.ToString+'_'+p.Y.ToString+'_'+r.Left.ToString+'_'+r.Top.ToString+'_'+r.Right.ToString+'_'+r.Bottom.ToString;
end;

class function TTestAttr2Class.GetStaticProp: Integer;
begin
  Result := static_var;
end;

class procedure TTestAttr2Class.SetStaticProp(value: Integer);
begin
  static_var := Value;
end;

function TTestAttr2Class.GetIndProp(arg1, arg2: Integer): TObject;
begin
  fa := arg1;
  fa2 := arg2;
  Result := Self;
end;

procedure TTestAttr2Class.SetIndProp(arg1, arg2: Integer; value: TObject);
begin
  fa := arg1;
  fa2 := arg2;
  Check((arg1 = 653) and (arg2 = 796) and ((value as TTestAttr2Class).fa2 = 796),
    'The setter of an indexed property is incorrectly called');
end;

procedure TTestAttr2Class.MethodForNil(arg1, arg2: TObject);
begin
  Check((arg1 = nil) and (arg2 = nil), 'MethodForNil did not get only nil');
end;

class function TTestAttr2Class.StaticMethod(str: string): Integer;
begin
  Check(str = 'simple string', 'The static method argument is incorrect');
  Result := 7775;
end;

class procedure TTestAttr2Class.ClassProc(var int: Integer; var str: string);
begin
  Check(Self.ClassName = 'TInherited2Class', 'Incorrect class transfer to Self');
  Inc(int, 12);
  str := str + '_addon';
end;

constructor TTestAttr2Class.Create(a1, a2: Integer);
begin
  fa:=a1;
  fa2:=a2;
end;

end.

