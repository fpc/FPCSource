program tcustomattr26;

{ attributes on the parameters of a method are in the parameter's RTTI, as
  Delphi lists them: before or after const, var or out, several in a list or
  in several lists, on the first name of a list of names, and those of the
  declaration rather than of the implementation's header }

{$mode delphi}
{$modeswitch prefixedattributes}

uses
  typinfo;

type
  NameAttribute = class(TCustomAttribute)
  public
    FName: string;
    constructor Create(const aName: string);
  end;

  FlagAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

  {$M+}
  IThing = interface
    ['{5D6F4F0E-2A4B-4A26-8E0F-3F0D6C3A1B11}']
    procedure Plain([Name('p')] aValue: Integer);
    procedure Listed([Flag, Name('q')] aValue: Integer);
    procedure Lists([Flag] [Name('r')] aValue: Integer);
    procedure AfterConst(const [Name('s')] aValue: string);
    procedure BeforeConst([Name('t')] const aValue: string);
    procedure AfterVar(var [Name('u')] aValue: Integer);
    procedure BeforeOut([Name('v')] out aValue: Integer);
    procedure Shared([Name('w')] a, b: Integer);
    procedure Mixed([Name('x')] a: Integer; b: Integer; [Flag] c: Integer);
    function Func([Name('y')] a: Integer): Integer;
    procedure RefToo(const [Ref, Name('z')] a: Integer);
    procedure Dflt([Name('d')] a: Integer = 5);
  end;
  {$M-}

  TProcType = procedure([Name('pt')] a: Integer);
  TMethType = procedure([Name('mt')] a: Integer) of object;

  TThing = class
  public
    procedure InImpl([Name('decl')] a: Integer);
    procedure ImplOnly(a: Integer);
  end;

constructor NameAttribute.Create(const aName: string);
begin
  inherited Create;
  FName := aName;
end;

constructor FlagAttribute.Create;
begin
  inherited Create;
end;

procedure Free1([Name('free')] a: Integer);
begin
end;

procedure TThing.InImpl([Name('impl')] a: Integer);
begin
end;

procedure TThing.ImplOnly([Name('only')] a: Integer);
begin
end;

function Listing(aParam: PVmtMethodParam): string;
var
  lIdx: Integer;
  lAttr: TCustomAttribute;
begin
  Result := '';
  if not Assigned(aParam^.AttributeTable) then
    Exit;
  for lIdx := 0 to aParam^.AttributeTable^.AttributeCount - 1 do begin
    lAttr := GetAttribute(aParam^.AttributeTable, lIdx);
    if lAttr is NameAttribute then
      Result := Result + 'N' + NameAttribute(lAttr).FName + ' '
    else
      Result := Result + 'F ';
    lAttr.Free;
  end;
end;

var
  gMethods: PIntfMethodTable;
  gErrors: Integer = 0;

procedure Check(aMethod, aParam: Integer; const aExpected: string);
var
  lEntry: PIntfMethodEntry;
  lParam: PVmtMethodParam;
  lGot: string;
begin
  lEntry := gMethods^.Method[aMethod];
  lParam := lEntry^.Param[aParam];
  lGot := Listing(lParam);
  if lGot <> aExpected then begin
    Writeln('error: method ', lEntry^.Name, ' param ', lParam^.Name, ': "', lGot, '", expected "', aExpected, '"');
    Inc(gErrors);
  end;
end;

{ parameter 0 of an interface method is its Self }
begin
  gMethods := PInterfaceData(GetTypeData(TypeInfo(IThing)))^.MethodTable;
  if gMethods^.Count <> 12 then
    Halt(1);
  Check(0, 0, '');
  Check(0, 1, 'Np ');
  Check(1, 1, 'F Nq ');
  Check(2, 1, 'F Nr ');
  Check(3, 1, 'Ns ');
  Check(4, 1, 'Nt ');
  Check(5, 1, 'Nu ');
  Check(6, 1, 'Nv ');
  Check(7, 1, 'Nw ');
  Check(7, 2, '');
  Check(8, 1, 'Nx ');
  Check(8, 2, '');
  Check(8, 3, 'F ');
  Check(9, 1, 'Ny ');
  Check(10, 1, 'Nz ');
  Check(11, 1, 'Nd ');
  if gErrors <> 0 then
    Halt(2);
  Writeln('ok');
end.
