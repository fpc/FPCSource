{ %skiptarget=qnx,wdosx,palmos,macosclassic,embedded,zxspectrum,msxdos,amstradcpc,ps1 }
{ %skipcpu=jvm }
{ Negation and "not" of a by-reference Variant whose reference is nil
  raise EVariantBadVarTypeError instead of reading through nil, both
  when the variant manager's VarNeg and VarNot are called directly and
  through the operators; a by-reference Variant with a reference is
  still negated and inverted. }
{$mode objfpc}{$h+}

uses
  SysUtils, Variants;

var
  failures : longint;
  manager : TVariantManager;

procedure Fail(const what: string);
begin
  writeln('FAILED: ', what);
  inc(failures);
end;

procedure CheckNil(t: TVarType; direct, donot: boolean);
var
  v, r : Variant;
  name : string;
begin
  name := '$' + hexstr(varByRef or t, 4);
  if donot then
    name := 'not ' + name
  else
    name := '-' + name;
  if direct then
    name := name + ' (direct)';
  TVarData(v).vType := varByRef or t;
  TVarData(v).vPointer := nil;
  try
    try
      if direct then
        begin
          if donot then
            manager.VarNot(v)
          else
            manager.VarNeg(v);
        end
      else if donot then
        r := not v
      else
        r := -v;
      Fail(name + ': no exception');
    except
      on e: EVariantBadVarTypeError do
        ;
      on e: Exception do
        Fail(name + ': ' + e.ClassName + ': ' + e.Message);
    end;
  finally
    TVarData(v).vType := varEmpty;
  end;
end;

procedure CheckType(t: TVarType);
begin
  CheckNil(t, true, false);
  CheckNil(t, true, true);
  CheckNil(t, false, false);
  CheckNil(t, false, true);
end;

var
  target, inner, v, r : Variant;
  value : longint;
  flag : wordbool;
begin
  failures := 0;
  GetVariantManager(manager);
  CheckType(varSmallint);
  CheckType(varInteger);
{$ifndef FPUNONE}
  CheckType(varSingle);
  CheckType(varDouble);
  CheckType(varDate);
{$endif}
  CheckType(varCurrency);
  CheckType(varOleStr);
  CheckType(varBoolean);
  CheckType(varVariant);
  CheckType(varShortInt);
  CheckType(varByte);
  CheckType(varWord);
  CheckType(varLongWord);
  CheckType(varInt64);
  CheckType(varQWord);
  CheckType(varString);
  CheckType(varUString);

  value := 5;
  TVarData(v).vType := varByRef or varInteger;
  TVarData(v).vPointer := @value;
  r := -v;
  if (VarType(r) <> varInteger) or (r <> -5) then
    Fail('-ref integer 5');
  r := not v;
  if (VarType(r) <> varInteger) or (r <> not longint(5)) then
    Fail('not ref integer 5');
  value := 0;
  r := -v;
  if (VarType(r) <> varInteger) or (r <> 0) then
    Fail('-ref integer 0');
  r := not v;
  if (VarType(r) <> varInteger) or (r <> -1) then
    Fail('not ref integer 0');
  TVarData(v).vType := varEmpty;

  flag := false;
  TVarData(v).vType := varByRef or varBoolean;
  TVarData(v).vPointer := @flag;
  r := -v;
  if (VarType(r) <> varSmallint) or (r <> 0) then
    Fail('-ref boolean false');
  r := not v;
  if (VarType(r) <> varBoolean) or not boolean(r) then
    Fail('not ref boolean false');
  flag := true;
  r := -v;
  if (VarType(r) <> varSmallint) or (r <> -1) then
    Fail('-ref boolean true');
  r := not v;
  if (VarType(r) <> varBoolean) or boolean(r) then
    Fail('not ref boolean true');
  TVarData(v).vType := varEmpty;

  inner := 3;
  TVarData(target).vType := varByRef or varVariant;
  TVarData(target).vPointer := @TVarData(inner);
  r := -target;
  if r <> -3 then
    Fail('-ref variant');
  r := not target;
  if r <> not longint(3) then
    Fail('not ref variant');
  TVarData(target).vType := varEmpty;

  if failures <> 0 then
    halt(1);
  writeln('ok');
end.
