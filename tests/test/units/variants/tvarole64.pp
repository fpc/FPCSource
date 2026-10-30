{ %skiptarget=qnx,wdosx,palmos,macosclassic,embedded,zxspectrum,msxdos,amstradcpc,ps1 }
{ %skipcpu=jvm }
{ assigning an olevariant to an int64 or a qword converts as assigning a
  variant does: a decimal integer string in range gives its value, one out of
  range raises EVariantError, and a custom variant type is asked for varInt64
  or varQWord }
program tvarole64;

{$mode objfpc}{$H+}

uses
  sysutils, variants;

type
  TRecordingType = class(TCustomVariantType)
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
  end;

var
  errors: longint = 0;
  asked: TVarType;
  recording: TRecordingType;

procedure TRecordingType.Clear(var V: TVarData);
begin
  V.vType := varEmpty;
end;

procedure TRecordingType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  Dest := Source;
end;

procedure TRecordingType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
begin
  asked := AVarType;
  Dest.vType := AVarType;
  Dest.vInt64 := 7;
end;

procedure check(const what, got, want: string);
begin
  if got = want then
    writeln('ok   ', what, ' = ', got)
  else
    begin
      writeln('FAIL ', what, ' = ', got, ', expected ', want);
      inc(errors);
    end;
end;

function viaInt64(const v: variant): string;
var
  i: int64;
begin
  try
    i := v;
    result := IntToStr(i);
  except
    on e: Exception do
      result := e.ClassName;
  end;
end;

function viaOleInt64(const v: olevariant): string;
var
  i: int64;
begin
  try
    i := v;
    result := IntToStr(i);
  except
    on e: Exception do
      result := e.ClassName;
  end;
end;

function viaQWord(const v: variant): string;
var
  q: qword;
begin
  try
    q := v;
    result := IntToStr(q);
  except
    on e: Exception do
      result := e.ClassName;
  end;
end;

function viaOleQWord(const v: olevariant): string;
var
  q: qword;
begin
  try
    q := v;
    result := IntToStr(q);
  except
    on e: Exception do
      result := e.ClassName;
  end;
end;

{ both operators must give the wanted value, or raise the wanted class }
procedure checkString(const s, wantInt64, wantQWord: string);
begin
  check('int64 := olevariant(''' + s + ''')', viaOleInt64(s), wantInt64);
  check('int64 := variant(''' + s + ''')', viaInt64(s), wantInt64);
  check('qword := olevariant(''' + s + ''')', viaOleQWord(s), wantQWord);
  check('qword := variant(''' + s + ''')', viaQWord(s), wantQWord);
end;

function askedFor(const v: variant; toQWord: boolean): string;
var
  i: int64;
  q: qword;
begin
  asked := varEmpty;
  if toQWord then
    q := v
  else
    i := v;
  result := IntToStr(asked);
end;

function askedForOle(const v: olevariant; toQWord: boolean): string;
var
  i: int64;
  q: qword;
begin
  asked := varEmpty;
  if toQWord then
    q := v
  else
    i := v;
  result := IntToStr(asked);
end;

var
  custom: variant;
  customOle: olevariant;
begin
  checkString('-9223372036854775809', 'EVariantError', 'EVariantError');
  checkString('-9223372036854775808', '-9223372036854775808', 'EVariantError');
  checkString('-9000000000', '-9000000000', 'EVariantError');
  checkString('-1', '-1', 'EVariantError');
  checkString('0', '0', '0');
  checkString('9223372036854775807', '9223372036854775807', '9223372036854775807');
  checkString('9223372036854775808', 'EVariantError', '9223372036854775808');
  checkString('18446744073709551615', 'EVariantError', '18446744073709551615');
  checkString('18446744073709551616', 'EVariantError', 'EVariantError');

  recording := TRecordingType.Create;
  { both made by hand, so that no conversion asks the type for anything }
  TVarData(custom).vType := recording.VarType;
  TVarData(custom).vInt64 := 7;
  TVarData(customOle).vType := recording.VarType;
  TVarData(customOle).vInt64 := 7;
  check('custom to int64 via variant asks', askedFor(custom, false), IntToStr(varInt64));
  check('custom to qword via variant asks', askedFor(custom, true), IntToStr(varQWord));
  check('custom to int64 via olevariant asks', askedForOle(customOle, false), IntToStr(varInt64));
  check('custom to qword via olevariant asks', askedForOle(customOle, true), IntToStr(varQWord));
  custom := Unassigned;
  customOle := Unassigned;
  recording.Free;

  if errors <> 0 then
    begin
      writeln(errors, ' failure(s)');
      halt(1);
    end;
  writeln('ok');
end.
