{ %cpu=powerpc,powerpc64,mips,mipsel,mips64,mips64el }
{ fixup_jmps must also fix a conditional branch whose target label has the
  same number as a label of another type that precedes it (e.g. the CFI label
  at the procedure entry): the setjmp check of the implicit try/finally in Big
  jumps over more than 32 KiB of code on PowerPC }
{$mode objfpc}{$H+}
{$macro on}
program tfixupjmps;

uses
  sysutils;

type
  TIni = class
    Count, FailAt: longint;
    procedure WriteString(const S, I, V: string); virtual;
  end;

  TEdit = class
    FText: string;
    function GetText: string; virtual;
    property Text: string read GetText;
  end;

{ Big comes first, so that its jump labels and the CFI labels at its entry
  get the same numbers }
{$define C1:=Ini.WriteString('Sec', 'Key', E.Text);}
{$define C10:=C1 C1 C1 C1 C1 C1 C1 C1 C1 C1}
{$define C100:=C10 C10 C10 C10 C10 C10 C10 C10 C10 C10}

procedure Big(Ini: TIni; E: TEdit);
begin
  C100 C100 C100 C100 C100
end;

procedure TIni.WriteString(const S, I, V: string);
begin
  inc(Count);
  if Count = FailAt then
    raise Exception.Create('boom');
end;

function TEdit.GetText: string;
begin
  Result := FText;
end;

var
  Ini: TIni;
  E: TEdit;
begin
  Ini := TIni.Create;
  E := TEdit.Create;
  E.FText := 'x' + IntToStr(42);
  Ini.FailAt := -1;
  Big(Ini, E);
  if (Ini.Count <> 500) or (StringRefCount(E.FText) <> 1) then
    halt(1);
  Ini.Count := 0;
  Ini.FailAt := 300;
  try
    Big(Ini, E);
    halt(2);
  except
    on Exception do ;
  end;
  if (Ini.Count <> 300) or (StringRefCount(E.FText) <> 1) then
    halt(3);
  E.Free;
  Ini.Free;
  writeln('ok');
end.
