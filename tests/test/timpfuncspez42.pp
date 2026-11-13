program timpfuncspez42;

{$mode delphi}
{$modeswitch implicitfunctionspecialization}
{$T+}

{ With typed addresses @lInt has a pointer type shared by every such address.
  The first call specializes P<T> for it, but takes the non-generic overload,
  since the specialization does not accept the second argument: the symbol made
  for the anonymous type is freed then and the shared type must not keep it for
  the later calls. }

type
  TH = class
    class function P(a: Pointer; const b: String): String; overload;
    class function P<T>(a: T; b: LongInt): String; overload;
  end;

class function TH.P(a: Pointer; const b: String): String;
begin
  Result := 'plain';
end;

class function TH.P<T>(a: T; b: LongInt): String;
begin
  Result := 'generic';
end;

var
  lInt: LongInt;
  lWord: Word;
begin
  if TH.P(@lInt, 'x') <> 'plain' then
    Halt(1);
  if TH.P(@lWord, 'x') <> 'plain' then
    Halt(2);
  if TH.P(@lInt, 1) <> 'generic' then
    Halt(3);
  if TH.P(@lWord, 2) <> 'generic' then
    Halt(4);
  Writeln('ok');
end.
