{ "type of" as a generic parameter: named result types work directly,
  others via a named type }
program ttypeinquiry28;

{$mode objfpc}

type
  generic TGen<T> = record
    v: T;
  end;

procedure kek;
begin
end;

type
  TKek = type of @kek;
  TKekGen = specialize TGen<TKek>;

var
  w: word;
  WordGen: specialize TGen<type of w>;
  KekGen: TKekGen;
begin
  if SizeOf(WordGen.v)<>SizeOf(word) then
    halt(1);
  KekGen.v:=@kek;
  if not Assigned(KekGen.v) then
    halt(2);
  w:=0;
  if w<>0 then
    halt(3);
end.
