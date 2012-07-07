program tconst;

{$mode delphi}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

type
  tc = class
    const x: longint = 5;
  end;

  ttypedconstrec = record
    l: longint;
  end;

procedure test; overload;
const
  l: longint = 1;
  r: ttypedconstrec = (l: 5);
begin
  if r.l<>5 then
    raise jlexception.create('test1 r.l');
  if l<>1 then
    raise jlexception.create('test1 l');
end;

procedure test(x: byte); overload;
const
  { check that it gets a different mangled name }
  l: longint = 4;
begin
  if l<>4 then
    raise jlexception.create('test1 l');
end;

begin
  test;
  test(3);
end.
