program tinitvar;

{$namespace org.freepascal.test.tinitvar}

type
  tenum = (ta,tb,tc);

procedure varpara(
var by: byte;
var si: shortint;
var mi: smallint;
var wo: word;
var li: longint;
var ca: cardinal;
var i6: int64;
var qw: qword;
var e: tenum;
var sg: single;
var db: double;
var c: ansichar;
var wc: widechar);
begin
end;

procedure test;
var
  by: byte;
  si: shortint;
  mi: smallint;
  wo: word;
  li: longint;
  ca: cardinal;
  i6: int64;
  qw: qword;
  e: tenum;
  sg: single;
  db: double;
  c: ansichar;
  wc: widechar;
begin
  varpara(
  by,
  si,
  mi,
  wo,
  li,
  ca,
  i6,
  qw,
  e,
  sg,
  db,
  c,
  wc
  );
end;

begin
  test;
end.