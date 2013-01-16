type
  TChomskyType = ( ctNoneGrammar = 0,
                   ctType0 = 1, ctRecursivelyEnumerable = ctType0,
                   ctType1 = 2, ctContextSensitive = ctType1,
                   ctType2 = 3, ctContextFree = ctType2,
                   ctType3 = 4, ctRegular = ctType3);

var
  e: TChomskyType;
  err: longint;
  s: shortstring;
begin
  val('ctType0',e,err);
  if e<>ctType0 then
    halt(1);

  val('ctRecursivelyEnumerable',e,err);
  if e<>ctRecursivelyEnumerable then
    halt(2);

  val('ctType1',e,err);
  if e<>ctType1 then
    halt(3);

  val('ctContextSensitive',e,err);
  if e<>ctContextSensitive then
    halt(4);

  val('ctType2',e,err);
  if e<>ctType2 then
    halt(5);

  val('ctContextFree',e,err);
  if e<>ctContextFree then
    halt(6);

  str(ctType0,s);
  { could be either since they have the same value }
  if (s<>'ctType0') and
     (s<>'ctRecursivelyEnumerable') then
    halt(7);

  str(ctRecursivelyEnumerable,s);
  if (s<>'ctType0') and
     (s<>'ctRecursivelyEnumerable') then
    halt(8);

  str(ctType1,s);
  if (s<>'ctType1') and
     (s<>'ctContextSensitive') then
    halt(9);

  str(ctContextSensitive,s);
  if (s<>'ctType1') and
     (s<>'ctContextSensitive') then
    halt(9);

  str(ctType2,s);
  if (s<>'ctType2') and
     (s<>'ctContextFree') then
    halt(10);

  str(ctContextFree,s);
  if (s<>'ctType2') and
     (s<>'ctContextFree') then
    halt(10);
end.
