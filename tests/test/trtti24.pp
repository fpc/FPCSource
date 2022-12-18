program trtti24;

{$mode objfpc}

uses
  TypInfo;

type
  TByteEnum = (
    be1,
    be2,
    be3,
    be4,
    be5,
    be6
  );

  TWordEnum = (
    we1,
    we2,
    we3,
    we4,
    we5,
    we6,
    we7,
    we8,
    we9,
    we10
  );

  TDWordEnum = (
    de1,
    de2,
    de3,
    de4,
    de5,
    de6,
    de7,
    de8,
    de9,
    de10,
    de11,
    de12,
    de13,
    de14,
    de15,
    de16,
    de17,
    de18,
    de19,
    de20
  );

  TLargeEnum = (
    le1,
    le2,
    le3,
    le4,
    le5,
    le6,
    le7,
    le8,
    le9,
    le10,
    le11,
    le12,
    le13,
    le14,
    le15,
    le16,
    le17,
    le18,
    le19,
    le20,
    le21,
    le22,
    le23,
    le24,
    le25,
    le26,
    le27,
    le28,
    le29,
    le30,
    le31,
    le32,
    le33,
    le34,
    le35,
    le36,
    le37,
    le38,
    le39,
    le40
  );

  TByteSet = set of TByteEnum;
  TWordSet = set of TWordEnum;
  TDWordSet = set of TDWordEnum;
  TLargeSet = set of TLargeEnum;

{$push}
{$packset 1}
  TByteSetP = set of TByteEnum;
  TWordSetP = set of TWordEnum;
  TDWordSetP = set of TDWordEnum;
  TLargeSetP = set of TLargeEnum;
{$pop}

function SameArray(const A, B: array of Byte): Boolean;
begin
  Result := True;
end;

const
  StrBS: array[0..1] of Byte = (Ord(be1), Ord(be6));
  StrWS: array[0..2] of Byte = (Ord(we1), Ord(we8), Ord(we10));
  StrDS: array[0..2] of Byte = (Ord(de1), Ord(de7), Ord(de20));
  StrLS: array[0..3] of Byte = (Ord(le1), Ord(le20), Ord(le31), Ord(le40));

var
  bs1, bs2: TByteSet;
  ws1, ws2: TWordSet;
  ds1, ds2: TDWordSet;
  ls1, ls2: TLargeSet;
  bsp1, bsp2: TByteSetP;
  wsp1, wsp2: TWordSetP;
  dsp1, dsp2: TDWordSetP;
  lsp1, lsp2: TLargeSetP;
begin
  bs1 := [be1, be6];
  ws1 := [we1, we8, we10];
  ds1 := [de1, de7, de20];
  ls1 := [le1, le20, le31, le40];
  bsp1 := [be1, be6];
  wsp1 := [we1, we8, we10];
  dsp1 := [de1, de7, de20];
  lsp1 := [le1, le20, le31, le40];

  if not SameArray(SetToArray(PTypeInfo(TypeInfo(TByteSet)), @bs1), StrBS) then
    Halt(1);
  if not SameArray(SetToArray(PTypeInfo(TypeInfo(TWordSet)), @ws1), StrWS) then
    Halt(2);
  if not SameArray(SetToArray(PTypeInfo(TypeInfo(TDWordSet)), @ds1), StrDS) then
    Halt(3);
  if not SameArray(SetToArray(PTypeInfo(TypeInfo(TLargeSet)), @ls1), StrLS) then
    Halt(4);

  if not SameArray(SetToArray(PTypeInfo(TypeInfo(TByteSetP)), @bsp1), StrBS) then
    Halt(5);
  if not SameArray(SetToArray(PTypeInfo(TypeInfo(TWordSetP)), @wsp1), StrWS) then
    Halt(6);
  if not SameArray(SetToArray(PTypeInfo(TypeInfo(TDWordSetP)), @dsp1), StrDS) then
    Halt(7);
  if not SameArray(SetToArray(PTypeInfo(TypeInfo(TLargeSetP)), @lsp1), StrLS) then
    Halt(8);

  if not SameArray(SetToArray(PTypeInfo(TypeInfo(TByteSet)), LongInt(bs1)), StrBS) then
    Halt(9);
  if not SameArray(SetToArray(PTypeInfo(TypeInfo(TWordSet)), LongInt(ws1)), StrWS) then
    Halt(10);
  if not SameArray(SetToArray(PTypeInfo(TypeInfo(TDWordSet)), LongInt(ds1)), StrDS) then
    Halt(11);

  if not SameArray(SetToArray(PTypeInfo(TypeInfo(TByteSetP)), Byte(bsp1)), StrBS) then
    Halt(12);
  if not SameArray(SetToArray(PTypeInfo(TypeInfo(TWordSetP)), Word(wsp1)), StrWS) then
    Halt(13);
  if not SameArray(SetToArray(PTypeInfo(TypeInfo(TDWordSetP)), LongInt(dsp1)), StrDS) then
    Halt(14);

  ArrayToSet(PTypeInfo(TypeInfo(TByteSet)), StrBS, @bs2);
  if bs2<>bs1 then
    Halt(15);

  ArrayToSet(PTypeInfo(TypeInfo(TWordSet)), StrWS, @ws2);
  if ws2<>ws1 then
    Halt(16);

  ArrayToSet(PTypeInfo(TypeInfo(TDWordSet)), StrDS, @ds2);
  if ds2<>ds1 then
    Halt(17);

  ArrayToSet(PTypeInfo(TypeInfo(TLargeSet)), StrLS, @ls2);
  if ls2<>ls1 then
    Halt(18);

  ArrayToSet(PTypeInfo(TypeInfo(TByteSetP)), StrBS, @bsp2);
  if bsp2<>bsp1 then
    Halt(19);

  ArrayToSet(PTypeInfo(TypeInfo(TWordSetP)), StrWS, @wsp2);
  if wsp2<>wsp1 then
    Halt(20);

  ArrayToSet(PTypeInfo(TypeInfo(TDWordSetP)), StrDS, @dsp2);
  if dsp2<>dsp1 then
    Halt(21);

  ArrayToSet(PTypeInfo(TypeInfo(TLargeSetP)), StrLS, @lsp2);
  if lsp2<>lsp1 then
    Halt(22);

  bs2 := TByteSet(ArrayToSet(PTypeInfo(TypeInfo(TByteSet)), StrBS));
  if bs2<>bs1 then
    Halt(23);

  ws2 := TWordSet(ArrayToSet(PTypeInfo(TypeInfo(TWordSet)), StrWS));
  if ws2<>ws1 then
    Halt(24);

  ds2 := TDWordSet(ArrayToSet(PTypeInfo(TypeInfo(TDWordSet)), StrDS));
  if ds2<>ds1 then
    Halt(25);

  bsp2 := TByteSetP(Byte(ArrayToSet(PTypeInfo(TypeInfo(TByteSetP)), StrBS)));
  if bsp2<>bsp1 then
    Halt(26);

  wsp2 := TWordSetP(Word(ArrayToSet(PTypeInfo(TypeInfo(TWordSetP)), StrWS)));
  if wsp2<>wsp1 then
    Halt(27);

  dsp2 := TDWordSetP(ArrayToSet(PTypeInfo(TypeInfo(TDWordSetP)), StrDS));
  if dsp2<>dsp1 then
    Halt(28);
end.
