{ SetPropValue with string and integer values for Char and boolean properties, issue #28290 }
program tsetpropvalue;

{$mode objfpc}{$h+}

uses
  SysUtils, TypInfo, Variants;

type
  {$M+}
  TPropHolder = class
  private
    FBool: Boolean;
    FByteBool: ByteBool;
    FWordBool: WordBool;
    FLongBool: LongBool;
    FBool64: Boolean64;
    FQWordBool: QWordBool;
    FChar: AnsiChar;
    FWideChar: WideChar;
  published
    // Boolean property
    property BoolProp: Boolean read FBool write FBool;
    // ByteBool property
    property ByteBoolProp: ByteBool read FByteBool write FByteBool;
    // WordBool property
    property WordBoolProp: WordBool read FWordBool write FWordBool;
    // LongBool property
    property LongBoolProp: LongBool read FLongBool write FLongBool;
    // Boolean64 property
    property Bool64Prop: Boolean64 read FBool64 write FBool64;
    // QWordBool property
    property QWordBoolProp: QWordBool read FQWordBool write FQWordBool;
    // AnsiChar property
    property CharProp: AnsiChar read FChar write FChar;
    // WideChar property
    property WideCharProp: WideChar read FWideChar write FWideChar;
  end;
  {$M-}

var
  lHolder: TPropHolder;
  lRaised: Boolean;

begin
  lHolder:=TPropHolder.Create;
  try
    SetPropValue(lHolder,'CharProp',AnsiString('a'));
    if lHolder.CharProp<>'a' then
      halt(1);
    SetPropValue(lHolder,'CharProp',UnicodeString('b'));
    if lHolder.CharProp<>'b' then
      halt(2);
    SetPropValue(lHolder,'CharProp','');
    if lHolder.CharProp<>#0 then
      halt(3);
    SetPropValue(lHolder,'CharProp',65);
    if lHolder.CharProp<>'A' then
      halt(4);

    SetPropValue(lHolder,'WideCharProp',UnicodeString(#$0416));
    if lHolder.WideCharProp<>#$0416 then
      halt(11);
    SetPropValue(lHolder,'WideCharProp',AnsiString('c'));
    if lHolder.WideCharProp<>'c' then
      halt(12);
    SetPropValue(lHolder,'WideCharProp',$0417);
    if lHolder.WideCharProp<>#$0417 then
      halt(13);

    SetPropValue(lHolder,'BoolProp','True');
    if not lHolder.BoolProp then
      halt(21);
    SetPropValue(lHolder,'BoolProp',UnicodeString('False'));
    if lHolder.BoolProp then
      halt(22);
    SetPropValue(lHolder,'BoolProp',1);
    if not lHolder.BoolProp then
      halt(23);
    lRaised:=False;
    try
      SetPropValue(lHolder,'BoolProp',2);
    except
      on ERangeError do
        lRaised:=True;
    end;
    if not lRaised then
      halt(24);

    SetPropValue(lHolder,'ByteBoolProp',1);
    if not lHolder.ByteBoolProp then
      halt(31);
    SetPropValue(lHolder,'ByteBoolProp',0);
    if lHolder.ByteBoolProp then
      halt(32);
    SetPropValue(lHolder,'ByteBoolProp',256);
    if not lHolder.ByteBoolProp then
      halt(33);
    SetPropValue(lHolder,'WordBoolProp',7);
    if not lHolder.WordBoolProp then
      halt(34);
    SetPropValue(lHolder,'LongBoolProp',5);
    if LongInt(lHolder.LongBoolProp)<>-1 then
      halt(35);
    SetPropValue(lHolder,'LongBoolProp','False');
    if lHolder.LongBoolProp then
      halt(36);
    SetPropValue(lHolder,'LongBoolProp','True');
    if not lHolder.LongBoolProp then
      halt(37);

    SetPropValue(lHolder,'Bool64Prop',1);
    if Int64(lHolder.Bool64Prop)<>1 then
      halt(41);
    lRaised:=False;
    try
      SetPropValue(lHolder,'Bool64Prop',2);
    except
      on ERangeError do
        lRaised:=True;
    end;
    if not lRaised then
      halt(42);
    SetPropValue(lHolder,'QWordBoolProp',3);
    if not lHolder.QWordBoolProp then
      halt(43);
    SetPropValue(lHolder,'QWordBoolProp',0);
    if lHolder.QWordBoolProp then
      halt(44);
  finally
    lHolder.Free;
  end;
  writeln('ok');
end.
