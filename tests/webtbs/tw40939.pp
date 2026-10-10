program tw40939;

{$mode objfpc}{$h+}

uses
  SysUtils;

type
  TGuardedFloatRec = packed record
    Rec: TFloatRec;
    Guard: array[0..3] of Byte;
  end;

  TCurrCase = record
    Value: Currency;
    Fmt: string;
    Expected: string;
  end;

const
  cCases: array[0..11] of TCurrCase = (
    (Value: 922337203685477.5807; Fmt: '0.0000'; Expected: '922337203685477.5807'),
    (Value: -922337203685477.5808; Fmt: '0.0000'; Expected: '-922337203685477.5808'),
    (Value: -922337203685477.5807; Fmt: '0.0000'; Expected: '-922337203685477.5807'),
    (Value: 922337203685477.5806; Fmt: '0.0000'; Expected: '922337203685477.5806'),
    (Value: 922337203685477.5807; Fmt: '#,##0.0000'; Expected: '922,337,203,685,477.5807'),
    (Value: -922337203685477.5808; Fmt: '#,##0.0000'; Expected: '-922,337,203,685,477.5808'),
    (Value: 922337203685477.5807; Fmt: '0.####'; Expected: '922337203685477.5807'),
    (Value: 922337203685477.5807; Fmt: '0.00'; Expected: '922337203685477.58'),
    (Value: -922337203685477.5808; Fmt: '0.000'; Expected: '-922337203685477.581'),
    (Value: 1.5; Fmt: '0.0000'; Expected: '1.5000'),
    (Value: 1.5; Fmt: '0.####'; Expected: '1.5'),
    (Value: -0.0001; Fmt: '0.0000'; Expected: '-0.0001')
  );

// Halts with aCode and writes aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    Writeln('check ',aCode,' failed: ',aMsg);
    Halt(aCode);
    end;
end;


// Formats every entry of cCases with FormatCurr
procedure TestFormatCurr;

var
  lSettings: TFormatSettings;
  lResult: string;
  i: Integer;

begin
  lSettings:=DefaultFormatSettings;
  lSettings.DecimalSeparator:='.';
  lSettings.ThousandSeparator:=',';
  for i:=Low(cCases) to High(cCases) do
    begin
    lResult:=FormatCurr(cCases[i].Fmt,cCases[i].Value,lSettings);
    Check(10+i,lResult=cCases[i].Expected,'FormatCurr('''+cCases[i].Fmt+''') = '+lResult+', expected '+cCases[i].Expected);
    end;
end;


// Checks that FloatToDecimal with 19 digits stays inside TFloatRec
procedure TestFloatToDecimalBounds;

var
  lGuarded: TGuardedFloatRec;
  lExt: Extended;
  lCurr: Currency;
  lDigits: AnsiString;

begin
  FillChar(lGuarded.Guard,SizeOf(lGuarded.Guard),$AA);
  lExt:=1.5;
  FloatToDecimal(lGuarded.Rec,lExt,fvExtended,19,20);
  Check(30,lGuarded.Guard[0]=$AA,'FloatToDecimal(1.5) wrote past TFloatRec');
  Check(31,(lGuarded.Rec.Digits[0]='1') and (lGuarded.Rec.Digits[1]='5') and (lGuarded.Rec.Digits[2]=#0),'FloatToDecimal(1.5) digits');
  lExt:=2/3;
  FloatToDecimal(lGuarded.Rec,lExt,fvExtended,19,20);
  Check(32,lGuarded.Guard[0]=$AA,'FloatToDecimal(2/3) wrote past TFloatRec');
  lCurr:=922337203685477.5807;
  FloatToDecimal(lGuarded.Rec,lCurr,fvCurrency,19,4);
  Check(33,lGuarded.Guard[0]=$AA,'FloatToDecimal(MaxCurrency) wrote past TFloatRec');
  SetString(lDigits,PAnsiChar(@lGuarded.Rec.Digits[0]),Length(lGuarded.Rec.Digits));
  Check(34,lDigits='9223372036854775807','FloatToDecimal(MaxCurrency) digits '+lDigits);
  Check(35,lGuarded.Rec.Exponent=15,'FloatToDecimal(MaxCurrency) exponent');
end;


begin
  TestFormatCurr;
  TestFloatToDecimalBounds;
  Writeln('ok');
end.
