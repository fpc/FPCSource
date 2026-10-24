{ Exp of negative and extreme arguments in every rounding mode, issue #41837 }
program texpneg;

{$mode objfpc}{$h+}

uses
  Math;

type
  TExpCase = record
    x: QWord;
    ref: QWord;
  end;

const
  { bit patterns of x and of the correctly rounded exp(x) }
  Cases: array[1..84] of TExpCase = (
    (x: QWord($BF50624DD2F1A9FC); ref: QWord($3FEFF7CFE56F1A9E)), { exp(-0.001) }
    (x: QWord($BFD0000000000000); ref: QWord($3FE8EBEF9EAC820B)), { exp(-0.25) }
    (x: QWord($BFE0000000000000); ref: QWord($3FE368B2FC6F960A)), { exp(-0.5) }
    (x: QWord($BFE8000000000000); ref: QWord($3FDE3B40EBEFCD7E)), { exp(-0.75) }
    (x: QWord($BFF0000000000000); ref: QWord($3FD78B56362CEF38)), { exp(-1.0) }
    (x: QWord($C000000000000000); ref: QWord($3FC152AAA3BF81CC)), { exp(-2.0) }
    (x: QWord($C014000000000000); ref: QWord($3F7B993FE00D5376)), { exp(-5.0) }
    (x: QWord($C024000000000000); ref: QWord($3F07CD79B5647C9B)), { exp(-10.0) }
    (x: QWord($C034000000000000); ref: QWord($3E21B48655F37267)), { exp(-20.0) }
    (x: QWord($C059000000000000); ref: QWord($36EA8C1F14E2AF5D)), { exp(-100.0) }
    (x: QWord($C07F400000000000); ref: QWord($12D9265E78D4438D)), { exp(-500.0) }
    (x: QWord($C085E00000000000); ref: QWord($00D14F2B0FB9307F)), { exp(-700.0) }
    (x: QWord($C086240000000000); ref: QWord($000E6CF6D08897AC)), { exp(-708.5) }
    (x: QWord($C086280000000000); ref: QWord($0008BFE55DE02338)), { exp(-709.0) }
    (x: QWord($3F50624DD2F1A9FC); ref: QWord($3FF0041919B7EE34)), { exp(0.001) }
    (x: QWord($3FE0000000000000); ref: QWord($3FFA61298E1E069C)), { exp(0.5) }
    (x: QWord($3FF0000000000000); ref: QWord($4005BF0A8B145769)), { exp(1.0) }
    (x: QWord($4000000000000000); ref: QWord($401D8E64B8D4DDAE)), { exp(2.0) }
    (x: QWord($4024000000000000); ref: QWord($40D5829DCF950560)), { exp(10.0) }
    (x: QWord($4059000000000000); ref: QWord($48F3494A9B171BF5)), { exp(100.0) }
    (x: QWord($407F400000000000); ref: QWord($6D045BA2A9F7E439)), { exp(500.0) }
    (x: QWord($4085E00000000000); ref: QWord($7F0D945DF4F8EC8E)), { exp(700.0) }
    (x: QWord($40862C0000000000); ref: QWord($7FE81E9B4B52D0C9)), { exp(709.5) }
    (x: QWord($40862D999999999A); ref: QWord($7FED75AE7A50EE14)), { exp(709.7) }
    (x: QWord($C0860A0000000000); ref: QWord($005740ACCC91284A)), { exp(-705.25) }
    (x: QWord($C0854C0000000000); ref: QWord($027BEC073FBD3202)), { exp(-681.5) }
    (x: QWord($C0848E0000000000); ref: QWord($04A0C3B5BE1E3B94)), { exp(-657.75) }
    (x: QWord($C083D00000000000); ref: QWord($06C421869A4110E5)), { exp(-634.0) }
    (x: QWord($C083120000000000); ref: QWord($08E82C67043D5433)), { exp(-610.25) }
    (x: QWord($C082540000000000); ref: QWord($0B0D07178E742492)), { exp(-586.5) }
    (x: QWord($C081960000000000); ref: QWord($0D316DA9A7A1DFE5)), { exp(-562.75) }
    (x: QWord($C080D80000000000); ref: QWord($0F54ED9B506932EF)), { exp(-539.0) }
    (x: QWord($C0801A0000000000); ref: QWord($11792176F592A610)), { exp(-515.25) }
    (x: QWord($C07EB80000000000); ref: QWord($139E2D5D77F36390)), { exp(-491.5) }
    (x: QWord($C07D3C0000000000); ref: QWord($15C21E587DBAE9E2)), { exp(-467.75) }
    (x: QWord($C07BC00000000000); ref: QWord($17E5C1C4EDE2B7E9)), { exp(-444.0) }
    (x: QWord($C07A440000000000); ref: QWord($1A0A203B42CC3951)), { exp(-420.25) }
    (x: QWord($C078C80000000000); ref: QWord($1C2F5F4A9F51208F)), { exp(-396.5) }
    (x: QWord($C0774C0000000000); ref: QWord($1E52D6067AD6562F)), { exp(-372.75) }
    (x: QWord($C075D00000000000); ref: QWord($20769E556086EEE2)), { exp(-349.0) }
    (x: QWord($C074540000000000); ref: QWord($229B29164D828B52)), { exp(-325.25) }
    (x: QWord($C072D80000000000); ref: QWord($24C04EAA93D39A5F)), { exp(-301.5) }
    (x: QWord($C0715C0000000000); ref: QWord($26E394FA8D0DE1A6)), { exp(-277.75) }
    (x: QWord($C06FC00000000000); ref: QWord($290783A1D4C0FAA7)), { exp(-254.0) }
    (x: QWord($C06CC80000000000); ref: QWord($2B2C3C6E5CA93303)), { exp(-230.25) }
    (x: QWord($C069D00000000000); ref: QWord($2D50F3FBF0D8EE55)), { exp(-206.5) }
    (x: QWord($C066D80000000000); ref: QWord($2F745B7E718C00CA)), { exp(-182.75) }
    (x: QWord($C063E00000000000); ref: QWord($31987202D671DAE5)), { exp(-159.0) }
    (x: QWord($C060E80000000000); ref: QWord($33BD5AADC40DBCA3)), { exp(-135.25) }
    (x: QWord($C05BE00000000000); ref: QWord($35E19FD93D9E7785)), { exp(-111.5) }
    (x: QWord($C055F00000000000); ref: QWord($380529DED105842F)), { exp(-87.75) }
    (x: QWord($C050000000000000); ref: QWord($3A2969D47321E4CC)), { exp(-64.0) }
    (x: QWord($C044200000000000); ref: QWord($3C4E84430D66E9FB)), { exp(-40.25) }
    (x: QWord($C030800000000000); ref: QWord($3E725284D838EF8C)), { exp(-16.5) }
    (x: QWord($401D000000000000); ref: QWord($4096006B5D53E8D9)), { exp(7.25) }
    (x: QWord($403F000000000000); ref: QWord($42BA6B765D8CDF6D)), { exp(31.0) }
    (x: QWord($404B600000000000); ref: QWord($44DFB9A123043219)), { exp(54.75) }
    (x: QWord($4053A00000000000); ref: QWord($47030C43BF8CCA52)), { exp(78.5) }
    (x: QWord($4059900000000000); ref: QWord($4926DF76F03BC2FA)), { exp(102.25) }
    (x: QWord($405F800000000000); ref: QWord($4B4B774C12967DFA)), { exp(126.0) }
    (x: QWord($4062B80000000000); ref: QWord($4D707D9FBE16FE2D)), { exp(149.75) }
    (x: QWord($4065B00000000000); ref: QWord($4F93CD5DADF2F278)), { exp(173.5) }
    (x: QWord($4068A80000000000); ref: QWord($51B7C757AB6B2574)), { exp(197.25) }
    (x: QWord($406BA00000000000); ref: QWord($53DC8DBCFFB57D24)), { exp(221.0) }
    (x: QWord($406E980000000000); ref: QWord($560124CD25A3DCDA)), { exp(244.75) }
    (x: QWord($4070C80000000000); ref: QWord($5824961D34EBA01D)), { exp(268.5) }
    (x: QWord($4072440000000000); ref: QWord($5A48B86719BC60E4)), { exp(292.25) }
    (x: QWord($4073C00000000000); ref: QWord($5C6DAF34AAE43996)), { exp(316.0) }
    (x: QWord($40753C0000000000); ref: QWord($5E91D29956E2D65A)), { exp(339.75) }
    (x: QWord($4076B80000000000); ref: QWord($60B566CFD9E9FC30)), { exp(363.5) }
    (x: QWord($4078340000000000); ref: QWord($62D9B30251C9F25A)), { exp(387.25) }
    (x: QWord($4079B00000000000); ref: QWord($64FEDC22DC262BD7)), { exp(411.0) }
    (x: QWord($407B2C0000000000); ref: QWord($672287476F0472D4)), { exp(434.75) }
    (x: QWord($407CA80000000000); ref: QWord($69463FC63443AEEF)), { exp(458.5) }
    (x: QWord($407E240000000000); ref: QWord($6B6AB78A19E0FBF2)), { exp(482.25) }
    (x: QWord($407FA00000000000); ref: QWord($6D900A7DE459292B)), { exp(506.0) }
    (x: QWord($40808E0000000000); ref: QWord($6FB3431D339A59CD)), { exp(529.75) }
    (x: QWord($40814C0000000000); ref: QWord($71D721540C4FE8F3)), { exp(553.5) }
    (x: QWord($40820A0000000000); ref: QWord($73FBC6630D6028FD)), { exp(577.25) }
    (x: QWord($4082C80000000000); ref: QWord($7620AD1C1FE99A69)), { exp(601.0) }
    (x: QWord($4083860000000000); ref: QWord($784406632D88C747)), { exp(624.75) }
    (x: QWord($4084440000000000); ref: QWord($7A680BD07BC1EB66)), { exp(648.5) }
    (x: QWord($4085020000000000); ref: QWord($7C8CDFF5C3916C1F)), { exp(672.25) }
    (x: QWord($4085C00000000000); ref: QWord($7EB1562AECC6EAB2))  { exp(696.0) }
  );

  MaxUlp = 4;

  ModeNames: array[TFPURoundingMode] of string = ('nearest','down','up','truncate');


// Returns the double with bit pattern aBits
function BitsToDouble(aBits: QWord): Double;

begin
  Move(aBits,Result,SizeOf(Result));
end;


// Returns the bit pattern of aValue
function DoubleToBits(aValue: Double): QWord;

begin
  Move(aValue,Result,SizeOf(Result));
end;


// Returns the largest distance in ulp between Exp and the reference over all cases
function WorstUlp(out aWorstCase: Integer): QWord;

var
  i: Integer;
  lGot, lDiff: QWord;
  lValue: Double;

begin
  Result:=0;
  aWorstCase:=Low(Cases);
  for i:=Low(Cases) to High(Cases) do
    begin
    lValue:=Exp(BitsToDouble(Cases[i].x));
    lGot:=DoubleToBits(lValue);
    if lGot>Cases[i].ref then
      lDiff:=lGot-Cases[i].ref
    else
      lDiff:=Cases[i].ref-lGot;
    if lDiff>Result then
      begin
      Result:=lDiff;
      aWorstCase:=i;
      end;
    end;
end;


var
  lMode: TFPURoundingMode;
  lWorst: QWord;
  lCase: Integer;

begin
  for lMode:=Low(TFPURoundingMode) to High(TFPURoundingMode) do
    begin
    SetRoundMode(lMode);
    lWorst:=WorstUlp(lCase);
    SetRoundMode(rmNearest);
    writeln('round ',ModeNames[lMode],': worst ',lWorst,' ulp for exp(',BitsToDouble(Cases[lCase].x),')');
    if lWorst>MaxUlp then
      halt(1+Ord(lMode));
    end;
  writeln('ok');
end.
