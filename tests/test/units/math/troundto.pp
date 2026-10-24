{ RoundTo with quotients outside the Int64 range, NaN and Inf, issue #41751 }
program troundto;

{$mode objfpc}{$h+}

uses
  Math;

var
  gDouble: Double;
  gSingle: Single;
{$ifdef FPC_HAS_TYPE_EXTENDED}
  gExtended: Extended;
{$endif}


// Halts with aCode and writes aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln('check ',aCode,' failed: ',aMsg);
    halt(aCode);
    end;
end;


begin
  gDouble:=Double(High(Int64))+1;
  Check(1,RoundTo(gDouble,0)=gDouble,'Double High(Int64)+1, 0 digits, unchanged');
  gDouble:=Double(High(Int64))/10+1;
  Check(2,RoundTo(gDouble,-1)=gDouble,'Double High(Int64)/10+1, -1 digits, unchanged');
  gDouble:=-1e300;
  Check(3,RoundTo(gDouble,2)=gDouble,'Double -1e300, 2 digits, unchanged');
  Check(4,RoundTo(Double(4503599627370495.5),0)=4503599627370496.0,'Double 2^52-0.5 rounds to even');
  Check(5,RoundTo(Double(1250),2)=1200,'Double 1250, 2 digits, banker''s rounding');
  Check(6,SameValue(RoundTo(Double(1234.5678),-2),1234.57),'Double 1234.5678, -2 digits');
  Check(7,IsNan(RoundTo(Double(NaN),0)),'Double NaN stays NaN');
  Check(8,RoundTo(Double(Infinity),-2)=Infinity,'Double Inf stays Inf');
  Check(9,RoundTo(Double(NegInfinity),2)=NegInfinity,'Double -Inf stays -Inf');

  gSingle:=1e20;
  Check(11,RoundTo(gSingle,0)=gSingle,'Single 1e20, 0 digits, unchanged');
  gSingle:=-1e30;
  Check(12,RoundTo(gSingle,-3)=gSingle,'Single -1e30, -3 digits, unchanged');
  gSingle:=2.5;
  Check(13,RoundTo(gSingle,0)=2,'Single 2.5 rounds to even');
  gSingle:=NaN;
  Check(14,IsNan(RoundTo(gSingle,0)),'Single NaN stays NaN');
  gSingle:=Infinity;
  Check(15,RoundTo(gSingle,0)=Infinity,'Single Inf stays Inf');

{$ifdef FPC_HAS_TYPE_EXTENDED}
  gExtended:=1e30;
  Check(21,RoundTo(gExtended,0)=gExtended,'Extended 1e30, 0 digits, unchanged');
  gExtended:=-Extended(High(Int64))*4;
  Check(22,RoundTo(gExtended,-1)=gExtended,'Extended -4*High(Int64), -1 digits, unchanged');
  gExtended:=1250;
  Check(23,RoundTo(gExtended,2)=1200,'Extended 1250, 2 digits, banker''s rounding');
  gExtended:=NaN;
  Check(24,IsNan(RoundTo(gExtended,0)),'Extended NaN stays NaN');
{$endif}
  writeln('ok');
end.
