{ For loops over the whole range of the counter type, and loops whose count does not fit the counter type, issue #42097 }

{$mode objfpc}
{$Q-,R-}

{$ifdef CPUINT64}
  {$define TEST_64BIT_TYPES}
{$endif}

// Halts with aCode when aGot is not aExpected
procedure Check(aCode: Integer; aGot, aExpected: QWord);

begin
  if aGot<>aExpected then
    begin
    writeln('check ',aCode,' failed: ',aGot,' iterations, expected ',aExpected);
    halt(aCode);
    end;
end;


{$ifdef TEST_64BIT_TYPES}
function QWordFullRange: QWord;

var
  q: QWord;

begin
  Result:=0;
  for q:=0 to high(QWord) do
    begin
    Inc(Result);
    if Result=3 then
      break;
    end;
end;


function Int64FullRange: QWord;

var
  i: Int64;

begin
  Result:=0;
  for i:=low(Int64) to high(Int64) do
    begin
    Inc(Result);
    if Result=3 then
      break;
    end;
end;


function Int64FullRangeDown: QWord;

var
  i: Int64;

begin
  Result:=0;
  for i:=high(Int64) downto low(Int64) do
    begin
    Inc(Result);
    if Result=3 then
      break;
    end;
end;
{$endif TEST_64BIT_TYPES}


function ByteConst: QWord;

var
  b: Byte;

begin
  Result:=0;
  for b:=0 to 255 do
    Inc(Result);
end;


function ByteTo(aFrom, aTo: Byte): QWord;

var
  b: Byte;

begin
  Result:=0;
  if aFrom=0 then
    for b:=0 to aTo do
      Inc(Result)
  else
    for b:=5 to aTo do
      Inc(Result);
end;


function CardinalTo(aFrom, aTo: Cardinal; aBreak: QWord): QWord;

var
  c: Cardinal;

begin
  Result:=0;
  if aFrom=0 then
    begin
    for c:=0 to aTo do
      begin
      Inc(Result);
      if Result=aBreak then
        break;
      end;
    end
  else
    for c:=5 to aTo do
      begin
      Inc(Result);
      if Result=aBreak then
        break;
      end;
end;


{$ifdef TEST_64BIT_TYPES}
function QWordFrom5(aTo: QWord): QWord;

var
  q: QWord;

begin
  Result:=0;
  for q:=5 to aTo do
    begin
    Inc(Result);
    if Result=10 then
      break;
    end;
end;
{$endif TEST_64BIT_TYPES}


function LongintTo(aFrom, aTo: Longint; aBreak: QWord): QWord;

var
  i: Longint;

begin
  Result:=0;
  case aFrom of
    0:
      for i:=0 to aTo do
        begin
        Inc(Result);
        if Result=aBreak then
          break;
        end;
    1:
      for i:=1 to aTo do
        begin
        Inc(Result);
        if Result=aBreak then
          break;
        end;
  else
    for i:=-1 to aTo do
      begin
      Inc(Result);
      if Result=aBreak then
        break;
      end;
  end;
end;


function LongintCountTo(aCount: Longint): QWord;

var
  i: Longint;

begin
  Result:=0;
  for i:=0 to aCount-1 do
    Inc(Result);
end;


begin
{$ifdef TEST_64BIT_TYPES}
  Check(1,QWordFullRange,3);
  Check(2,Int64FullRange,3);
  Check(3,Int64FullRangeDown,3);
{$endif TEST_64BIT_TYPES}
  Check(4,ByteConst,256);
  Check(5,ByteTo(0,255),256);
  Check(6,ByteTo(0,7),8);
  Check(7,ByteTo(5,2),0);
  Check(8,ByteTo(5,9),5);
  Check(9,CardinalTo(0,high(Cardinal),7),7);
  Check(10,CardinalTo(5,2,High(QWord)),0);
  Check(11,CardinalTo(5,9,High(QWord)),5);
{$ifdef TEST_64BIT_TYPES}
  Check(12,QWordFrom5(2),0);
  Check(13,QWordFrom5(6),2);
{$endif TEST_64BIT_TYPES}
  Check(14,LongintTo(0,high(Longint),10),10);
  Check(15,LongintTo(0,-5,High(QWord)),0);
  Check(16,LongintTo(1,5,High(QWord)),5);
  Check(17,LongintTo(1,low(Longint),High(QWord)),0);
  Check(18,LongintTo(-1,3,High(QWord)),5);
  Check(19,LongintCountTo(10),10);
  Check(20,LongintCountTo(0),0);
  writeln('ok');
end.
