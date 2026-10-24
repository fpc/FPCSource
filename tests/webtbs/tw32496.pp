program tw32496;

{$mode objfpc}{$H+}

uses
  sysutils;

var
  lErrors: Integer = 0;


procedure Check(aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
      Writeln('Failed: ',aMsg);
      Inc(lErrors);
    end;
end;


procedure TestUnsigned;

var
  lU: LongWord;
  lCount: SizeInt;
begin
  lU:=0;
  lCount:=SScanf('4000000000','%u',[@lU]);
  Check(lCount=1,'%u converts one value');
  Check(lU=4000000000,'%u reads a value above High(LongInt)');
  lU:=7;
  Check(SScanf('-5','%u',[@lU])=0,'%u does not accept a negative number');
  Check(lU=7,'%u leaves the target untouched on failure');
end;


procedure TestPercent;

var
  lI1, lI2: LongInt;
begin
  lI1:=0;
  Check(SScanf('50%','%d%%',[@lI1])=1,'%% is accepted after a specifier');
  Check(lI1=50,'value before %% is converted');
  lI2:=0;
  Check(SScanf('50% 7','%d%% %d',[@lI1,@lI2])=2,'%% matches a literal percent sign');
  Check(lI2=7,'value after %% is converted');
  Check(SScanf('50x 7','%d%% %d',[@lI1,@lI2])=1,'%% does not match another character');
end;


procedure TestLiterals;

var
  lI1, lI2: LongInt;
  lFailed: Boolean;
begin
  lI1:=0;
  lI2:=0;
  Check(SScanf('123.456 789','%d %d',[@lI1,@lI2])=1,'%d stops at a decimal separator');
  Check(lI1=123,'%d reads the integer part');
  Check(lI2=0,'second %d is not assigned');
  Check(SScanf('(123, 456)','(%d, %d)',[@lI1,@lI2])=2,'literals around specifiers match');
  Check((lI1=123) and (lI2=456),'values between literals are converted');
  Check(SScanf('(123, 456','(%d, %d)',[@lI1,@lI2])=2,'result counts converted fields only');
  Check(SScanf('x=5','y=%d',[@lI1])=0,'literal mismatch stops the scan');
  Check(SScanf('abc x=5','x=%d',[@lI1])=0,'literal is matched at the current position');
  Check(SScanf('1;2','%d,%d',[@lI1,@lI2])=1,'literal mismatch between specifiers stops the scan');
  Check(SScanf('1   2','%d %d',[@lI1,@lI2])=2,'format space matches several spaces');
  Check(SScanf('1,2',' %d , %d',[@lI1,@lI2])=2,'format space matches no space');
  Check((lI1=1) and (lI2=2),'values around optional spaces are converted');
  Check(SScanf('-','%d',[@lI1])=0,'a sign without digits is not a number');
  lFailed:=False;
  try
    SScanf('5','%d%',[@lI1,@lI2]);
  except
    on EFormatError do
      lFailed:=True;
  end;
  Check(lFailed,'a trailing % in the format raises EFormatError');
end;


procedure TestOther;

var
  lI: LongInt;
  lC: Char;
  lS: string;
  lE: Extended;
begin
  lC:=#0;
  Check(SScanf('1x','%d%c',[@lI,@lC])=2,'%c reads the last character');
  Check(lC='x','%c value');
  Check(SScanf('asdf 1'+FormatSettings.DecimalSeparator+'5 12','%s %f %d',[@lS,@lE,@lI])=3,'%s %f %d');
  Check((lS='asdf') and (lE=1.5) and (lI=12),'%s %f %d values');
end;


begin
  TestUnsigned;
  TestPercent;
  TestLiterals;
  TestOther;
  if lErrors>0 then
    Halt(1);
  Writeln('ok');
end.
