{ %KNOWNRUNERROR=1 Known array of char problems }
{$P-}

type
   CharA4 = array [1..4] of char;
   CharA6 = array [1..6] of char;
   String4 = String[4];
   String5 = String[5];
   String6 = String[6];
   String8 = String[8];

const
   car4_1 : CharA4 = 'ABCD';
   car4_2 : CharA4 = 'abcd';
   car6_1 : CharA6 = 'EFGHIJ';
   car6_2 : CharA6 = 'efghij';
   cst4_1 : String4 = 'ABCD';
   cst6_2 : string6 = 'EFGHIJ';
   cst8_1 : string8 = 'abcd';
   cst8_2 : string8 = 'efghij';

var
  ar4_1, ar4_2 : CharA4;
  ar6_1, ar6_2 : CharA6;
  st4_1, st4_2 : string4;
  st5_1, st5_2 : string5;
  st6_1, st6_2 : string6;
  st8_1, st8_2 : string8;
  pc : pchar;

const
  has_errors : boolean = false;

  procedure error(const st : string);
    begin
      Writeln('Error: ',st);
      has_errors:=true;
    end;

  procedure testvalueconv(st : string4);
  begin
    Writeln('st=',st);
    Writeln('Length(st)=',Length(st));
    If Length(st)>4 then
      Error('string length too big in calling value arg');
  end;

  procedure testconstconv(const st : string4);
  begin
    Writeln('st=',st);
    Writeln('Length(st)=',Length(st));
    If Length(st)>4 then
      Error('string length too big in calling const arg');
  end;

  procedure testvarconv(var st : string4);
  begin
    Writeln('st=',st);
    Writeln('Length(st)=',Length(st));
    If Length(st)>4 then
      Error('string length too big in calling var arg');
  end;

begin
  { compare array of char to constant strings }
  Writeln('Testing if "',car4_1,'" is equal to "',cst4_1,'"');
  if car4_1<>cst4_1 then
    error('Comparison of array of char and string don''t work');
  Writeln('Testing if "',car4_1,'" is equal to "ABCD"');
  if car4_1<>'ABCD' then
    error('Comparison of array of char and constat string don''t work');
  Writeln('Testing if "',cst4_1,'" is equal to "ABCD"');
  if 'ABCD'<>cst4_1 then
    error('Comparison of string and constant string don''t work');
  car4_1:='AB'#0'D';
  if car4_1='AB' then
    Writeln('Anything beyond a #0 is ignored')
  else if car4_1='AB'#0'D' then
    Writeln('Chars after #0 are not ignored')
  else
    Error('problems if #0 in array of char');
{$ifdef FPC this is not allowed in BP !}
  car4_1:=cst4_1;
{ if it is allowed then it must also work correctly !! }
  Writeln('Testing if "',car4_1,'" is equal to "',cst4_1,'"');
  if car4_1<>cst4_1 then
    error('Comparison of array of char and string don''t work');
  if string4(car6_2)<>'efgh' then
    error('typcasting to shorter strings leads to problems');
  ar4_2:='Test';
  ar4_1:=cst6_2;
  if ar4_2<>'Test' then
    error('overwriting beyond char array size');
  ar6_1:='Test'#0'T';
  st6_1:=ar6_1;
  if (st6_1<>ar6_1) or (st6_1='Test') then
    error('problems with #0');
  ar6_1:='AB';
  if ar6_1='AB'#0't'#0'T' then
    Error('assigning strings to array of char does not zero end of array if string is shorter');
  if ar6_1='AB'#0#0#0#0 then
    writeln('assigning shorter strings to array of char does zero  fo tserarray')
  else
    error('assigning "AB" to ar6_1 gives '+ar6_1);
{$endif}
  cst8_1:=car4_1;
{ if it is allowed then it must also work correctly !! }
  Writeln('Testing if "',car4_1,'" is equal to "',cst8_1,'"');
  if car4_1<>cst8_1 then
    error('Comparison of array of char and string don''t work');
  st4_2:='Test';
  st4_1:=car6_1;
  if (st4_2<>'Test') or (st4_1<>'EFGH') then
    error('problems when copying long char array to shorter string');
  testvalueconv('AB');
  testvalueconv('ABCDEFG');
  testvalueconv(car4_1);
  testvalueconv(car6_1);
  getmem(pc,256);
  pc:='Long Test';
{$ifdef FPC this is not allowed in BP !}
  testvalueconv(pc);
{$endif def FPC this is not allowed in BP !}
  testconstconv('AB');
  testconstconv('ABCDEFG');
  testconstconv(st4_1);
  testconstconv(cst6_2);
{$ifdef FPC this is not allowed in BP !}
  testconstconv(pc);
{$endif def FPC this is not allowed in BP !}
  testvarconv(st4_2);
  testvarconv(cst4_1);
{$ifdef FPC this is not allowed in BP !}
  testvarconv(st6_1);
  testvarconv(cst8_1);
{$endif def FPC this is not allowed in BP !}
  { testvarconv(pc); this one fails at compilation }
  if has_errors then
    begin
      Writeln('There are still problems with arrays of char');
      Halt(1);
    end;
end.
