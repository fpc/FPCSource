
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

const
  has_errors : boolean = false;

  procedure error(const st : string);
    begin
      Writeln('Error: ',st);
      has_errors:=true;
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
  if has_errors then
    begin
      Writeln('There are still problems with arrays of char');
      Halt(1);
    end;
end.
