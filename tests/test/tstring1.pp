program TestStr;

function TestOK:boolean;
Const
  TestStr: string[22]='HELLO, THIS IS A TEST ';
var
  I : INTEGER;
  U : STRING[1];
  Q : STRING[100];
  S : STRING[55];
  T : STRING[60];
  V : STRING;
begin
  TestOk:=false;
  T:='THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG 1234567890';
  Insert (T, T, 1);
{Writeln(T);}
  Delete (T, 55, 54);
  S:=Copy (T, -5, 2);     {'TH'}
  U:=Copy (T, 7, 4);      {'I'}
  S:=S + U;               {'THI'}
  Q:=Copy (T, 32, 70);    {'THE LAZY DOG 1234567890'}
  Delete (Q, 2, 1);         {'TE LAZY DOG 1234567890'}
  Delete (Q, 100, 2);       {'TE LAZY DOG 1234567890'}
  Delete (Q, 3, -4);        {'TE LAZY DOG 1234567890'}
  Delete (Q, 3, 10);        {'TE1234567890'}
{  writeln('TE1234567890 - ',Q);}
  I:=Pos ('S', T);        {25}
  Insert(Copy(T,I,200),Q,3);{'TES OVER THE LAZY DOG 12345678901234567890'}
  Delete (Q, 4, 6);         {'TESTHE LAZY DOG 12345678901234567890}
  S:=S + T [25];          {'THIS'}
  S:=S + Copy (S, 3, -5) + Copy (S, 3, 2);  {'THISIS'}
  V:=T;                   {'THE QUICK BROWN FOX JUMPS OVER THE LAZY ..'}
  Delete (V, 1, 36);      {'AZY DOG 1234567890'}
  if (Copy (V, -7, -1)='') and (Pos ('DOG', V)=5) then {TRUE}
   Insert (V, S, 200);    {'THISISAZY DOG 1234567890'}
  U:=Copy (T, 44, 40);    {' '}
  Insert (U, S, 5);         {'THIS ISAZY DOG 1234567890'}
  I:=Pos ('ZY', S);       {9}
  Delete (S, I, -5);        {'THIS ISAZY DOG 1234567890'}
  Insert (Copy(S,5,1),S,8); {'THIS IS AZY DOG 1234567890'}
  Delete (S, 10, 16);       {'THIS IS A0'}
  if S [Length (S)]='0' then {TRUE}
   S:=S + Q;            {'THIS IS A0TESTHE LAZY DOG 123456789012345...'}
  V:=Copy (S, Length (S) - 19, 10); {'1234567890'}
  if V=Copy (S, Length (S) - 9, 10) then {TRUE}
   Delete (S, 15, 3 * Length (V)+2); {'THIS IS A0TEST'}
  Insert ('', S, 0);        {'THIS IS A0TEST'}
  Insert(Copy(S,5,1),S,11); {'THIS IS A0 TEST'}
  Insert ('HELLO', S, -4);  {'HELLOTHIS IS A0 TEST'}
  Insert (',', S, 6);       {'HELLO,THIS IS A0 TEST'}
  Delete (S, Pos ('TEST', S) - 2, 1); {'HELLO,THIS IS A TEST'}
  Delete (Q, 1, 32767);     {''}
  Q:=Q + ' ';             {' '}
  Insert (Q, S, 7);         {'HELLO, THIS IS A TEST'}
  Insert (Q, S, 255);       {'HELLO, THIS IS A TEST '}
  if (S=TestStr) and (Q=' ') and (V='1234567890') and
     (T='THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG 1234567890') then
   TestOK:=true;
end;


begin
  if TestOK then
   WriteLn('Test OK')
  else
    begin
      WriteLn('Test Failure!');
      halt(1);
    end;  
end.
