{ Old file: tbs0220.pp }
{ array of char overloading problem with strings        OK 0.99.11 (PFV) }

type
  a = array[1..100] of char;

var
  a1 : a;
  s : string;
begin
  a1[1]:='1';a1[2]:='2';a1[3]:='3';
  a1[4]:='4';a1[5]:='5';a1[6]:='6';
  a1[7]:='7';a1[8]:='8';a1[9]:='9';
  a1[10]:='0';a1[11]:='1';
  s:=Copy(a1,1,10);
  if s<>'1234567890' then halt(1);
  writeln('ok');
end.
