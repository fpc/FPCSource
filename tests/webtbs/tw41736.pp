{ CompareStr on strings that differ after the first half, also with -tunicodertl }
program tw41736;

{$mode objfpc}{$h+}

uses
  SysUtils;

var
  s1, s2: string;

begin
  s1:='tvectorpanel';
  s2:='tvectorsheet';
  if CompareStr(s1,s2)>=0 then
    halt(1);
  if CompareStr(s2,s1)<=0 then
    halt(2);
  if CompareStr(s1,s1)<>0 then
    halt(3);
  if CompareStr(s1,Copy(s1,1,Length(s1)))<>0 then
    halt(4);
  if CompareStr('abcdefgh','abcdefgi')>=0 then
    halt(5);
  if CompareStr('abcdefgi','abcdefgh')<=0 then
    halt(6);
  if CompareStr('abcdef','abcdefgh')>=0 then
    halt(7);
  if CompareStr('abcdefgh','abcdef')<=0 then
    halt(8);
  if CompareStr('','a')>=0 then
    halt(9);
  if CompareStr('','')<>0 then
    halt(10);
  writeln('ok');
end.
