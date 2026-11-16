{ A resourcestring of one non-ASCII character in a UTF-8 source is accepted and has the bytes of the same
  character in a longer resourcestring. }
{$mode objfpc}{$H+}
{$codepage utf8}
program tresstr2;

resourcestring
  rsOne = 'é';
  rsTwo = 'éa';

begin
  if rsOne<>Copy(rsTwo,1,Length(rsTwo)-1) then
    begin
      Writeln('rsOne has ', Length(rsOne), ' bytes, rsTwo ', Length(rsTwo));
      Halt(1);
    end;
  Writeln('ok');
end.
