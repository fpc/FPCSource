{ A resourcestring of one non-ASCII character in a file with a UTF-8 signature and a mode that turns on the
  system code page: it is accepted and has the bytes of the same character in a longer resourcestring. }
{$mode delphiunicode}
program tbommode7;

resourcestring
  rsOne = 'é';
  rsTwo = 'éa';

begin
  if rsOne<>Copy(rsTwo,1,Length(rsTwo)-1) then
    begin
      Writeln('rsOne has ', Length(rsOne), ' elements, rsTwo ', Length(rsTwo));
      Halt(1);
    end;
  Writeln('ok');
end.
