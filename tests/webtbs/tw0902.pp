uses
 dos;
begin
  writeln;
  writeln(fsearch('c:\command.com', ''));
  { here you get the full path in BP7, but nothing in FPC }
  writeln(fsearch('c:\command.com', 'c:\a'));
  { I really would not consider this as a bug !!  }
  { this use of fsearch is not document in BP PM  }
  if fsearch('c:\command.com', '')<>fsearch('c:\command.com', 'c:\a') then
    Writeln('fsearch result is not BP compatible');
end.
