{$mode objfpc}
resourcestring
  s = 'Hello world';

begin
   if s<>'Hello world' then
     halt(1);
end.
