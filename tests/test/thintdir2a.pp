{ %FAIL }
{ %NORUN }
program thintdir2a;

// don't allow to use hint modifier twice

{$mode delphi}
const
  Test = 1 deprecated 'Do not use this const' deprecated 'Use that const';
begin
end.                      
