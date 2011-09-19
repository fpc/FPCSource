uses
{$ifdef unix}
  cwstring,
{$endif unix}
  SysUtils;
  
type
  ts850 = type AnsiString(850);
  ts1251 = type AnsiString(1251);
var
  a850:ts850;
  a1251 : ts1251;  
  au : utf8string;
begin 
  au := #$00AE#$00A7;
  a850 := au; 
  a1251 := au;
  if (a850>a1251) then
    halt(1);
  if (a850<a1251) then
    halt(2);
    
  a850 := a850 + 'a'; 
  if (a850<=a1251) then
    halt(3);
  if (a1251>=a850) then
    halt(4);
  
  writeln('ok');
end.
