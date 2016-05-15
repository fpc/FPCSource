uses
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif unix}
  SysUtils;
  
type
  ts1253 = type AnsiString(1253);
  ts1251 = type AnsiString(1251);
var
  s1 : ts1253;
  s2 : ts1251;
  au : unicodestring;
begin 
  au := #$20AC; // Euro symbol
  s1 := au;
  s2 := au;
  if Ord(s1[1]) = Ord(s2[1]) then
    halt(5);
  if (s1>s2) then
    halt(1);
  if (s1<s2) then
    halt(2);
    
  s1 := s1 + 'a';
  if (s1<=s2) then
    halt(3);
  if (s2>=s1) then
    halt(4);
  
  writeln('ok');
end.
