{$codepage utf-8}

const
  engChar: WideChar = 'r'; // OK
  rusChar1: WideChar = 'ё'; // Error
  rusChar2: WideChar = WideChar('ё'); // Error
  eng: array[0..2] of WideChar = ('u', 'R', 'z'); // OK
  rus1: array[0..2] of WideChar = ('ё', 'м', 'я'); // Error
  rus2: array[0..2] of WideChar = (WideChar('ё'), WideChar('м'), WideChar('я')); // Error

  w: unicodestring = 'ёмя';

begin
  if rusChar1<>w[1] then
    halt(1);

if rus1[0]<>w[1] then
    halt(2);
  if rus1[1]<>w[2] then
    halt(3);
  if rus1[2]<>w[3] then
    halt(4);

  if rus2[0]<>w[1] then
    halt(5);
  if rus2[1]<>w[2] then
    halt(6);
  if rus2[2]<>w[3] then
    halt(7);
end.
