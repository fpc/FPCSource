{ %FILES=testmac.txt }
Program trdtxt03;

{$ifdef fpc}
uses
  strings;
{$else}
uses
  SysUtils;
{$endif}

procedure test(b: boolean);
begin
  if b then exit;
  WriteLn('Error : Invalid data read!');
  halt(1);
end;

var
 T: Text;
 value_char: char;
 value_byte: byte;
 value_shortint : shortint;
 value_smallint : smallint;
 value_word : word;
 value_longint : longint;
 value_longword : cardinal;
 value_real : real;
 value_shortstr : shortstring;
 value_pchar : array[0..255] of char;
Begin
 Assign(T,'testmac.txt');
 Reset(T);
 { Read all the data in the correct order }
 { Read some characters }
 value_char := #0;
 ReadLn(T,value_char);
 test(value_char = 'a');
 value_char := #0;
 ReadLn(T,value_char);
 test(value_char = 'c');
 value_char := #0;
 ReadLn(T,value_char);
 test(value_char = 'z');
 value_char := #0;
 ReadLn(T,value_char);
 test(value_char = '#');
 { ***** Read some integer values ***** }
 {**** HEX ****}
 value_byte := 0;
 ReadLn(T,value_byte);
 test(value_byte = 127);
 value_byte := 0;
 ReadLn(T,value_byte);
 test(value_byte = 255);
 value_byte := 0;
 ReadLn(T,value_byte);
 test(value_byte = 51);
 value_shortint := 0;
 ReadLn(T,value_shortint);
 test(value_shortint = -127);
 {*** Integral *** }
 value_byte := 0;
 ReadLn(T,value_byte);
 test(value_byte = 127);
 value_byte := 0;
 ReadLn(T,value_byte);
 test(value_byte = 255);
 value_byte := 0;
 ReadLn(T,value_byte);
 test(value_byte = 33);
 value_shortint := 0;
 ReadLn(T,value_shortint);
 test(value_shortint = -127);
 {**** HEX ****}
 value_word := 0;
 ReadLn(T,value_word);
 test(value_word = 32767);
 value_word := 0;
 ReadLn(T,value_word);
 test(value_word = 65535);
 value_word := 0;
 ReadLn(T,value_word);
 test(value_word = 4660);
 value_smallint := 0;
 ReadLn(T,value_smallint);
 test(value_smallint = -32767);
 {*** Integral *** }
 value_word := 0;
 ReadLn(T,value_word);
 test(value_word = 12700);
 value_word := 0;
 ReadLn(T,value_word);
 test(value_word = 2550);
 value_word := 0;
 ReadLn(T,value_word);
 test(value_word = +33200);
 value_smallint := 0;
 ReadLn(T,value_smallint);
 test(value_smallint = -12700);
 {**** HEX ****}
 value_longword := 0;
 ReadLn(T,value_longword);
 test(value_longword = +$7FFFFFFF);
 value_longword := 0;
 ReadLn(T,value_longword);
 test(value_longword = $FFFFFFFF);
 value_longword := 0;
 ReadLn(T,value_longword);
 test(value_longword = $12341234);
 value_longint := 0;
 ReadLn(T,value_longint);
 test(value_longint = -$7FFFFFFF);
 {*** Integral *** }
 value_longword := 0;
 ReadLn(T,value_longword);
 test(value_longword = 12700);
 value_longword := 0;
 ReadLn(T,value_longword);
 test(value_longword = 2550);
 value_longword := 0;
 ReadLn(T,value_longword);
 test(value_longword = +2147483647);
 value_longint := 0;
 ReadLn(T,value_longint);
{ test(value_longint = -2147483648);}

 { Read some real type values }
 value_real := 0.0;
 ReadLn(T,value_real);
 test(trunc(value_real) = trunc(01234));
 value_real := 0.0;
 ReadLn(T,value_real);
 test(trunc(value_real) = trunc(1278.1278));
 value_real := 0.0;
 ReadLn(T,value_real);
 test(trunc(value_real) = trunc(121223.1278E00));
 value_real := 0.0;
 ReadLn(T,value_real);
 test(trunc(value_real) = trunc(121224.1278e2));
 value_real := 0.0;
 ReadLn(T,value_real);
 test(trunc(value_real) = trunc(121225.1278E02));
 value_real := 0.0;
 ReadLn(T,value_real);
 test(trunc(value_real) = trunc(121216.1278E+00));
 value_real := 0.0;
 ReadLn(T,value_real);
 test(trunc(value_real) = trunc(121227.1278e+2));
 value_real := 0.0;
 ReadLn(T,value_real);
 test(trunc(value_real) = trunc(121228.1278E+02));
 value_real := 0.0;
 ReadLn(T,value_real);
 test(trunc(value_real) = trunc(121233.1278E-00));
 value_real := 0.0;
 ReadLn(T,value_real);
 test(trunc(value_real) = trunc(121234.1278e-2));

 { Read some strings }
 value_shortstr := '';
 ReadLn(T,value_shortstr);
 test(length(value_shortstr) = 255);
 value_shortstr := '';
 ReadLn(T,value_shortstr);
 test(value_shortstr = 'Hello world!');
 value_shortstr := '';
 ReadLn(T,value_shortstr);
 test(length(value_shortstr) = 42);
 { Read a null terminated value }
 value_shortstr := '';
 ReadLn(T,value_pchar);
 test(strlen(value_pchar) = 33);

 { Read a value_charhar and make sure the value is value_chartrl-Z (#26) }
 ReadLn(T,value_char);
 test(value_char = #26);
 Close(T);
 WriteLn('All tests Ok!');
end.
