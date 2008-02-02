program tst2;
{$ifdef fpc}{$mode objfpc}{$h+}{$endif}

var
 a: array[0..0] of char = (#0);

function test_pchar: boolean;
var
 s: string;
 p: pchar;
begin
 p := '';
 s := '1234567890';
 s := p;
 test_pchar := (s = '');
 if not test_pchar then writeln('test_pchar failed');
end;

function test_chararray: boolean;
var
 s: string;
begin
 s := '1234567890';
 s := a;
 test_chararray := (s = '');
 if not test_chararray then writeln('test_chararray failed');  
end;

function test_pchar_to_widestr: boolean;
var
 s: widestring;
 p: PChar;
begin
 p := '';
 s := '1234567890';
 s := p;                         { win32: function result assign not optimized! }
 test_pchar_to_widestr := (s = '');
 if not test_pchar_to_widestr then writeln('test_pchar_to_widestr failed');  
end;

function test_chararray_to_widestr: boolean;
var
 s: widestring;
begin
 s := '1234567890';
 s := a;
 test_chararray_to_widestr := (s = '');
 if not test_chararray_to_widestr then writeln('test_chararray_to_widestr failed');  
end;

begin
 if not test_pchar then Halt(1);
 if not test_chararray then Halt(2);
 if not test_pchar_to_widestr then Halt(3);
 if not test_chararray_to_widestr then Halt(4);
end.
