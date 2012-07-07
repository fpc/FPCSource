{$APPTYPE CONSOLE}
program CurrencyFormatTest;

(*
   Test subject: .\rtl\inc\sstrings.inc::fpc_shortstr_currency
   Test FPC having problems: r21245, win32-i386
 *)

type  
  TTestCase = record
    value  : currency;
    expect : array [0..5] of string;
  end;

const
  test_cases : array [0..19] of TTestCase = (
   ( value : 0.9500;   expect : ('0.95000','0.9500','0.950','0.95','1.0','1')),
   ( value :-0.9500;   expect : ('-0.95000','-0.9500','-0.950','-0.95','-1.0','-1')),
   ( value : 1.4445;   expect : ('1.44450','1.4445','1.445','1.44','1.4','1')),
   ( value :-1.4445;   expect : ('-1.44450','-1.4445','-1.445','-1.44','-1.4','-1')),
   ( value : 199.4445; expect : ('199.44450','199.4445','199.445','199.44','199.4','199')),
   ( value :-199.4445; expect : ('-199.44450','-199.4445','-199.445','-199.44','-199.4','-199')),
   ( value : 1.9995;   expect : ('1.99950','1.9995','2.000','2.00','2.0','2')),
   ( value :-1.9995;   expect : ('-1.99950','-1.9995','-2.000','-2.00','-2.0','-2')),
   ( value : 99.9996;  expect : ('99.99960','99.9996','100.000','100.00','100.0','100')),
   ( value :-99.9996;  expect : ('-99.99960','-99.9996','-100.000','-100.00','-100.0','-100')),
   ( value : 0.9005;   expect : ('0.90050','0.9005','0.901','0.90','0.9','1')),
   ( value :-0.9005;   expect : ('-0.90050','-0.9005','-0.901','-0.90','-0.9','-1')),
   ( value : 0.0005;   expect : ('0.00050','0.0005','0.001','0.00','0.0','0')),
   ( value :-0.0005;   expect : ('-0.00050','-0.0005','-0.001','-0.00','-0.0','-0')), // NOTE!: at least Delphi 5/7 leaves '-' sign for zero!
   ( value : 0.0145;   expect : ('0.01450','0.0145','0.015','0.01','0.0','0')),
   ( value :-0.0145;   expect : ('-0.01450','-0.0145','-0.015','-0.01','-0.0','-0')), // NOTE!: at least Delphi 5/7 leaves '-' sign for zero!
   ( value : 99.9997;  expect : ('99.99970','99.9997','100.000','100.00','100.0','100')),
   ( value :-99.9997;  expect : ('-99.99970','-99.9997','-100.000','-100.00','-100.0','-100')),
   ( value : 999.9996; expect : ('999.99960','999.9996','1000.000','1000.00','1000.0','1000')),
   ( value :-999.9996; expect : ('-999.99960','-999.9996','-1000.000','-1000.00','-1000.0','-1000'))
  );

function test_it(const test_case:TTestCase) : boolean;
var 
  expect,
  s : string;
  i : integer;
  c : char;
  ok : boolean;
begin
  ok := true;
  writeln('Using Str for ',test_case.value);
  for i := high(test_case.expect) downto low(test_case.expect) do
    begin
      expect:=test_case.expect[high(test_case.expect)-i];
      str(test_case.value:0:i,s);
      if s=expect then
       c := ' '
      else
        begin
         c := '?';
         ok := false;
        end;
      writeln(c,' frac=',i,', expected=',expect,', got=',s);
    end;
  writeln;
  test_it := ok;
end;

var
  i : integer;
  ok : boolean;

begin
  writeln;
  ok := true;
  for i := low(test_cases) to high(test_cases) do
    if not test_it(test_cases[i]) then
      ok := false;
  if not ok then
    begin
      writeln('Verdict: failed!');
      halt(1);
    end;
end.
