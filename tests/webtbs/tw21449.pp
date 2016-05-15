
{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  data_record = record
    amountStr:String;
    amount:Currency;
  end;

const
  kColCount = 5;
  kFormatString:array[0..kColCount-1]of String = ( '%1.0f', '%1.1f', '%1.2f', '%1.3f', '%1.4f' );

  kRowCount = 2;
  kTestData:array[0..kRowCount-1] of data_record = (
    (amountStr:'209.98'; amount:209.98 ),
    (amountStr:'9.94'; amount:9.94 ) );
  ExpectedResults: array[0..kRowCount-1,0..kColCount-1] of string =
    (('210','210.0','209.98','209.980','209.9800'),
     ('10','9.9','9.94','9.940','9.9400'));

procedure test;
var
  amount:Currency;
  index:Integer;
  rowIndex:Integer;
begin
  rowIndex := 0;
  while( rowIndex < kRowCount )do
  begin
    val(kTestData[rowIndex].amountStr,amount,index);
    if index<>0 then
      halt(1);
    write(kTestData[rowIndex].amountStr,' -- ',amount:0:4,': ');
    index := 0;
    while( index < kColCount )do
    begin
      write(Format( kFormatString[index], [amount] ),',');
      if Format( kFormatString[index], [amount] )<>ExpectedResults[rowindex,index] then
        halt(2);
      Inc( index );
    end;
    writeln;
    Inc( rowIndex );
  end;
end;

begin
  DecimalSeparator := '.';
  test;
  writeln('ok');
end.

