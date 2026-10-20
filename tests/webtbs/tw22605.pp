{ HostAddrToStr6/NetAddrToStr6 produce the RFC 5952 text form of an IPv6 address }
program tw22605;

{$mode objfpc}{$H+}

uses
  Sockets, SysUtils;

type
  TAddrCase = record
    Input, Expected: string;
  end;

const
  CCases: array[0..17] of TAddrCase = (
    (Input: '0:1:0:2:0:0:3:4';            Expected: '0:1:0:2::3:4'),
    (Input: '1:0:2:0:0:3:0:4';            Expected: '1:0:2::3:0:4'),
    (Input: '1:0:0:2:0:0:0:3';            Expected: '1:0:0:2::3'),
    (Input: '1:0:0:2:0:0:3:4';            Expected: '1::2:0:0:3:4'),
    (Input: '2001:db8:0:1:1:1:1:1';       Expected: '2001:db8:0:1:1:1:1:1'),
    (Input: '2001:0DB8:0000:0000:0000:0000:0000:0001'; Expected: '2001:db8::1'),
    (Input: 'FE80::3B92:3429:FF16:A3E4';  Expected: 'fe80::3b92:3429:ff16:a3e4'),
    (Input: '0:0:0:0:0:0:0:0';            Expected: '::'),
    (Input: '0:0:0:0:0:0:0:1';            Expected: '::1'),
    (Input: '1:0:0:0:0:0:0:0';            Expected: '1::'),
    (Input: '0:0:1:0:0:0:0:0';            Expected: '0:0:1::'),
    (Input: '1:2:3:4:5:6:7:0';            Expected: '1:2:3:4:5:6:7:0'),
    (Input: '0:2:3:4:5:6:7:8';            Expected: '0:2:3:4:5:6:7:8'),
    (Input: 'abcd:ef01:2345:6789:abcd:ef01:2345:6789'; Expected: 'abcd:ef01:2345:6789:abcd:ef01:2345:6789'),
    (Input: '::ffff:192.0.2.1';           Expected: '::ffff:192.0.2.1'),
    (Input: '::FFFF:C000:0201';           Expected: '::ffff:192.0.2.1'),
    (Input: '::ffff:0.0.0.0';             Expected: '::ffff:0.0.0.0'),
    (Input: '::1:ffff:c000:201';          Expected: '::1:ffff:c000:201')
  );

var
  lErrors: Integer;


{ Reports a failed check }
procedure Check(aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
      writeln('FAIL: ', aMsg);
      Inc(lErrors);
    end;
end;


{ Returns True when both addresses are equal }
function SameAddr(const aLeft, aRight: TIn6_Addr): Boolean;

begin
  Result:=CompareByte(aLeft, aRight, SizeOf(TIn6_Addr))=0;
end;


{ Checks the fixed list of addresses and their expected text form }
procedure TestCases;

var
  i: Integer;
  lAddr: TIn6_Addr;
  lStr: string;

begin
  for i:=Low(CCases) to High(CCases) do
    begin
      Check(TryStrToHostAddr6(CCases[i].Input, lAddr), CCases[i].Input+' parses');
      lStr:=HostAddrToStr6(lAddr);
      Check(lStr=CCases[i].Expected, 'HostAddrToStr6('+CCases[i].Input+') = '+CCases[i].Expected+', got '+lStr);
      lStr:=NetAddrToStr6(lAddr);
      Check(lStr=CCases[i].Expected, 'NetAddrToStr6('+CCases[i].Input+') = '+CCases[i].Expected+', got '+lStr);
    end;
end;


{ Checks every pattern of zero and non-zero groups for a valid text form that parses back }
procedure TestAllZeroPatterns;

var
  lPattern, i, lRun, lLongest, lColons: Integer;
  lAddr, lBack: TIn6_Addr;
  lStr: string;

begin
  for lPattern:=0 to 255 do
    begin
      lRun:=0;
      lLongest:=0;
      for i:=0 to 7 do
        if (lPattern and (1 shl i))<>0 then
          begin
            lAddr.u6_addr16[i]:=htons($0a0b+i);
            lRun:=0;
          end
        else
          begin
            lAddr.u6_addr16[i]:=0;
            Inc(lRun);
            if lRun>lLongest then
              lLongest:=lRun;
          end;
      lStr:=HostAddrToStr6(lAddr);
      Check(Pos(':::', lStr)=0, 'No ::: in '+lStr);
      Check((Pos('::', lStr)=0) or (Pos('::', Copy(lStr, Pos('::', lStr)+2, MaxInt))=0), 'At most one :: in '+lStr);
      Check((Pos('::', lStr)>0)=(lLongest>=2), ':: used only for 2 or more zero groups in '+lStr);
      Check(Pos(':0a', ':'+lStr)=0, 'No leading zeros in '+lStr);
      Check(lStr=LowerCase(lStr), 'Lowercase hexadecimal in '+lStr);
      lColons:=0;
      for i:=1 to Length(lStr) do
        if lStr[i]=':' then
          Inc(lColons);
      Check((Pos('::', lStr)>0) or (lColons=7), 'Eight groups without :: in '+lStr);
      Check(TryStrToHostAddr6(lStr, lBack) and SameAddr(lAddr, lBack), lStr+' parses back to the same address');
    end;
end;


begin
  lErrors:=0;
  TestCases;
  TestAllZeroPatterns;
  if lErrors>0 then
    begin
      writeln(lErrors, ' errors');
      Halt(1);
    end;
  writeln('ok');
end.
