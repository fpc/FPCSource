{$mode objfpc}
{$h+}
{$hints on}
{$warnings on}

uses
  StrUtils;

var
  exitCode: integer = 0;

procedure isWildTest(const testString, wildString: ansistring;
                     const ignoreCase, expectation: boolean;
                     const testnr: integer);

  begin
    if isWild(testString, wildString, ignoreCase) <> expectation then
      begin
      writeln('Testing strUtils/isWild: Test ', testnr, ' failed.');
      exitCode := 1;
      end;
  end; 

const
  CignoreCase = true;
  CfollowCase = false;

begin
  isWildTest('abcd', 'abcd', CignoreCase, true,  1);
  isWildTest('abcd', 'AbCd', CignoreCase, true,  2);
  isWildTest('abcd', 'AbCd', CfollowCase, false, 3);
  isWildTest('abcd', '*bCd', CignoreCase, true,  4);
  isWildTest('abcd', '*bCd', CfollowCase, false, 5);
  isWildTest('abcd', '?bCd', CignoreCase, true,  6);
  isWildTest('abcd', '?bCd', CfollowCase, false, 7);
  isWildTest('abcd', 'AbC*', CignoreCase, true,  8);
  isWildTest('abcd', 'AbC*', CfollowCase, false, 9);
  isWildTest('abcd', 'AbC?', CignoreCase, true,  10);
  isWildTest('abcd', 'AbC?', CfollowCase, false, 11);
  isWildTest('abcd', 'Ab*d', CignoreCase, true,  12);
  isWildTest('abcd', 'Ab*d', CfollowCase, false, 13);
  isWildTest('abcd', 'Ab?d', CignoreCase, true,  14);
  isWildTest('abcd', 'Ab?d', CfollowCase, false, 15);

  isWildTest('abcd', 'abcde', CignoreCase, false, 21);
  isWildTest('abcd', 'AbCde', CignoreCase, false, 22);
  isWildTest('abcd', 'AbCde', CfollowCase, false, 23);
  isWildTest('abcd', '*bCde', CignoreCase, false, 24);
  isWildTest('abcd', '*bCde', CfollowCase, false, 25);
  isWildTest('abcd', '?bCde', CignoreCase, false, 26);
  isWildTest('abcd', '?bCde', CfollowCase, false, 27);
  isWildTest('abcd', 'AbC*e', CignoreCase, false, 28);
  isWildTest('abcd', 'AbC*e', CfollowCase, false, 29);
  isWildTest('abcd', 'AbC?e', CignoreCase, false, 30);
  isWildTest('abcd', 'AbC?e', CfollowCase, false, 31);
  isWildTest('abcd', 'Ab*de', CignoreCase, false, 32);
  isWildTest('abcd', 'Ab*de', CfollowCase, false, 33);
  isWildTest('abcd', 'Ab?de', CignoreCase, false, 34);
  isWildTest('abcd', 'Ab?de', CfollowCase, false, 35);

  isWildTest('abcde', 'abcd', CignoreCase, false, 41);
  isWildTest('abcde', 'AbCd', CignoreCase, false, 42);
  isWildTest('abcde', 'AbCd', CfollowCase, false, 43);
  isWildTest('abcde', '*bCd', CignoreCase, false, 44);
  isWildTest('abcde', '*bCd', CfollowCase, false, 45);
  isWildTest('abcde', '?bCd', CignoreCase, false, 46);
  isWildTest('abcde', '?bCd', CfollowCase, false, 47);
  isWildTest('abcde', 'AbC*', CignoreCase, true,  48);
  isWildTest('abcde', 'AbC*', CfollowCase, false, 49);
  isWildTest('abcde', 'AbC?', CignoreCase, false, 50);
  isWildTest('abcde', 'AbC?', CfollowCase, false, 51);
  isWildTest('abcde', 'Ab*d', CignoreCase, false, 52);
  isWildTest('abcde', 'Ab*d', CfollowCase, false, 53);
  isWildTest('abcde', 'Ab?d', CignoreCase, false, 54);
  isWildTest('abcde', 'Ab?d', CfollowCase, false, 55);

  isWildTest('bcd', 'abcd', CignoreCase, false, 61);
  isWildTest('bcd', 'AbCd', CignoreCase, false, 62);
  isWildTest('bcd', 'AbCd', CfollowCase, false, 63);
  isWildTest('bcd', '*bCd', CignoreCase, true,  64);
  isWildTest('bcd', '*bCd', CfollowCase, false, 65);
  isWildTest('bcd', '?bCd', CignoreCase, false, 66);
  isWildTest('bcd', '?bCd', CfollowCase, false, 67);
  isWildTest('bcd', 'AbC*', CignoreCase, false, 68);
  isWildTest('bcd', 'AbC*', CfollowCase, false, 69);
  isWildTest('bcd', 'AbC?', CignoreCase, false, 70);
  isWildTest('bcd', 'AbC?', CfollowCase, false, 71);
  isWildTest('bcd', 'Ab*d', CignoreCase, false, 72);
  isWildTest('bcd', 'Ab*d', CfollowCase, false, 73);
  isWildTest('bcd', 'Ab?d', CignoreCase, false, 74);
  isWildTest('bcd', 'Ab?d', CfollowCase, false, 75);

  isWildTest('abcd', 'bcd', CignoreCase, false, 81);
  isWildTest('abcd', 'bCd', CignoreCase, false, 82);
  isWildTest('abcd', 'bCd', CfollowCase, false, 83);
  isWildTest('abcd', '*Cd', CignoreCase, true,  84);
  isWildTest('abcd', '*Cd', CfollowCase, false, 85);
  isWildTest('abcd', '?Cd', CignoreCase, false, 86);
  isWildTest('abcd', '?Cd', CfollowCase, false, 87);
  isWildTest('abcd', 'bC*', CignoreCase, false, 88);
  isWildTest('abcd', 'bC*', CfollowCase, false, 89);
  isWildTest('abcd', 'bC?', CignoreCase, false, 90);
  isWildTest('abcd', 'bC?', CfollowCase, false, 91);
  isWildTest('abcd', 'b*d', CignoreCase, false, 92);
  isWildTest('abcd', 'b*d', CfollowCase, false, 93);
  isWildTest('abcd', 'b?d', CignoreCase, false, 94);
  isWildTest('abcd', 'b?d', CfollowCase, false, 95);

  isWildTest('abcd', '*', CignoreCase, true,  101);
  isWildTest('abcd', '*', CfollowCase, true,  102);
  isWildTest('abcd', '???', CignoreCase, false, 103);
  isWildTest('abcd', '???', CfollowCase, false, 104);
  isWildTest('abcd', '????', CignoreCase, true,  105);
  isWildTest('abcd', '????', CfollowCase, true,  106);
  isWildTest('abcd', '?????', CignoreCase, false, 107);
  isWildTest('abcd', '?????', CfollowCase, false, 108);

{ Tests from bug report 25494 }
  isWildTest('CILA', '*a*b*c*d*e*', CignoreCase, false, 111);
  isWildTest('Nokia', '*a*b*c*d*e*', CignoreCase, false, 112);
  isWildTest('QTVideoCodec.vca', '*a*b*c*d*e*', CignoreCase, false, 113);
  isWildTest('Kenia', '*a*b*c*d*e*', CignoreCase, false, 114);
  isWildTest('hfghshshwywyw ww', '*a*b*c*d*e*', CignoreCase, false, 115);
  isWildTest('en-GB.pak', '??*??.pak', CignoreCase, true,  116);
  isWildTest('en-GB.pak', '??*??.pak', CfollowCase, true,  117);
  isWildTest('am.pak', '??*??.pak', CignoreCase, false, 118);
  isWildTest('am.pak', '??*??.pak', CfollowCase, false, 119);
  
  halt(exitCode);
end.
