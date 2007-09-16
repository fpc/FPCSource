
{ Included by several source with different
  definitions of the type
  IntegerType
  to check that the test is working for
  all basic integer types }


procedure TestVal(comment,s : string; ExpectedRes : ValTestType; expected : IntegerType);
var
  i : IntegerType;
  err,err1 : word;
  OK : boolean;
begin
  OK:=false;
  if not silent and (Comment<>'') then
    Writeln(Comment);
  Val(s,i,err);
  if ExpectedRes=ValShouldFail then
    begin
      if err=0 then
        begin
          if not silent or not HasErrors then
            Writeln('Error: string ',Display(s),
              ' is a valid input for val function');
          HasErrors:=true;
        end
      else
        begin
          OK:=true;
          if not silent then
            Writeln('Correct: string ',Display(s),
              ' is a not valid input for val function');
        end;
    end
  else if ExpectedRes=ValShouldSucceed then
    begin
      if err=0 then
        begin
          OK:=true;
          if not silent then
            Writeln('Correct: string ',Display(s),
              ' is a valid input for val function');
        end
      else
        begin
          if not silent or not HasErrors then
            Writeln('Error: string ',Display(s),
              ' is a not valid input for val function',
              ' error pos=',err);
          HasErrors:=true;
        end;
    end
  else if ExpectedRes=ValShouldSucceedAfterRemovingTrail then
    begin
      if err=0 then
        begin
          if not silent or not HasErrors then
            Writeln('Error: string ',Display(s),
              ' is a valid input for val function');
          HasErrors:=true;
        end
      else
        begin
          err1:=err;
          Val(Copy(s,1,err1-1),i,err);
          if err=0 then
            begin
              OK:=true;
              if not silent then
                Writeln('Correct: string ',Display(s),
                  ' is a valid input for val function up to position ',err1);
            end
          else
            begin
              if not silent or not HasErrors then
                Writeln('Error: string ',Display(Copy(s,1,err1-1)),
                  ' is a not valid input for val function',
                  ' error pos=',err);
              HasErrors:=true;
            end;
        end;
    end;
  if (err=0) and CheckVal and (i<>expected) then
    begin
      OK:=false;
      if not silent or not HasErrors then
        Writeln('Error: string ',Display(s),
          ' value is ',i,' <> ',expected);
      HasErrors:=true;
    end;
  if OK then
    inc(SuccessCount)
  else
    inc(FailCount);
end;

Procedure TestBase(Const Prefix : string;ValidChars : TCharSet);
var
  i,j : longint;
  st : string;
begin
  CheckVal:=false;
  Silent:=true;
  for i:=0 to 255 do
    begin
      st:=prefix+chr(i);
      if chr(i) in ValidChars then
        TestVal('',st,ValShouldSucceed,0)
      else
        TestVal('',st,ValShouldFail,0);
    end;
  for i:=0 to 255 do
    for j:=0 to 255 do
      begin
        st:=prefix+chr(i)+chr(j);
        if (chr(i) in ValidChars) and
           (chr(j) in ValidChars) then
          TestVal('',st,ValShouldSucceed,0)
        else
          begin
            if ((prefix<>'') or
               (not (chr(i) in SpecialCharsFirst))) and
                not (chr(j) in SpecialCharsSecond) then
              TestVal('',st,ValShouldFail,0);
          end;
      end;
end;


Function TestAll : boolean;

var
  S : string;
begin
  TestVal('Testing empty string','',ValShouldFail,0);
  TestVal('Testing string with #0',#0,ValShouldFail,0);
  TestVal('Testing string with base prefix and no value','0x',ValShouldFail,0);
  TestVal('Testing string with base prefix and no value','x',ValShouldFail,0);
  TestVal('Testing string with base prefix and no value','X',ValShouldFail,0);
  TestVal('Testing string with base prefix and no value','$',ValShouldFail,0);
  TestVal('Testing string with base prefix and no value','%',ValShouldFail,0);
  TestVal('Testing string with base prefix and no value','&',ValShouldFail,0);
  TestVal('Testing string with base prefix and #0','0x'#0,ValShouldFail,0);
  TestVal('Testing normal ''''0'''' string','0',ValShouldSucceed,0);
  TestVal('Testing leading space',' 0',ValShouldSucceed,0);
  TestVal('Testing leading 2 spaces','  0',ValShouldSucceed,0);
  TestVal('Testing leading 2 tabs',#9#9'0',ValShouldSucceed,0);
  TestVal('Testing leading 3 spaces','   0',ValShouldSucceed,0);
  TestVal('Testing leading 3 tabs',#9#9#9'0',ValShouldSucceed,0);
  TestVal('Testing leading space/tab combination',#9' 0',ValShouldSucceed,0);
  TestVal('Testing leading space/tab combination',' '#9'0',ValShouldSucceed,0);
  TestVal('Testing leading space/tab combination',' '#9' 0',ValShouldSucceed,0);
  TestVal('Testing leading space/tab combination',#9' '#9' 0',ValShouldSucceed,0);
  TestVal('Testing #0 following normal ''''0''','0'#0,ValShouldSucceed,0);
  TestVal('Testing leading space with trailing #0',' 0'#0,ValShouldSucceed,0);
  TestVal('Testing leading 2 spaces with trailing #0','  0'#0,ValShouldSucceed,0);
  TestVal('Testing leading 2 tabs with trailing #0',#9#9'0'#0,ValShouldSucceed,0);
  TestVal('Testing leading 3 spaces with trailing #0','   0'#0,ValShouldSucceed,0);
  TestVal('Testing leading 3 tabs with trailing #0',#9#9#9'0'#0,ValShouldSucceed,0);
  TestVal('Testing leading space/tab combination with trailing #0',#9' 0'#0,ValShouldSucceed,0);
  TestVal('Testing leading space/tab combination with trailing #0',' '#9'0'#0,ValShouldSucceed,0);
  TestVal('Testing leading space/tab combination with trailing #0',' '#9' 0'#0,ValShouldSucceed,0);
  TestVal('Testing leading space/tab combination with trailing #0',#9' '#9' 0'#0,ValShouldSucceed,0);
  TestVal('Testing trailing space','0 ',ValShouldSucceedAfterRemovingTrail,0);
  TestVal('Testing trailing 2 spaces','0  ',ValShouldSucceedAfterRemovingTrail,0);
  TestVal('Testing trailing 2 tabs','0'#9#9,ValShouldSucceedAfterRemovingTrail,0);
  TestVal('Testing trailing 3 spaces','0   ',ValShouldSucceedAfterRemovingTrail,0);
  TestVal('Testing trailing 3 tabs','0'#9#9#9,ValShouldSucceedAfterRemovingTrail,0);
  TestVal('Testing trailing space/tab combination','0'#9' ',ValShouldSucceedAfterRemovingTrail,0);
  TestVal('Testing trailing space/tab combination','0 '#9,ValShouldSucceedAfterRemovingTrail,0);
  TestVal('Testing trailing space/tab combination','0 '#9' ',ValShouldSucceedAfterRemovingTrail,0);
  TestVal('Testing trailing space/tab combination','0'#9' '#9' ',ValShouldSucceedAfterRemovingTrail,0);
  TestVal('Testing several zeroes',' 00'#0,ValShouldSucceed,0);
  TestVal('Testing normal zero','0',ValShouldSucceed,0);
  TestVal('Testing several zeroes','00',ValShouldSucceed,0);
  TestVal('Testing normal zero with leading space',' 0',ValShouldSucceed,0);
  TestVal('Testing several zeroes with leading space',' 00',ValShouldSucceed,0);
  TestVal('Testing string with base prefix and zero','0x0',ValShouldSucceed,0);
  TestVal('Testing string with base prefix and zero','x0',ValShouldSucceed,0);
  TestVal('Testing string with base prefix and zero','X0',ValShouldSucceed,0);
  TestVal('Testing string with base prefix and zero','$0',ValShouldSucceed,0);
  TestVal('Testing string with base prefix and zero','%0',ValShouldSucceed,0);
  TestVal('Testing string with base prefix and zero','&0',ValShouldSucceed,0);
  TestVal('Testing string with base prefix and one','0x1',ValShouldSucceed,1);
  TestVal('Testing string with base prefix and one','x1',ValShouldSucceed,1);
  TestVal('Testing string with base prefix and one','X1',ValShouldSucceed,1);
  TestVal('Testing string with base prefix and one','$1',ValShouldSucceed,1);
  TestVal('Testing string with base prefix and one','%1',ValShouldSucceed,1);
  TestVal('Testing string with base prefix and one','&1',ValShouldSucceed,1);
  TestVal('Testing string with base prefix and two','0x2',ValShouldSucceed,2);
  TestVal('Testing string with base prefix and two','x2',ValShouldSucceed,2);
  TestVal('Testing string with base prefix and two','X2',ValShouldSucceed,2);
  TestVal('Testing string with base prefix and two','$2',ValShouldSucceed,2);
  TestVal('Testing string with base prefix and two','%2',ValShouldFail,0);
  TestVal('Testing string with base prefix and two','&2',ValShouldSucceed,2);
  TestVal('Testing string with base prefix and seven','0x7',ValShouldSucceed,7);
  TestVal('Testing string with base prefix and seven','x7',ValShouldSucceed,7);
  TestVal('Testing string with base prefix and seven','X7',ValShouldSucceed,7);
  TestVal('Testing string with base prefix and seven','$7',ValShouldSucceed,7);
  TestVal('Testing string with base prefix and seven','%7',ValShouldFail,0);
  TestVal('Testing string with base prefix and seven','&7',ValShouldSucceed,7);
  TestVal('Testing string with base prefix and eight','0x8',ValShouldSucceed,8);
  TestVal('Testing string with base prefix and eight','x8',ValShouldSucceed,8);
  TestVal('Testing string with base prefix and eight','X8',ValShouldSucceed,8);
  TestVal('Testing string with base prefix and eight','$8',ValShouldSucceed,8);
  TestVal('Testing string with base prefix and eight','%8',ValShouldFail,0);
  TestVal('Testing string with base prefix and eight','&8',ValShouldFail,0);
  TestVal('Testing string with base prefix and nine','0x9',ValShouldSucceed,9);
  TestVal('Testing string with base prefix and nine','x9',ValShouldSucceed,9);
  TestVal('Testing string with base prefix and nine','X9',ValShouldSucceed,9);
  TestVal('Testing string with base prefix and nine','$9',ValShouldSucceed,9);
  TestVal('Testing string with base prefix and nine','%9',ValShouldFail,0);
  TestVal('Testing string with base prefix and nine','&9',ValShouldFail,0);
  TestVal('Testing string with base prefix and "a"','0xa',ValShouldSucceed,10);
  TestVal('Testing string with base prefix and "a"','xa',ValShouldSucceed,10);
  TestVal('Testing string with base prefix and "a"','Xa',ValShouldSucceed,10);
  TestVal('Testing string with base prefix and "a"','$a',ValShouldSucceed,10);
  TestVal('Testing string with base prefix and "a"','%a',ValShouldFail,0);
  TestVal('Testing string with base prefix and "a"','&a',ValShouldFail,0);
  TestVal('Testing string with base prefix and "A"','0xA',ValShouldSucceed,10);
  TestVal('Testing string with base prefix and "A"','xA',ValShouldSucceed,10);
  TestVal('Testing string with base prefix and "A"','XA',ValShouldSucceed,10);
  TestVal('Testing string with base prefix and "A"','$A',ValShouldSucceed,10);
  TestVal('Testing string with base prefix and "A"','%A',ValShouldFail,0);
  TestVal('Testing string with base prefix and "A"','&A',ValShouldFail,0);
  TestVal('Testing string with base prefix and "f"','0xf',ValShouldSucceed,15);
  TestVal('Testing string with base prefix and "f"','xf',ValShouldSucceed,15);
  TestVal('Testing string with base prefix and "f"','Xf',ValShouldSucceed,15);
  TestVal('Testing string with base prefix and "f"','$f',ValShouldSucceed,15);
  TestVal('Testing string with base prefix and "f"','%f',ValShouldFail,0);
  TestVal('Testing string with base prefix and "f"','&f',ValShouldFail,0);
  TestVal('Testing string with base prefix and "F"','0xF',ValShouldSucceed,15);
  TestVal('Testing string with base prefix and "F"','xF',ValShouldSucceed,15);
  TestVal('Testing string with base prefix and "F"','XF',ValShouldSucceed,15);
  TestVal('Testing string with base prefix and "F"','$F',ValShouldSucceed,15);
  TestVal('Testing string with base prefix and "F"','%F',ValShouldFail,0);
  TestVal('Testing string with base prefix and "F"','&F',ValShouldFail,0);

//  TestVal('Testing -zero','-0',ValShouldSucceed,0);
  TestVal('Testing +zero','+0',ValShouldSucceed,0);
  TestVal('Testing - zero','- 0',ValShouldFail,0);
  TestVal('Testing + zero','+ 0',ValShouldFail,0);
  TestVal('Testing --zero','--0',ValShouldFail,0);
  TestVal('Testing ++zero','++0',ValShouldFail,0);
  TestVal('Testing -+zero','-+0',ValShouldFail,0);

  TestBase('%', ValidNumeralsBase2);
  TestBase('&', ValidNumeralsBase8);
  TestBase('', ValidNumeralsBase10);
  TestBase('0x', ValidNumeralsBase16);

  if HasErrors then
    begin
      Writeln(FailCount,' tests failed over ',SuccessCount+FailCount);
    end
  else
    Writeln('All tests succeeded count=',SuccessCount);
  TestAll:=HasErrors;

end;

