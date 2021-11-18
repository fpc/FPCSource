{*******************************************************************}
{ Tests overflow checks when PostPeepholeOptADDSUB has taken effect }
{ when adding or subtracting 128 to a variable                      }
{*******************************************************************}

{$ifdef fpc}
{$mode objfpc}
{$endif}
program toverflow;
uses
  SysUtils;
  
  function TestOverflow32(Initial: LongWord; Subtract, OverflowExpected: Boolean): Boolean;
    var
      Output: LongWord;
    begin
      Result := False;
      if Subtract then
        Write('Testing 32-bit subtraction of 128 from ', Initial, '... ')
      else
        Write('Testing 32-bit addition of 128 to ', Initial, '... ');
    
      try
        if Subtract then
          Output := Initial - $80
        else
          Output := Initial + $80;
      
        Write('no exception... ');
        
        if OverflowExpected then
          begin
            WriteLn('FAIL: Overflow not triggered then it should have');
            Exit(True);
          end
        else if Subtract then
          begin
            if Output + $80 <> Initial then
              begin
                WriteLn('FAIL: Result of ', Output, ' was incorrect');
                Exit(True);
              end;
          end
        else
          begin
            if Output - $80 <> Initial then
              begin
                WriteLn('FAIL: Result of ', Output, ' was incorrect');
                Exit(True);
              end;
          end
          
      except
        on E: ERangeError do
          if not OverflowExpected then 
            begin
              WriteLn('FAIL: Range error triggered when it shouldn''t have');
              Exit(True);
            end
          else
            Write('ERangeError triggered... ');

        on E: EIntOverflow do
          if not OverflowExpected then 
            begin
              WriteLn('FAIL: Overflow triggered when it shouldn''t have');
              Exit(True);
            end
          else
            Write('EIntOverflow triggered... ');

        on E: Exception do
          begin
            WriteLn('FAIL: Unexpected exception ' + E.ClassName + ': ' + E.Message);
            Exit(True);
          end;
      end;
      
      WriteLn('Pass');
    end;
  
  function TestOverflow64(Initial: QWord; Subtract, OverflowExpected: Boolean): Boolean;
    var
      Output: QWord;
    begin
      Result := False;
      if Subtract then
        Write('Testing 64-bit subtraction of 128 from ', Initial, '... ')
      else
        Write('Testing 64-bit addition of 128 to ', Initial, '... ');
    
      try
        if Subtract then
          Output := Initial - $80
        else
          Output := Initial + $80;
          
        Write('no exception... ');
      
        if OverflowExpected then
          begin
            WriteLn('FAIL: Overflow not triggered then it should have');
            Exit(True);
          end
        else if Subtract then
          begin
            if Output + $80 <> Initial then
              begin
                WriteLn('FAIL: Result of ', Output, ' was incorrect');
                Exit(True);
              end;
          end
        else
          begin
            if Output - $80 <> Initial then
              begin
                WriteLn('FAIL: Result of ', Output, ' was incorrect');
                Exit(True);
              end;
          end
          
      except
        on E: ERangeError do
          if not OverflowExpected then 
            begin
              WriteLn('FAIL: Range error triggered when it shouldn''t have');
              Exit(True);
            end
          else
            Write('ERangeError triggered... ');

        on E: EIntOverflow do
          if not OverflowExpected then 
            begin
              WriteLn('FAIL: Overflow triggered when it shouldn''t have');
              Exit(True);
            end
          else
            Write('EIntOverflow triggered... ');

        on E: Exception do
          begin
            WriteLn('FAIL: Unexpected exception ' + E.ClassName + ': ' + E.Message);
            Exit(True);
          end;
      end;
      
      WriteLn('Pass');
    end;

var
  Fail: Boolean = False;
begin
  { 32-bit add }
  Fail := TestOverflow32($FFFFFF7F, False, False) or Fail;
  Fail := TestOverflow32($FFFFFF80, False, True) or Fail; { Result is zero and overflows }
  Fail := TestOverflow32($FFFFFF81, False, True) or Fail;
  Fail := TestOverflow32($FFFFFFFF, False, True) or Fail;
  
  { 32-bit subtract }
  Fail := TestOverflow32($81, True, False) or Fail;
  Fail := TestOverflow32($80, True, False) or Fail; { Result is zero but doesn't overflow }
  Fail := TestOverflow32($7F, True, True) or Fail;
  Fail := TestOverflow32($0, True, True) or Fail;  

  { 64-bit add }
  Fail := TestOverflow64(QWord($FFFFFFFFFFFFFF7F), False, False) or Fail;
  Fail := TestOverflow64(QWord($FFFFFFFFFFFFFF80), False, True) or Fail; { Result is zero and overflows }
  Fail := TestOverflow64(QWord($FFFFFFFFFFFFFF81), False, True) or Fail;
  Fail := TestOverflow64(QWord($FFFFFFFFFFFFFFFF), False, True) or Fail;
  
  { 64-bit subtract }
  Fail := TestOverflow64($81, True, False) or Fail;
  Fail := TestOverflow64($80, True, False) or Fail; { Result is zero but doesn't overflow }
  Fail := TestOverflow64($7F, True, True) or Fail;
  Fail := TestOverflow64($0, True, True) or Fail;  
  
  ExitCode := LongInt(Fail);
  if not Fail then
    WriteLn('ok');
end.