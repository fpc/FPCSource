
program project1;
// Run the following to cause an access violation
//
// ./project1 'as.*0' 'ascii_lf1'
//

{$mode objfpc}{$H+}

uses
  regex,
  SysUtils;

var
  re : TRegexEngine;
  aErrorPos : integer;
  aErrorCode: TRegexError;
  MatchPos : integer;
  Offset : integer;
  s1,s2 : string;
begin
  s1:='as.*0';
  s2:='ascii_lf1';
  try
    WriteLn('Regex: Trim(s1) = >>'+Trim(s1)+'<<');
    WriteLn('Test: Trim(s2) = >>'+Trim(s2)+'<<');
    re := TRegexEngine.Create(Trim(s1));
    if re.Parse(aErrorPos,aErrorCode) then begin
      Offset := 1;
      if re.MatchString(s2,MatchPos,Offset) then begin
        WriteLn('Match');
      end else begin
        WriteLn('No Match');
      end;
    end else begin
      WriteLn('Parse Failed');
    end;
  except
    on E : Exception do begin
      WriteLn('Exception: '+E.Message);
    end;
  end;
end.
