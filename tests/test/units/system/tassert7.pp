program tassert7;
{$C+}
{$mode objfpc}

uses sysutils;

procedure Success;
 Begin
   WriteLn('Success!');
   halt;
 end;

Begin
  Write('Try..catch of assertion...');
  try
    assert(false);
  except
    on EAssertionFailed do Success;
  end;
  WriteLn('Failed!');
  Halt(1);
end.
