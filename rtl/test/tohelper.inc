  Result:='';
  V:=value;
  if not AssertEquals('ToString',ValueAsString,V.ToString) then
    Exit;
  if not AssertEquals('ToBoolean',True,V.ToBoolean) then
    Exit;
  V:=0;
  if not AssertEquals('ToBoolean',False,V.ToBoolean) then
    Exit;
  V:=value;
  if not AssertEquals('ToHexString',ValueAshex,V.ToHexString) then
    Exit;
  if not AssertEquals('ToHexString',ValueAsHexDigString,V.ToHexString(ValueAsHexDig)) then
    Exit;
  if not AssertEquals('ToSingle',Single(Value+0.0),V.ToSingle,0.00001) then
    Exit;
  if not AssertEquals('ToDouble',Double(Value+0.0),V.ToDouble,0.00001) then
    Exit;
  if not AssertEquals('ToExtended',Extended(Value+0.0),V.ToExtended,0.00001) then
    Exit;
