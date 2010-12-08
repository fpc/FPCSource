function NSSTR (inString: PChar): NSString;
begin
  Result := NSString(CFSTR(inString));
end;
