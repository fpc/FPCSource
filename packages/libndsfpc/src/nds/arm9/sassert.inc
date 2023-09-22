{$ifdef NDS_INTERFACE}
procedure __sassert(const fileName: PAnsiChar; lineNumber: cint; const conditionString: PAnsiChar; const amessage: PAnsiChar); cdecl; external;
procedure sassert(e: boolean; msg: ansistring); inline;
procedure AssertErrorHandler(const Msg, Filename:shortstring; LineNumber: Integer; ErrorAddr: Pointer);

{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
procedure AssertErrorHandler(const Msg, Filename: shortstring; LineNumber: Integer; ErrorAddr: Pointer);
begin
  //ErrorAddr is address Assert will return to
  //(i.e. instruction following call to Assert).
  //Subtract 1 from it to ensure it refers to part
  //of the Assert call, making it more appropriate
//  DebugMsg('%s (%s, line %d, address $%x)',
//    [Msg, Filename, LineNumber, Pred(Integer(ErrorAddr))]);
  __sassert(PAnsiChar(@Filename[1]), LineNumber, nil, PAnsiChar(@Msg[1]));
end;


procedure sassert1(e: boolean; msg: PAnsiChar); inline;
begin
  if e then 
    exit 
  else
    //__sassert(__FILE__, __LINE__, nil, msg));
    __sassert('file', 0, nil, msg);
end;

procedure sassert2(e: boolean; msg: PAnsiChar); inline;
var
  i: integer = 0;
  s: ansistring;
begin
  if e then 
    exit 
  else
  begin
    s := {$include %LINE%};
    Str(i, s);
    __sassert({$include %FILE%}, i, nil, msg);
  end;
end;

procedure sassert(e: boolean; msg: ansistring); inline;
begin
  assert(e, msg);
end;


{$endif NDS_IMPLEMENTATION}
