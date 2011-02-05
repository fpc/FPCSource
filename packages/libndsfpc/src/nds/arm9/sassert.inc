{$ifdef NDS_INTERFACE}
procedure __sassert(const fileName: pchar; lineNumber: cint; const conditionString: pchar; const amessage: pchar); cdecl; external;
procedure sassert(e: boolean; msg: string); inline;
procedure AssertErrorHandler(const Msg, Filename: string; LineNumber: Integer; ErrorAddr: Pointer);

{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
procedure AssertErrorHandler(const Msg, Filename: string; LineNumber: Integer; ErrorAddr: Pointer);
begin
  //ErrorAddr is address Assert will return to
  //(i.e. instruction following call to Assert).
  //Subtract 1 from it to ensure it refers to part
  //of the Assert call, making it more appropriate
//  DebugMsg('%s (%s, line %d, address $%x)',
//    [Msg, Filename, LineNumber, Pred(Integer(ErrorAddr))]);
  __sassert(pchar(@Filename[1]), LineNumber, nil, pchar(@Msg[1]));
end;


procedure sassert1(e: boolean; msg: pchar); inline;
begin
  if e then 
    exit 
  else
    //__sassert(__FILE__, __LINE__, nil, msg));
    __sassert('file', 0, nil, msg);
end;

procedure sassert2(e: boolean; msg: pchar); inline;
var
  i: integer;
  s: string;
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

procedure sassert(e: boolean; msg: string); inline;
begin
  assert(e, msg);
end;


{$endif NDS_IMPLEMENTATION}
