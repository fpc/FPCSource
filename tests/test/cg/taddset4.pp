type
  TCompilerIntfFlag = (ifHasGuid,ifDispInterface,ifDispatch,ifHasStrGUID);
  TCompilerIntfFlags = set of TCompilerIntfFlag;

procedure t(l: longint);
begin
  writeln(hexstr(l,8));
  { exactly 3 bits must be set }
  l:=l and (l-1);
  if (l = 0) then
    halt(1);
  l:=l and (l-1);
  if (l = 0) then
    halt(2);
  l:=l and (l-1);
  if (l <> 0) then
    halt(3);
end;

var
  b:boolean;
begin
  b:=true;
  t({$ifdef CPU16}byte{$else}longint{$endif}([
      TCompilerIntfFlag(ord(ifHasGuid)*ord(b)),
      TCompilerIntfFlag(ord(ifHasStrGUID)*ord(b)),
      TCompilerIntfFlag(ord(ifDispInterface)*ord(b))
    ]));
end.
