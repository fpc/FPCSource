{$mode objfpc}
{$h+}
{$inline on}

var
  s: String;

  function TestWinCPToUTF8(const s: string): string;
  begin
    if pointer(s)=pointer(result) then
      halt(1);
    Result := s; // Result is now invalid
    SetCodePage(RawByteString(Result), CP_ACP, False);
  end;

  function Test: string; inline;
  var
    s: String;
  begin
    s := 'test';
    result:=s+'a';
    Result := TestWinCPToUTF8(Result);
  end;

begin
  s := Test;
end.
