{ %target=linux }
{ cwstring cases a letter outside the Basic Multilingual Plane, a surrogate pair, as one
  code point; an unpaired surrogate stays as it is }
uses
  cwstring, sysutils;

const
  LC_ALL = 6;

function setlocale(category : longint; locale : pansichar) : pansichar; cdecl; external 'c' name 'setlocale';

procedure check(const name : string; const got, want : unicodestring; err : longint);
  begin
    if got<>want then
      begin
        writeln('Error ',err,': ',name,' gave ',length(got),' units');
        halt(err);
      end;
  end;

var
  lo, up : unicodestring;
begin
  { towupper and towlower follow the locale, and the runner's may be C }
  if setlocale(LC_ALL,'C.UTF-8')=nil then
    begin
      writeln('Error 20: locale C.UTF-8 is not available');
      halt(20);
    end;
  { U+10428 DESERET SMALL LETTER LONG I and U+10400 DESERET CAPITAL LETTER LONG I }
  lo:=#$d801#$dc28;
  up:=#$d801#$dc00;
  check('upper',UnicodeUpperCase('a'+lo+'b'+lo),'A'+up+'B'+up,1);
  check('lower',UnicodeLowerCase(up),lo,2);
  if not UnicodeSameText(lo,up) then
    begin
      writeln('Error 3: UnicodeSameText');
      halt(3);
    end;
  { unpaired surrogates }
  check('high at end',UnicodeUpperCase('a'#$d801),'A'#$d801,4);
  check('high before a letter',UnicodeUpperCase(#$d801'j'),#$d801'J',5);
  check('high before U+E000',UnicodeUpperCase(#$d801#$e000),#$d801#$e000,6);
  check('lone low',UnicodeUpperCase(#$dc28'a'),#$dc28'A',7);
  writeln('ok');
end.
