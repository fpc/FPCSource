{ %NOTE=This test requires a C library }

{$mode objfpc}

uses
  strings;

{$ifdef win32}
{$linklib msvcrt}
procedure printf(const formatstr : pchar; const args : array of const);cdecl; external name 'printf';
procedure sprintf(p : pchar;const formatstr : pchar; const args : array of const);cdecl; external name 'sprintf';
{$else}
{$linklib c}
procedure printf(const formatstr : pchar; const args : array of const);cdecl; external;
procedure sprintf(p : pchar;const formatstr : pchar; const args : array of const);cdecl; external;
{$endif}



type
 THandle = longint;
const
  l : longint = 45;
  ll : int64 = 345;
  s : pchar = 'Enclosed text';
  s2 : pchar = 'next';
  si : single = 32.12;
  d : double = 45.45;
  e : extended = 74.74;
  p : pchar = nil;
  has_errors : boolean = false;

begin
  getmem(p,500);

  Writeln('Testing C printf function called from FPC code');
//  printf('Simple test without arg'#10,[]);
  Writeln('Testing with single pchar argument');
  printf('Text containing "%s" text'#10,[s]);
  sprintf(p,'Text containing "%s" text'#10,[s]);
  if strpos(p,'g "Enclosed text" ')=nil then
    begin
      writeln('The output of sprintf for pchar is wrong: ',p);
      has_errors:=true;
    end;

  Writeln('Testing with single longint argument');
  printf('Text containing longint: %d'#10,[l]);
  sprintf(p,'Text containing longint: %d'#10,[l]);
  if strpos(p,'longint: 45')=nil then
    begin
      writeln('The output of sprintf for longint is wrong: ',p);
      has_errors:=true;
    end;

  Writeln('Testing with single int64 argument');
  printf('Text containing int64: %lld'#10,[ll]);
  sprintf(p,'Text containing int64: %lld'#10,[ll]);
  if strpos(p,'int64: 345')=nil then
    begin
      writeln('The output of sprintf for int64 is wrong: ',p);
      has_errors:=true;
    end;

  Writeln('Testing with single single argument');
  printf('Text containing single: %f'#10,[si]);
  sprintf(p,'Text containing single: %f'#10,[si]);
  if strpos(p,'single: 32.1')=nil then
    begin
      writeln('The output of sprintf for double is wrong: ',p);
      has_errors:=true;
    end;

  Writeln('Testing with single double argument');
  printf('Text containing double: %f'#10,[d]);
  sprintf(p,'Text containing double: %f'#10,[d]);
  if strpos(p,'double: 45.4')=nil then
    begin
      writeln('The output of sprintf for double is wrong: ',p);
      has_errors:=true;
    end;

  printf('Text containing long double: %f'#10,[e]);
  sprintf(p,'Text containing long double: %f'#10,[e]);
  if strpos(p,'long double: 74.7')=nil then
    begin
      writeln('The output of sprintf for long double is wrong:',p);
      has_errors:=true;
    end;

  Writeln('Testing with combined pchar argument');
  printf('Text containing "%s" and "%s" text'#10,[s,s2]);
  sprintf(p,'Text containing "%s" and "%s" text'#10,[s,s2]);
  if strpos(p,'g "Enclosed text" and "next"')=nil then
    begin
      writeln('The output of sprintf for two pchars is wrong: ',p);
      has_errors:=true;
    end;

  Writeln('Testing with single longint argument and pchar');
  printf('Text containing longint: %d"%s"'#10,[l,s2]);
  sprintf(p,'Text containing longint: %d"%s"'#10,[l,s2]);
  if strpos(p,'longint: 45"next"')=nil then
    begin
      writeln('The output of sprintf for longint is wrong: ',p);
      has_errors:=true;
    end;

  Writeln('Testing with single int64 argument and pchar');
  printf('Text containing int64: %lld"%s"'#10,[ll,s2]);
  sprintf(p,'Text containing int64: %lld"%s"'#10,[ll,s2]);
  if strpos(p,'int64: 345"next"')=nil then
    begin
      writeln('The output of sprintf for int64 is wrong: ',p);
      has_errors:=true;
    end;

  Writeln('Testing with single single argument');
  printf('Text containing single: %f"%s"'#10,[si,s2]);
  sprintf(p,'Text containing single: %f"%s"'#10,[si,s2]);
  if (strpos(p,'single: 32.1')=nil) or
     (strpos(p,'"next"')=nil) then
    begin
      writeln('The output of sprintf for double is wrong: ',p);
      has_errors:=true;
    end;

  Writeln('Testing with single double argument');
  printf('Text containing double: %f"%s"'#10,[d,s2]);
  sprintf(p,'Text containing double: %f"%s"'#10,[d,s2]);
  if (strpos(p,'double: 45.4')=nil) or
     (strpos(p,'"next"')=nil) then
    begin
      writeln('The output of sprintf for double is wrong: ',p);
      has_errors:=true;
    end;

  printf('Text containing long double: %f"%s"'#10,[e,s2]);
  sprintf(p,'Text containing long double: %f"%s"'#10,[e,s2]);
  if (strpos(p,'long double: 74.7')=nil) or
     (strpos(p,'"next"')=nil) then
    begin
      writeln('The output of sprintf for long double is wrong:',p);
      has_errors:=true;
    end;

  if has_errors then
    halt(1);
end.
