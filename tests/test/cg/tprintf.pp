{ %NOTE=This test requires a C library }

{$mode objfpc}


{$linklib c}

uses
  strings;

type
 THandle = longint;
const
  l : longint = 45;
  s : pchar = 'Enclosed text';
  d : double = 45.45;
  p : pchar = nil;
  has_errors : boolean = false;

procedure printf(const formatstr : pchar; const args : array of const);cdecl; external;
procedure sprintf(p : pchar;const formatstr : pchar; const args : array of const);cdecl; external;

begin
  getmem(p,500);

  Writeln('Testing C printf function called from FPC code');
//  printf('Simple test without arg'#10,[]);
  Writeln('Testing with single pchar argument');
  printf('Text containing "%s" text'#10,[s]);
  sprintf(p,'Text containing "%s" text'#10,[s]);
  if strpos(p,'g "Enclosed text" ')=nil then
    begin
      writeln('The output of sprintf for pchar is wrong:',p);
      has_errors:=true;
    end;

  Writeln('Testing with single longint argument');
  printf('Text containing longint : %d'#10,[l]);
  sprintf(p,'Text containing longint : %d'#10,[l]);
  if strpos(p,'longint : 45')=nil then
    begin
      writeln('The output of sprintf for longint is wrong:',p);
      has_errors:=true;
    end;

  Writeln('Testing with single double argument');
  printf('Text containing double : %f'#10,[d]);
  sprintf(p,'Text containing double : %f'#10,[d]);
  if strpos(p,' : 45.45')=nil then
    begin
      writeln('The output of sprintf for double is wrong:',p);
      has_errors:=true;
    end;

  if has_errors then
    halt(1);
end.
