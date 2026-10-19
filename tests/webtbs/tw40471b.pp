{ %needlibrary }
{ %target=linux }
{ %opt=-gl -O- }
{ %delfiles=tw40471a }

{ Looks up line info of a library address and of its library-relative offset }
program tw40471b;

uses
  dl;

function LabelAddress: CodePointer; cdecl; external 'tw40471a';

var
  lAddr, lRelAddr: codeptruint;
  lInfo: dl_info;
  lFunc, lSource: shortstring;
  lLine: longint;

begin
  lAddr:=codeptruint(LabelAddress);
  FillChar(lInfo, SizeOf(lInfo), 0);
  if dladdr(CodePointer(lAddr), @lInfo)=0 then
    begin
      writeln('dladdr failed');
      halt(1);
    end;
  lRelAddr:=lAddr-codeptruint(lInfo.dli_fbase);
  writeln('Library address $', hexstr(lAddr, SizeOf(lAddr)*2), ', relative $', hexstr(lRelAddr, SizeOf(lRelAddr)*2));

  if not GetLineInfo(lAddr, lFunc, lSource, lLine) then
    begin
      writeln('GetLineInfo of the library address failed');
      halt(2);
    end;
  writeln('Library address: func=', lFunc, ' source=', lSource, ' line=', lLine);
  if pos('tw40471a.pp', lSource)=0 then
    begin
      writeln('Library address must be found in tw40471a.pp');
      halt(3);
    end;

  if GetLineInfo(lRelAddr, lFunc, lSource, lLine) then
    writeln('Relative address: func=', lFunc, ' source=', lSource, ' line=', lLine);
  if pos('tw40471a.pp', lSource)<>0 then
    begin
      writeln('Relative address must not return the cached info of the library address');
      halt(4);
    end;
  writeln('ok');
end.
