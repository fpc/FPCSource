{$mode objfpc}
uses Sysutils;

procedure store (Handle: Pointer; const FileName: PChar; ErrorStruct: Pointer); cdecl; alias:'store';

  procedure yes (const aa: AnsiString);
  begin
    writeln(aa);
  end;

begin
  try
    yes (FileName);

  except
    on e: Exception do
      begin
      end;
  end;
end;

procedure extstore(h:pointer;filename:pchar;errorstr:pointer);external name 'store';


begin
  extstore(nil,'test',nil);
end.
