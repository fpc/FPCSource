
procedure fpcxmlFree(mem: pointer); EXTDECL;
begin
  FreeMem(mem);
end;

function fpcxmlMalloc(size: csize_t): pointer; EXTDECL;
begin
  GetMem(Result, size);
end;

function fpcxmlRealloc(mem: pointer; size: csize_t): pointer; EXTDECL;
begin
  Result := mem;
  ReallocMem(Result, size);
end;

function fpcxmlStrdup(str: PAnsiChar): PAnsiChar; EXTDECL;
var
  L: SizeInt;
begin
  L := Length(str) + 1;
  Getmem(Result, L);
  if Result <> nil then
    Move(str^, Result^, L);
end;

procedure fpcxmlStructuredErrorHandler(userData: pointer; error: xmlErrorPtr); EXTDECL;
begin
  writeln('struct error');
end;

{$IFDEF NO_EXTERNAL_VARS}
function GetxmlMalloc: xmlMallocFunc; inline;
begin
  Result := varxmlMalloc^;
end;

procedure SetxmlMalloc(AValue: xmlMallocFunc); inline;
begin
  varxmlMalloc^ := AValue;
end;

function GetxmlMallocAtomic: xmlMallocFunc; inline;
begin
  Result := varxmlMallocAtomic^;
end;

procedure SetxmlMallocAtomic(AValue: xmlMallocFunc); inline;
begin
  varxmlMallocAtomic^ := AValue;
end;

function GetxmlRealloc: xmlReallocFunc; inline;
begin
  Result := varxmlRealloc^;
end;

procedure SetxmlRealloc(AValue: xmlReallocFunc); inline;
begin
  varxmlRealloc^ := AValue;
end;

function GetxmlFree: xmlFreeFunc; inline;
begin
  Result := varxmlFree^;
end;

procedure SetxmlFree(AValue: xmlFreeFunc); inline;
begin
  varxmlFree^ := AValue;
end;

function GetxmlMemStrdup: xmlStrdupFunc; inline;
begin
  Result := varxmlMemStrdup^;
end;

procedure SetxmlMemStrdup(AValue: xmlStrdupFunc); inline;
begin
  varxmlMemStrdup^ := AValue;
end;
{$ENDIF}

