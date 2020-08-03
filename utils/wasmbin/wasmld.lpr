program wasmld;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  { you can add units after this }
  Classes, SysUtils, wasmbin, lebutils, wasmbindebug, wasmlink;

function ReadStream(st: TStream): Boolean;
var
  dw  : LongWord;
  ofs : int64;
  sc  : TSection;
  ps  : int64;
  nm  : string;
begin
  dw := st.ReadDWord;
  Result := dw = WasmId_Int;
  if not Result then begin
    writeln('not a wasm file');
    Exit;
  end;
  dw := st.ReadDWord;
  writeln('version: ', dw);
  while st.Position<st.Size do begin
    ofs := st.Position;
    sc.id := st.ReadByte;
    sc.Size := ReadU(st);
    writeln(ofs,': id=', sc.id,'(', SectionIdToStr(sc.id),') sz=', sc.size);

    ps := st.Position+sc.size;
    if sc.id=0 then begin
      nm := GetName(st);
      writeln(nm);
      if nm = SectionName_Linking then
        DumpLinking(st, sc.size - (st.Position - ofs));
    end;
    //if sc.id= 1 then DumpTypes(st);

    if st.Position <> ps then
    begin
      //writeln('adjust stream targ=',ps,' actual: ', st.position);
      st.Position := ps;
    end;
  end;

end;

var
  fs :TFileStream;
begin
  if ParamCount=0 then begin
    writeln('please sepcify .wasm file');
    exit;
  end;
  fs := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyNone);
  try
    ReadStream(fs);
  finally
    fs.Free;
  end;
end.

