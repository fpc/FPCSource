unit wasmlinkchange;

interface

uses Classes, SysUtils, wasmlink, wasmbin, lebutils;

procedure ProcessLinkingSection(st: TStream);

implementation

procedure ProcessLinkingSection(st: TStream);
var
  mt  : TLinkingMetadata;
  //en  : Int64;
  sub : TLinkinSubSection;
  cnt : LongWord;
  nx  : Int64;
  i   : integer;
  si  : TSymInfo;
  ofs : Int64;
begin
  //en := st.Position+secsize;
  ReadMetadata(st, mt);
  writeln('version: ', mt.version);
  //while st.Position<en do begin
    ReadLinkSubSect(st, sub);
    nx := st.Position+sub.length;

    writeln('subsec=',SubSecTypeToStr(sub.sectype),' ',sub.sectype);
    cnt := ReadU(st);
    writeln('- symbol table [count=', cnt,']');
    for i:=0 to cnt-1 do begin
      write('  - ',i,' ');

      ofs := st.Position;
      ReadSymInfo(st, si);
      //write(SymKindToStr(si.kind),' ',IntToHex(si.flags,8));
      //if si.hasSymName then write(' ',si.symname);
      //writeln;
      if si.flags and WASM_SYM_UNDEFINED = 0 then
        si.flags := si.flags or WASM_SYM_BINDING_LOCAL;

      st.Position := ofs;
      WriteSymInfo(st, si);

      //writeln(si.symname);
    end;

    st.Position:=nx;
  //end;
end;

end.
