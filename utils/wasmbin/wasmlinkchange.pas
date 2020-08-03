unit wasmlinkchange;

interface

uses Classes, SysUtils, wasmlink, wasmbin, lebutils;

type
  TSymbolType = (
    st_Nochange, st_Hidden, st_Weak
  );

  TSymbolConfigure = class(TObject)
    symname  : string;
    needtype : TSymbolType;
  end;

procedure ReadSymbolsConf(const fn: string; dst: TStrings);
procedure ReadSymbolsConf(src: TStream; dst: TStrings);

procedure ProcessLinkingSection(st: TStream; syms: TStrings);

implementation

procedure ReadSymbolsConf(const fn: string; dst: TStrings);
var
  fs: TFileStream;
begin
  fs:=TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    ReadSymbolsConf(fs, dst);
  finally
    fs.Free;
  end;
end;

function StrToSymType(const s: string): TSymbolType;
begin
  if length(s)=0 then
    Result:=st_Nochange
  else
  case upCase(s[1]) of
    'H','L': Result:=st_Hidden;
    'W': Result:=st_Weak;
  else
    Result:=st_Nochange;
  end;
end;


procedure ReadSymbolsConf(src: TStream; dst: TStrings);
begin
  dst.LoadFromStream(src);
end;

procedure ProcessLinkingSection(st: TStream; syms: TStrings);
var
  mt  : TLinkingMetadata;
  //en  : Int64;
  sub : TLinkinSubSection;
  cnt : LongWord;
  nx  : Int64;
  i   : integer;
  si  : TSymInfo;
  ofs : Int64;
  v   : string;
  tt  : TSymbolType;
  fl  : LongWord;
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

      if si.hasSymName then begin
        v := syms.Values[si.symname];
        tt := StrToSymType(v);

        fl := si.flags;
        case tt of
          st_Hidden:
            si.flags := (si.flags or WASM_SYM_BINDING_LOCAL) and (not WASM_SYM_BINDING_WEAK) and (not WASM_SYM_UNDEFINED);
          st_Weak:
            si.flags := (si.flags or WASM_SYM_BINDING_WEAK) and (not WASM_SYM_BINDING_LOCAL)
        end;

        if fl <> si.flags then begin
          st.Position := ofs;
          WriteSymInfo(st, si);
        end;
      end;

      //writeln(si.symname);
    end;

    st.Position:=nx;
  //end;
end;

end.
