function Iconvert(S: string; var Res: string; FromEncoding, ToEncoding: string): cint;
var
  InLen, OutLen, Offset: size_t;
  Src, Dst: pchar;
  H: iconv_t;
  lerr: cint;
  iconvres: size_t;
begin
  H := iconv_open(PChar(ToEncoding), PChar(FromEncoding));
  if not assigned(H) then
  begin
    Res := S;
    exit(-1);
  end;

  try
    SetLength(Res, Length(S));
    InLen := Length(S);
    OutLen := Length(Res);
    Src := PChar(S);
    Dst := PChar(Res);

    while InLen > 0 do
    begin
      iconvres := iconv(H, @Src, @InLen, @Dst, @OutLen);
      if iconvres = size_t(-1) then
      begin
        lerr := cerrno;
        if lerr = ESysEILSEQ then // unknown char, skip
          begin
            Dst^ := Src^;
            Inc(Src);
            Inc(Dst);
            Dec(InLen);
            Dec(OutLen);
          end
        else
          if lerr = ESysE2BIG then
            begin
              Offset := Dst - PChar(Res);
              SetLength(Res, Length(Res)+InLen*2+5); // 5 is minimally one utf-8 char
              Dst := PChar(Res) + Offset;
              OutLen := Length(Res) - Offset;
            end
          else
            exit(-1)
      end;
    end;

    // iconv has a buffer that needs flushing, specially if the last char is not #0
    iconv(H, nil, nil, @Dst, @Outlen);

    // trim output buffer
    SetLength(Res, Length(Res) - Outlen);
  finally
    iconv_close(H);
  end;

  Result := 0;
end;
