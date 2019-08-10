{$IF defined(WINDOWS)}
    type
      isoLPWStr = PWideChar;
      isoWinBool = LongBool;
      TSysCharSet = set of AnsiChar;

    function GetEnvironmentStringsW: isoLPWStr; stdcall; external 'kernel32' name 'GetEnvironmentStringsW';
    function FreeEnvironmentStringsW(_para1 : isoLPWStr): isoWinBool; stdcall; external 'kernel32' name 'FreeEnvironmentStringsW';

    function StrLen(p : PWideChar): sizeint; external name 'FPC_PWIDECHAR_LENGTH'; overload;

    {$push}
    {$checkpointer off}

    function CharInSet(Ch : WideChar; const CSet : TSysCharSet): Boolean;
    begin
      CharInSet := (Ch <= #$FF) and (AnsiChar(byte(Ch)) in CSet);
    end;

    function InternalChangeCase(const S : UnicodeString; const Chars: TSysCharSet; const Adjustment: Longint): UnicodeString;
      var
        i : Integer;
        p : PWideChar;
        unique : Boolean;
      begin
        InternalChangeCase := S;
        if InternalChangeCase = '' then
          exit;
        unique := false;
        p := PWideChar(InternalChangeCase);
        for i := 1 to Length(InternalChangeCase) do
        begin
          if CharInSet(p^, Chars) then
          begin
            if not unique then
            begin
              UniqueString(InternalChangeCase);
              p := @InternalChangeCase[i];
              unique := true;
            end;
            p^ := WideChar(Ord(p^) + Adjustment);
          end;
          inc(p);
        end;
      end;

    function UpperCase(const s : UnicodeString) : UnicodeString;
      begin
        UpperCase := InternalChangeCase(s, ['a'..'z'], -32);
      end;

    function GetEnvironmentVariable(const EnvVar : UnicodeString) : UnicodeString;
    var
      s, upperenv : UnicodeString;
      i : Longint;
      hp, p : PWideChar;
    begin
      GetEnvironmentVariable := '';
      p := GetEnvironmentStringsW;
      hp := p;
      upperenv := uppercase(envvar);
      while hp^ <> #0 do
      begin
        s := hp;
        i := pos('=', s);
        if uppercase(copy(s,1,i-1)) = upperenv then
        begin
          GetEnvironmentVariable := copy(s, i+1, length(s)-i);
          break;
        end;
        { next string entry }
        hp := hp + strlen(hp) + 1;
      end;
      FreeEnvironmentStringsW(p);
    end;

    function getTempDir: String;
    var
      astringLength : Integer;
    begin
      getTempDir := GetEnvironmentVariable('TMP');
      if getTempDir = '' then
        getTempDir := GetEnvironmentVariable('TEMP');
      astringlength := Length(getTempDir);
      if (astringlength > 0) and (getTempDir[astringlength] <> DirectorySeparator) then
        getTempDir := getTempDir + DirectorySeparator;
    end;

    {$pop}

{$ELSEIF defined(UNIX) and not defined(android)}

  function getTempDir: string;
    var
      key: string;
      value: string;
      i_env, i_key, i_value: integer;
    begin
      value := '/tmp/';  (** default for UNIX **)
      while (envp <> NIL) and assigned(envp^) do
      begin
        i_env := 0;
        i_key := 1;
        while not (envp^[i_env] in ['=', #0]) do
        begin
          key[i_key] := envp^[i_env];
          inc(i_env);
          inc(i_key);
        end;
        setlength(key, i_key - 1);
        if (key = 'TEMP') or (key = 'TMP') or (key = 'TMPDIR') then
        begin
          inc(i_env);    (** skip '=' **)
          i_value := 1;
          while (envp^[i_env] <> #0) do
          begin
            value[i_value] := envp^[i_env];
            inc(i_env);
            inc(i_value);
          end;
          setlength(value, i_value - 1);
        end;
        inc(envp);
      end;
      i_value:=length(value);
      if (i_value > 0) and (value[i_value] <> DirectorySeparator) then
        value := value + DirectorySeparator;
      getTempDir := value;
    end;

{$ELSE}  // neither unix nor windows

  function getTempDir: string;
  begin
    getTempDir:='';
  end;

{$ENDIF}
