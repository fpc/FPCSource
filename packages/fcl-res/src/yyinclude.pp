{%MainUnit rcparser.pas}

{$IFDEF INC_HEADER}

type
  tyinclude = class
  const
    yi_maxlevels = 5;
  var
    stack: array[0..yi_maxlevels] of record
      yyinput           : Text;        (* input and output file *)
      yyline            : String;      (* current input line *)
      yylineno, yycolno : Integer;     (* current input position *)
      fn                : AnsiString;
      prev_wrap         : yywrap_t;
    end;
    level: integer;
    WorkDir: string;
    SearchPaths: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    class function wrapone(): Boolean; static;
    function push(const incfile: ansistring): Boolean;
    function pop(): Boolean;
    function expand(fn: AnsiString): AnsiString;
  end;

var
  yinclude: tyinclude;

{$ELSE}

class function tyinclude.wrapone(): Boolean;
begin
  Result:= yinclude.pop;
end;

function tyinclude.push(const incfile: ansistring): Boolean;
begin
  stack[level].yyinput:= yyinput;
  stack[level].yyline:= yyline;
  stack[level].yylineno:= yylineno;
  stack[level].yycolno:= yycolno;
  stack[level].prev_wrap:= yywrap;
  stack[level].fn:= yyfilename;
  inc(level);
  yywrap:= @tyinclude.wrapone;
  AssignFile(yyinput, incfile);
  Reset(yyinput);
  yyfilename:= incfile;
  yyline:= '';
  yylineno:= 0;
  yycolno:= 0;
  {$if declared(ypreproc)}
  ypreproc.newfile(yyfilename);
  {$endif}
  Result:= true;
end;

function tyinclude.pop(): Boolean;
begin
  Close(yyinput);
  Result:= level = 0;
  if not Result then begin
    Dec(level);
    yyinput:= stack[level].yyinput;
    yyline:= stack[level].yyline;
    yylineno:= stack[level].yylineno;
    yycolno:= stack[level].yycolno;
    yywrap:= stack[level].prev_wrap;
    yyfilename:= stack[level].fn;
    {$if declared(ypreproc)}
    ypreproc.newfile(yyfilename);
    {$endif}
  end;
end;

function tyinclude.expand(fn: AnsiString): AnsiString;
var
  i: integer;
  f: string;
begin
  result:= '';
  if Length(fn) > 3 then begin
    if (fn[1] = '<') and (fn[length(fn)] = '>') then begin
      fn:= copy(fn, 2, Length(fn)-2);
      for i:= 0 to SearchPaths.Count - 1 do begin
        f:= ConcatPaths([SearchPaths[i], fn]);
        if FileExists(f) then
          Exit(f);
      end;
      yyerror('Include file not found on search paths: <'+fn+'>');
    end
    else if (fn[1] = '"') and (fn[length(fn)] = '"') then begin
      fn:= copy(fn, 2, Length(fn)-2);
      f:= ConcatPaths([WorkDir, fn]);
      if FileExists(f) then
        Exit(f);
      if fn = 'windows.h' then begin
        // treat windows.h as an alias for windres.h
        f:= ConcatPaths([WorkDir, 'windres.h']);
        if FileExists(f) then
          Exit(f);
      end;
      yyerror('Include file not found: "'+fn+'"');
    end;
  end;
  yyerror('Invalid include directive: "'+fn+'"');
end;

constructor tyinclude.Create;
begin
  inherited Create;
  level:= 0;
  WorkDir:= GetCurrentDir;
  SearchPaths:= TStringList.Create;
end;

destructor tyinclude.Destroy;
begin
  FreeAndNil(SearchPaths);
  inherited;
end;

{$ENDIF}

