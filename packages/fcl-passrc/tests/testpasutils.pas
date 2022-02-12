unit TestPasUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PasTree;

function ExtractFileUnitName(aFilename: string): string;
function GetPasElementDesc(El: TPasElement): string;
procedure ReadNextPascalToken(var Position: PChar; out TokenStart: PChar;
  NestedComments: boolean; SkipDirectives: boolean);

implementation

function ExtractFileUnitName(aFilename: string): string;
var
  p: Integer;
begin
  Result:=ExtractFileName(aFilename);
  if Result='' then exit;
  for p:=length(Result) downto 1 do
    case Result[p] of
    '/','\': exit;
    '.':
      begin
      Delete(Result,p,length(Result));
      exit;
      end;
    end;
end;

function GetPasElementDesc(El: TPasElement): string;
begin
  if El=nil then exit('nil');
  Result:=El.Name+':'+El.ClassName+'['+El.SourceFilename+','+IntToStr(El.SourceLinenumber)+']';
end;

procedure ReadNextPascalToken(var Position: PChar; out TokenStart: PChar;
  NestedComments: boolean; SkipDirectives: boolean);
const
  IdentChars = ['a'..'z','A'..'Z','_','0'..'9'];
  HexNumberChars = ['0'..'9','a'..'f','A'..'F'];
var
  c1:char;
  CommentLvl: Integer;
  Src: PChar;
begin
  Src:=Position;
  // read till next atom
  while true do
    begin
    case Src^ of
    #0: break;
    #1..#32:  // spaces and special characters
      inc(Src);
    #$EF:
      if (Src[1]=#$BB)
      and (Src[2]=#$BF) then
        begin
        // skip UTF BOM
        inc(Src,3);
        end
      else
        break;
    '{':    // comment start or compiler directive
      if (Src[1]='$') and (not SkipDirectives) then
        // compiler directive
        break
      else begin
        // Pascal comment => skip
        CommentLvl:=1;
        while true do
          begin
          inc(Src);
          case Src^ of
          #0: break;
          '{':
            if NestedComments then
              inc(CommentLvl);
          '}':
            begin
            dec(CommentLvl);
            if CommentLvl=0 then
              begin
              inc(Src);
              break;
              end;
            end;
          end;
        end;
      end;
    '/':  // comment or real division
      if (Src[1]='/') then
        begin
        // comment start -> read til line end
        inc(Src);
        while not (Src^ in [#0,#10,#13]) do
          inc(Src);
        end
      else
        break;
    '(':  // comment, bracket or compiler directive
      if (Src[1]='*') then
        begin
        if (Src[2]='$') and (not SkipDirectives) then
          // compiler directive
          break
        else
          begin
          // comment start -> read til comment end
          inc(Src,2);
          CommentLvl:=1;
          while true do
            begin
            case Src^ of
            #0: break;
            '(':
              if NestedComments and (Src[1]='*') then
                inc(CommentLvl);
            '*':
              if (Src[1]=')') then
                begin
                dec(CommentLvl);
                if CommentLvl=0 then
                  begin
                  inc(Src,2);
                  break;
                  end;
                inc(Position);
                end;
            end;
            inc(Src);
            end;
        end;
      end else
        // round bracket open
        break;
    else
      break;
    end;
    end;
  // read token
  TokenStart:=Src;
  c1:=Src^;
  case c1 of
  #0:
    ;
  'A'..'Z','a'..'z','_':
    begin
    // identifier
    inc(Src);
    while Src^ in IdentChars do
      inc(Src);
    end;
  '0'..'9': // number
    begin
    inc(Src);
    // read numbers
    while (Src^ in ['0'..'9']) do
      inc(Src);
    if (Src^='.') and (Src[1]<>'.') then
      begin
      // real type number
      inc(Src);
      while (Src^ in ['0'..'9']) do
        inc(Src);
      end;
    if (Src^ in ['e','E']) then
      begin
      // read exponent
      inc(Src);
      if (Src^='-') then inc(Src);
      while (Src^ in ['0'..'9']) do
        inc(Src);
      end;
    end;
  '''','#':  // string constant
    while true do
      case Src^ of
      #0: break;
      '#':
        begin
        inc(Src);
        while Src^ in ['0'..'9'] do
          inc(Src);
        end;
      '''':
        begin
        inc(Src);
        while not (Src^ in ['''',#0]) do
          inc(Src);
        if Src^='''' then
          inc(Src);
        end;
      else
        break;
      end;
  '$':  // hex constant
    begin
    inc(Src);
    while Src^ in HexNumberChars do
      inc(Src);
    end;
  '&':  // octal constant or keyword as identifier (e.g. &label)
    begin
    inc(Src);
    if Src^ in ['0'..'7'] then
      while Src^ in ['0'..'7'] do
        inc(Src)
    else
      while Src^ in IdentChars do
        inc(Src);
    end;
  '{':  // compiler directive (it can't be a comment, because see above)
    begin
    CommentLvl:=1;
    while true do
      begin
      inc(Src);
      case Src^ of
      #0: break;
      '{':
        if NestedComments then
          inc(CommentLvl);
      '}':
        begin
        dec(CommentLvl);
        if CommentLvl=0 then
          begin
          inc(Src);
          break;
          end;
        end;
      end;
      end;
    end;
  '(':  // bracket or compiler directive
    if (Src[1]='*') then
      begin
      // compiler directive -> read til comment end
      inc(Src,2);
      while (Src^<>#0) and ((Src^<>'*') or (Src[1]<>')')) do
        inc(Src);
      inc(Src,2);
      end
    else
      // round bracket open
      inc(Src);
  #192..#255:
    begin
    // read UTF8 character
    inc(Src);
    if ((ord(c1) and %11100000) = %11000000) then
      begin
      // could be 2 byte character
      if (ord(Src[0]) and %11000000) = %10000000 then
        inc(Src);
      end
    else if ((ord(c1) and %11110000) = %11100000) then
      begin
      // could be 3 byte character
      if ((ord(Src[0]) and %11000000) = %10000000)
      and ((ord(Src[1]) and %11000000) = %10000000) then
        inc(Src,2);
      end
    else if ((ord(c1) and %11111000) = %11110000) then
      begin
      // could be 4 byte character
      if ((ord(Src[0]) and %11000000) = %10000000)
      and ((ord(Src[1]) and %11000000) = %10000000)
      and ((ord(Src[2]) and %11000000) = %10000000) then
        inc(Src,3);
      end;
    end;
  else
    inc(Src);
    case c1 of
    '<': if Src^ in ['>','='] then inc(Src);
    '.': if Src^='.' then inc(Src);
    '@':
      if Src^='@' then
        begin
        // @@ label
        repeat
          inc(Src);
        until not (Src^ in IdentChars);
        end
    else
      if (Src^='=') and (c1 in [':','+','-','/','*','<','>']) then
        inc(Src);
    end;
  end;
  Position:=Src;
end;

end.

