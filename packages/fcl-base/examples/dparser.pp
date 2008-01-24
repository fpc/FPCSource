Program DParser;

{$mode objfpc}{$h+}

uses Classes;

var
  InFile           : TFileStream;
  Parser           : TParser;
begin
  InFile := TFileStream.Create('parser.dat', fmOpenRead);
  if Assigned(InFile) then begin
    try
      Parser := TParser.Create(InFile);
      if Assigned(Parser) then begin
        try
          while Parser.Token <> toEOF do begin
            case Parser.Token of
              toInteger : WriteLn('Found integer: "', Parser.TokenInt, '"');
              toFloat   : WriteLn('Found float:   "', Parser.TokenFloat, '"');
              toString  : WriteLn('Found string:  "', Parser.TokenString, '"');
              toSymbol  : WriteLn('Found symbol:  "', Parser.TokenString, '"');
            else
              // Skip all other characters
              ;
            end;
            Parser.NextToken;
          end;
        finally
          WriteLn('Freeing parser object');
          Parser.Free;
        end;
      end;
    finally
      WriteLn('Freeing infile object');
      InFile.Free;
    end;
  end;
end.
