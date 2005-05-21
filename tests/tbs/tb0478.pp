type
 integer = longint;

const
  {** @abstract(Character encoding value: UTF-8 storage format)}
  CHAR_ENCODING_UTF8 = 0;
  {** @abstract(Character encoding value: unknown format)}
  CHAR_ENCODING_UNKNOWN = -1;
  {** @abstract(Character encoding value: UTF-32 Big endian)}
  CHAR_ENCODING_UTF32BE = 1;
  {** @abstract(Character encoding value: UTF-32 Little endian)}
  CHAR_ENCODING_UTF32LE = 2;
  {** @abstract(Character encoding value: UTF-16 Little endian)}
  CHAR_ENCODING_UTF16LE = 3;
  {** @abstract(Character encoding value: UTF-16 Big endian)}
  CHAR_ENCODING_UTF16BE = 4;
  {** @abstract(Character encoding value: One byte per character storage
format)}
  CHAR_ENCODING_BYTE = 5;
  {** @abstract(Character encoding value: UTF-16 unknown endian
(determined by BOM))}
  CHAR_ENCODING_UTF16 = 6;
  {** @abstract(Character encoding value: UTF-32 unknown endian
(determined by BOM))}
  CHAR_ENCODING_UTF32 = 7;


function GetCharEncoding(alias: string; var _name: string): integer;
var
 encoding: integer;
 newencoding: integer;
begin
  _name:='';
  if length(alias) = 0 then
    Runerror(255);             { FAILED! }
  newencoding:=CHAR_ENCODING_UTF8;
  encoding:=CHAR_ENCODING_BYTE;
           case newencoding of
           { currently unsupported }
           CHAR_ENCODING_UNKNOWN:
              Begin
              end;
           { verify if we are using the correct encoding }
           CHAR_ENCODING_UTF16:
              begin
                if (encoding <> CHAR_ENCODING_UTF16BE) and
                   (encoding <> CHAR_ENCODING_UTF16LE) then
                      encoding:=255;
              end;
           { verify if we are using the correct encoding }
           CHAR_ENCODING_UTF32:
              begin
                if (encoding <> CHAR_ENCODING_UTF32BE) and
                   (encoding <> CHAR_ENCODING_UTF32LE) then
                      encoding:=255;
              end;
           CHAR_ENCODING_UTF16BE,
           CHAR_ENCODING_UTF16LE,
           CHAR_ENCODING_UTF32LE,
           CHAR_ENCODING_UTF32BE:
              begin
              end;
           else
              begin
                 encoding:=newencoding;
              end;
           end;
  if encoding <> CHAR_ENCODING_UTF8 then
    RunError(255);
end;


var
 _encoding: string;
Begin
 _encoding:='UTF-8';
 GetCharencoding(_encoding,_encoding);
end.
