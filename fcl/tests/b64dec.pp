// $Id$

// base64-decodes a file (argument #1) and writes the output to StdOut
// (c) 1999 Sebastian Guenther

{$MODE objfpc}

program b64dec;
uses classes, base64, sysutils;
var
  b64decoder: TBase64DecodingStream;
  InputStream: TStream;
  IsEnd: Boolean;
begin

  InputStream := TFileStream.Create(ParamStr(1), fmOpenRead);

  b64decoder := TBase64DecodingStream.Create(InputStream);

  while not IsEnd do
    try
      Write(Chr(b64decoder.ReadByte));
    except
      on e: EStreamError do IsEnd := True;
    end;

  b64decoder.Free;
  InputStream.Free;
end.


{
  $Log$
  Revision 1.1  1999-08-09 16:12:26  michael
  * Fixes and new examples from Sebastian Guenther

}
