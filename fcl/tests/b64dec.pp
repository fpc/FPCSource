// $Id$

// base64-decodes data from StdIn and writes the output to StdOut
// (c) 1999 Sebastian Guenther

{$MODE objfpc}

program b64dec;
uses classes, base64, sysutils;
var
  b64decoder: TBase64DecodingStream;
  InputStream: TStream;
  IsEnd: Boolean;
begin

  InputStream := THandleStream.Create(StdInputHandle);

  b64decoder := TBase64DecodingStream.Create(InputStream);

  IsEnd := False;
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
  Revision 1.4  2000-01-06 01:20:36  peter
    * moved out of packages/ back to topdir

  Revision 1.1  2000/01/03 19:33:10  peter
    * moved to packages dir

  Revision 1.2  1999/08/13 16:31:43  michael
  + Patch to support sizeless streams by Sebastian Guenter

  Revision 1.1  1999/08/09 16:12:26  michael
  * Fixes and new examples from Sebastian Guenther

}
