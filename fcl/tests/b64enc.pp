// $Id$

// base64-encodes data from StdIn and writes the output to StdOut
// (c) 1999 Sebastian Guenther

{$MODE objfpc}

program b64enc;
uses classes, base64, sysutils;
var
  b64encoder: TBase64EncodingStream;
  InputStream, OutputStream: TStream;
  IsEnd: Boolean;
begin

  InputStream := THandleStream.Create(StdInputHandle);
  OutputStream := THandleStream.Create(StdOutputHandle);

  b64encoder := TBase64EncodingStream.Create(OutputStream);

  while not IsEnd do
    try
      b64encoder.WriteByte(InputStream.ReadByte);
    except
      on e: EStreamError do IsEnd := True;
    end;

  b64encoder.Free;
  InputStream.Free;
  OutputStream.Free;
end.


{
  $Log$
  Revision 1.1  2000-07-13 06:33:45  michael
  + Initial import

  Revision 1.3  2000/01/06 01:20:36  peter
    * moved out of packages/ back to topdir

  Revision 1.1  2000/01/03 19:33:10  peter
    * moved to packages dir

  Revision 1.1  1999/08/09 16:12:26  michael
  * Fixes and new examples from Sebastian Guenther

}
