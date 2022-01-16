(*
  HPACK: Header Compression for HTTP/2 (rfc7541)
  ----------------------------------------------
  Pascal implementation of HTTP/2 headers send and receive process.

  Code based in Twitter's HPACK for java https://github.com/twitter/hpack

  History:

  2016.04.21 - Initial development by Jose Mejuto

  Package source files

    uhpackapi.pas (this file)
    uhpack.pas
    uhpacktables.pas
    rfc7541.txt (rfc based on)

  Basic API:

  HPackDecoder.Create(MaxHeaderSize,MaxHeaderTableSize)
    MaxHeaderSize: Each header block must not exceed this value (default: 8192)
    MaxHeaderTableSize: Max size for the dynamic table (default: 4096)

  HPackDecoder.Decode(DataStream)
    This procedure receives a RawByteString or a Stream and decodes its headers.
    If an OnAddHeader is created it will be called for each decoded header.
    After all data has been sent to "Decode" the plain headers can be accessed
    using "DecodedHeaders". After headers has been processed the function
    "EndHeaderBlockTruncated" should be called to verify that the headers has
    been successfully decoded.

  HPackEncoder.Create(MaxHeaderTableSize)
    Creates the Encoder with a MaxHeaderTableSize.

  HPackEncoder.AddHeader(OutputStream,Name,Value,bSensitive)
    Encodes a header pair Name/Value and also a sensitive flag (header should
    not be stored in internal tables nor in encoder, nor in decoder) in the
    OutputStream parameter.

  THPACKException
    Exception raised if some internal state do not work as expected, or sent
    information does not meets the structure expected.
    If the exception happens, even as some of the errors could be recovered, the
    best approach is to free the object and recreate again and also drop the
    http2 connection and restart it, as when this exception is raised is quite
    sure that the connection is out of sync with remote end point.

  License:

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit uhpack;

(*
  This file exposes only the needed symbols instead the whole
  infrastructure to handle HPack.
*)

{$mode objfpc}{$H+}

interface

uses
  uhpackimp;

const
  HPACK_MAX_HEADER_SIZE = uhpackimp.HPACK_MAX_HEADER_SIZE;
  HPACK_MAX_HEADER_TABLE_SIZE = uhpackimp.HPACK_MAX_HEADER_TABLE_SIZE;

type
  THPackDecoder=uhpackimp.THPackDecoder;
  THPackEncoder=uhpackimp.THPackEncoder;
  THPackHeaderAddEvent = uhpackimp.THPackHeaderAddEvent;
  THPACKException= uhpackimp.THPACKException;
  THPackHeaderTextList = uhpackimp.THPackHeaderTextList;

implementation

end.

