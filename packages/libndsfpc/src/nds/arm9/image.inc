{$ifdef NDS_INTERFACE}
type
//holds a rgb triplet
  RGB_24 = packed record
    r: cuchar;
    g: cuchar;
    b: cuchar;
  end;

 //holds a basic image type for loading image files
 sImage = record
    height: cshort; 
    width: cshort; 
    bpp: cint;
    palette: pcushort;
    image: record
      case integer of
        0: (data8: pcuchar);
        1: (data16: pcushort);
        2: (data32: pcuint);
    end;
  end;
  TsImage = sImage; 
  PsImage = ^sImage;


procedure image24to16(img: PsImage); cdecl; external;
procedure image8to16(img: PsImage); cdecl; external;
procedure image8to16trans(img: PsImage; transperentColor: cuint8); cdecl; external;
procedure imageDestroy(img: PsImage); cdecl; external;
procedure imageTileData(img: PsImage); cdecl; external;
{$endif NDS_INTERFACE}
