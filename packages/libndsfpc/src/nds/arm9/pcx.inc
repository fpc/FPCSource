{$ifdef NDS_INTERFACE}
type
  // short int = cint16???
  PCXHeader = packed record
    manufacturer: cchar;   //should be 0
    version: cchar;        //should be 5 
    encoding: cchar;       //should be 1
    bitsPerPixel: cchar; //should be 8
    xmin,ymin: cint16;      //coordinates for top left,bottom right
    xmax,ymax: cint16;     
    hres: cint16;           //resolution
    vres: cint16;
    palette16: array [0..47] of cchar;  //16 color palette if 16 color image
    reserved: cchar;       //ignore
    colorPlanes: cchar;   //ignore
    bytesPerLine: cint16; 
    paletteYype: cint16;   //should be 2
    filler: array [0..57] of cchar;     //ignore
  end;
  TPCXHeader = PCXHeader;
  PPCXHeader = ^PCXHeader;


function loadPCX(const pcx: pcuchar; image: PsImage): cint; cdecl; external;
{$endif NDS_INTERFACE}
