unit uvcoord_zero;

interface

uses
  ctypes;


const
  ZERO_BITMAP_WIDTH = 128;
  ZERO_BITMAP_HEIGHT = 256;
  ZERO_NUM_IMAGES = 10;


var
  zero_texcoords: array [1..ZERO_NUM_IMAGES * 4] of cuint = (
    0, 0, 52, 46,				// 0
    52, 0, 52, 46,				// 1
    0, 46, 52, 46,				// 2
    52, 46, 52, 46,				// 3
    0, 92, 52, 46,				// 4
    52, 92, 52, 46,				// 5
    0, 138, 52, 46,				// 6
    0, 184, 52, 46,				// 7
    52, 138, 52, 46,				// 8
    52, 184, 52, 46				// 9
  );

implementation

end.
