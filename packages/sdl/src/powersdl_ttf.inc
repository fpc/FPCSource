
var PowerSDLTTFBase : pLibrary;

const
    POWERSDL_TTFNAME : PChar = 'powersdl_ttf.library';

function TTF_Linked_Version : pSDL_version; syscall r12base PowerSDLTTFBase 028;
procedure TTF_ByteSwappedUNICODE(swapped : LongInt); syscall r12base PowerSDLTTFBase 034;
function TTF_Init : LongInt; syscall r12base PowerSDLTTFBase 040;
function TTF_OpenFont(const file_ : pChar; ptsize : LongInt) : pTTF_Font; syscall r12base PowerSDLTTFBase 046;
function TTF_OpenFontIndex(const file_ : pChar; ptsize : LongInt; index : LongInt) : pTTF_Font; syscall r12base PowerSDLTTFBase 052;
function TTF_OpenFontRW(src : pSDL_RWops; freesrc : LongInt; ptsize : LongInt) : pTTF_Font; syscall r12base PowerSDLTTFBase 058;
function TTF_OpenFontIndexRW(src : pSDL_RWops; freesrc : LongInt; ptsize : LongInt; index : LongInt) : pTTF_Font; syscall r12base PowerSDLTTFBase 064;
function TTF_GetFontStyle(font : pTTF_Font) : LongInt; syscall r12base PowerSDLTTFBase 070;
procedure TTF_SetFontStyle(font : pTTF_Font; style : LongInt); syscall r12base PowerSDLTTFBase 076;
function TTF_FontHeight(font : pTTF_Font) : LongInt; syscall r12base PowerSDLTTFBase 082;
function TTF_FontAscent(font : pTTF_Font) : LongInt; syscall r12base PowerSDLTTFBase 088;
function TTF_FontDescent(font : pTTF_Font) : LongInt; syscall r12base PowerSDLTTFBase 094;
function TTF_FontLineSkip(font : pTTF_Font) : LongInt; syscall r12base PowerSDLTTFBase 100;
function TTF_FontFaces(font : pTTF_Font) : LongInt; syscall r12base PowerSDLTTFBase 106;
function TTF_FontFaceIsFixedWidth(font : pTTF_Font) : LongInt; syscall r12base PowerSDLTTFBase 112;
function TTF_FontFaceFamilyName(font : pTTF_Font) : pChar; syscall r12base PowerSDLTTFBase 118;
function TTF_FontFaceStyleName(font : pTTF_Font) : pChar; syscall r12base PowerSDLTTFBase 124;
function TTF_GlyphMetrics(font : pTTF_Font; ch : Word; minx : pLongInt; maxx : pLongInt; miny : pLongInt; maxy : pLongInt; advance : pLongInt) : LongInt; syscall r12base PowerSDLTTFBase 130;
function TTF_SizeText(font : pTTF_Font; const text : pChar; w : pLongInt; h : pLongInt) : LongInt; syscall r12base PowerSDLTTFBase 136;
function TTF_SizeUTF8(font : pTTF_Font; const text : pChar; w : pLongInt; h : pLongInt) : LongInt; syscall r12base PowerSDLTTFBase 142;
function TTF_SizeUNICODE(font : pTTF_Font; const text : Word; w : pLongInt; h : pLongInt) : LongInt; syscall r12base PowerSDLTTFBase 148;
function TTF_RenderText_Solid(font : pTTF_Font; const text : pChar; fg : tSDL_Color) : pSDL_Surface; syscall r12base PowerSDLTTFBase 154;
function TTF_RenderUTF8_Solid(font : pTTF_Font; const text : pChar; fg : tSDL_Color) : pSDL_Surface; syscall r12base PowerSDLTTFBase 160;
function TTF_RenderUNICODE_Solid(font : pTTF_Font; const text : Word; fg : tSDL_Color) : pSDL_Surface; syscall r12base PowerSDLTTFBase 166;
function TTF_RenderGlyph_Solid(font : pTTF_Font; ch : Word; fg : tSDL_Color) : pSDL_Surface; syscall r12base PowerSDLTTFBase 172;
function TTF_RenderText_Shaded(font : pTTF_Font; const text : pChar; fg : tSDL_Color; bg : tSDL_Color) : pSDL_Surface; syscall r12base PowerSDLTTFBase 178;
function TTF_RenderUTF8_Shaded(font : pTTF_Font; const text : pChar; fg : tSDL_Color; bg : tSDL_Color) : pSDL_Surface; syscall r12base PowerSDLTTFBase 184;
function TTF_RenderUNICODE_Shaded(font : pTTF_Font; const text : Word; fg : tSDL_Color; bg : tSDL_Color) : pSDL_Surface; syscall r12base PowerSDLTTFBase 190;
function TTF_RenderGlyph_Shaded(font : pTTF_Font; ch : Word; fg : tSDL_Color; bg : tSDL_Color) : pSDL_Surface; syscall r12base PowerSDLTTFBase 196;
function TTF_RenderText_Blended(font : pTTF_Font; const text : pChar; fg : tSDL_Color) : pSDL_Surface; syscall r12base PowerSDLTTFBase 202;
function TTF_RenderUTF8_Blended(font : pTTF_Font; const text : pChar; fg : tSDL_Color) : pSDL_Surface; syscall r12base PowerSDLTTFBase 208;
function TTF_RenderUNICODE_Blended(font : pTTF_Font; const text : Word; fg : tSDL_Color) : pSDL_Surface; syscall r12base PowerSDLTTFBase 214;
function TTF_RenderGlyph_Blended(font : pTTF_Font; ch : Word; fg : tSDL_Color) : pSDL_Surface; syscall r12base PowerSDLTTFBase 220;
procedure TTF_CloseFont(font : pTTF_Font); syscall r12base PowerSDLTTFBase 226;
procedure TTF_Quit; syscall r12base PowerSDLTTFBase 232;
function TTF_WasInit : LongInt; syscall r12base PowerSDLTTFBase 238;
