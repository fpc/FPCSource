
var PowerSDLImageBase : pLibrary;

const
    POWERSDL_IMAGENAME : PChar = 'powersdl_image.library';

function IMG_LoadTyped_RW(src : pSDL_RWops; freesrc : LongInt; type_ : pChar) : pSDL_Surface; syscall r12base PowerSDLImageBase 028;
function IMG_Load(const file_ : pChar) : pSDL_Surface; syscall r12base PowerSDLImageBase 034;
function IMG_Load_RW(src : pSDL_RWops; freesrc : LongInt) : pSDL_Surface; syscall r12base PowerSDLImageBase 040;
function IMG_InvertAlpha(on : LongInt) : LongInt; syscall r12base PowerSDLImageBase 046;
function IMG_isBMP(src : pSDL_RWops) : LongInt; syscall r12base PowerSDLImageBase 052;
function IMG_isPNM(src : pSDL_RWops) : LongInt; syscall r12base PowerSDLImageBase 058;
function IMG_isXPM(src : pSDL_RWops) : LongInt; syscall r12base PowerSDLImageBase 064;
function IMG_isXCF(src : pSDL_RWops) : LongInt; syscall r12base PowerSDLImageBase 070;
function IMG_isPCX(src : pSDL_RWops) : LongInt; syscall r12base PowerSDLImageBase 076;
function IMG_isGIF(src : pSDL_RWops) : LongInt; syscall r12base PowerSDLImageBase 082;
function IMG_isJPG(src : pSDL_RWops) : LongInt; syscall r12base PowerSDLImageBase 088;
function IMG_isTIF(src : pSDL_RWops) : LongInt; syscall r12base PowerSDLImageBase 094;
function IMG_isPNG(src : pSDL_RWops) : LongInt; syscall r12base PowerSDLImageBase 100;
function IMG_isLBM(src : pSDL_RWops) : LongInt; syscall r12base PowerSDLImageBase 106;
function IMG_LoadBMP_RW(src : pSDL_RWops) : pSDL_Surface; syscall r12base PowerSDLImageBase 112;
function IMG_LoadPNM_RW(src : pSDL_RWops) : pSDL_Surface; syscall r12base PowerSDLImageBase 118;
function IMG_LoadXPM_RW(src : pSDL_RWops) : pSDL_Surface; syscall r12base PowerSDLImageBase 124;
function IMG_LoadXCF_RW(src : pSDL_RWops) : pSDL_Surface; syscall r12base PowerSDLImageBase 130;
function IMG_LoadPCX_RW(src : pSDL_RWops) : pSDL_Surface; syscall r12base PowerSDLImageBase 136;
function IMG_LoadGIF_RW(src : pSDL_RWops) : pSDL_Surface; syscall r12base PowerSDLImageBase 142;
function IMG_LoadJPG_RW(src : pSDL_RWops) : pSDL_Surface; syscall r12base PowerSDLImageBase 148;
function IMG_LoadTIF_RW(src : pSDL_RWops) : pSDL_Surface; syscall r12base PowerSDLImageBase 154;
function IMG_LoadPNG_RW(src : pSDL_RWops) : pSDL_Surface; syscall r12base PowerSDLImageBase 160;
function IMG_LoadTGA_RW(src : pSDL_RWops) : pSDL_Surface; syscall r12base PowerSDLImageBase 166;
function IMG_LoadLBM_RW(src : pSDL_RWops) : pSDL_Surface; syscall r12base PowerSDLImageBase 172;
function IMG_ReadXPMFromArray(var xpm : pShortInt) : pSDL_Surface; syscall r12base PowerSDLImageBase 178;
function IMG_Linked_Version : pSDL_version; syscall r12base PowerSDLImageBase 184;
function IMG_isXV(src : pSDL_RWops) : LongInt; syscall r12base PowerSDLImageBase 190;
function IMG_LoadXV_RW(src : pSDL_RWops) : pSDL_Surface; syscall r12base PowerSDLImageBase 196;
