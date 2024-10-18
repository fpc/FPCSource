//  (C) Copyright 1993-1995 Sony Corporation,Tokyo,Japan. All Rights Reserved
// 		libgpu.h: Graphic Primitive Structures Database
{
 *
 * Primitive list:
 *
 *      Name     |Size*1|Shade  |Vertex |Texture| Function
 *      ---------+------+-------+-------+-------+------------------------
 *      POLY_F3  | 5	|Flat   |   3   |OFF    | Flat Triangle
 *      POLY_FT3 | 8	|Flat   |   3   |ON     | Flat Textured Triangle
 *      POLY_G3  | 7	|Gouraud|   3   |OFF    | Gouraud Triangle
 *      POLY_GT3 |10	|Gouraud|   3   |ON     | Gouraud Textured Triangle
 *      POLY_F4  | 6	|Flat   |   4   |OFF    | Flat Quadrangle
 *      POLY_FT4 |10	|Flat   |   4   |ON     | Flat Textured Quadrangle
 *      POLY_G4  | 9	|Gouraud|   4   |OFF    | Gouraud Quadrangle
 *      POLY_GT4 |13	|Gouraud|   4   |ON     | Gouraud Textured Quadrangle
 *      ---------+------+-------+-------+-------+------------------------
 *      LINE_F2  | 4	|Flat   |   2   | -     | unconnected Flat Line 
 *      LINE_G2  | 5	|Gouraud|   2   | -     | unconnected Gouraud Line 
 *      LINE_F3  | 6	|Flat	|   3	| -     | 3-connected Flat Line
 *      LINE_G3  | 8	|Gouraud|   3	| -     | 3-connected Gouraud Line
 *      LINE_F4  | 7	|Flat	|   4	| -    	| 4-connected Flat Line
 *      LINE_G4  |10	|Gouraud|   4	| -    	| 4-connected Gouraud Line
 *      ---------+------+-------+-------+-------+------------------------
 *      SPRT	 | 5	|Flat	|   1   |ON     | free size Sprite
 *      SPRT_16	 | 4	|Flat	|   1   |ON     | 16x16 Sprite
 *      SPRT_8	 | 4	|Flat	|   1   |ON     | 8x8 Sprite
 *      ---------+------+-------+-------+-------+------------------------
 *      TILE	 | 4	|Flat	|   1   |OFF    | free size Sprite
 *      TILE_16	 | 3	|Flat	|   1   |OFF    | 16x16 Sprite
 *      TILE_8	 | 3	|Flat	|   1   |OFF    | 8x8 Sprite
 *      TILE_1	 | 3	|Flat	|   1   |OFF    | 1x1 Sprite
 *      ---------+------+-------+-------+-------+------------------------
 *      DR_TWIN	 | 3	|   -	|   -   | -     | Texture Window
 *      DR_AREA	 | 3	|   -	|   -   | -     | Drawing Area
 *      DR_OFFSET| 3	|   -	|   -   | -     | Drawing Offset
 *      DR_MODE  | 3	|   -	|   -   | -     | Drawing Mode
 *      DR_ENV   |16	|   -	|   -	| -     | Drawing Environment
 *      DR_MOVE  | 6	|   -	|   -	| -     | MoveImage
 *      DR_LOAD  |17	|   -	|   -	| -     | LoadImage
 *      DR_TPAGE | 2    |   -   |   -   | -     | Drawing TPage
 *      DR_STP   | 3    |   -   |   -   | -     | Drawing STP
 *
 *	*1: in long-word
 *
 * Texture Attributes:
 *	abr: ambient rate
 *		abr	0	1	2	3
 *		-------------------------------------
 *		Front	0.5	1.0	0.5	-1.0
 *		Back	0.5	1.0	1.0	 1.0
 *
 *	tp: texture mode
 *		 tp	0	1	2	
 *		 -----------------------------
 *		 depth	4bit	8bit	16bit
 *		 color	CLUT	CLUT	DIRECT
}
unit libgpu; 
interface
uses libgte;

function GPU_printf(fmt: pchar): longint; varargs; stdcall; external; // printf() object

// Time-out Cycle
const
	WAIT_TIME	=	$800000;

//	General
procedure limitRange(var x: longint; l, h: longint); inline;




// Rectangle:
type
	RECT = packed record
			x, y : smallint;	// offset point on VRAM
			w, h : smallint;	// width and height
	       end;
	PRECT = ^RECT;

	RECT32 = packed record
			x, y : longint;		// offset point on VRAM
			w, h : longint;		// width and height
		 end;


// Environment 

	DR_ENV = packed record			// Packed Drawing Environment
			tag : dword;
			code : array [0..14] of dword;
		 end;
	PDR_ENV = ^DR_ENV;

	       
	DRAWENV = packed record
				clip : RECT;						// clip area
				ofs : array [0..1] of smallint;		// drawing offset
				tw : RECT;							// texture window
				tpage : word;						// texture page
				dtd : byte;							// dither flag (0:off, 1:on)
				dfe : byte;							// flag to draw on display area (0:off 1:on)
				isbg : byte;						// enable to auto-clear
				r0, g0, b0 : byte;					// initital background color
				_dr_env : DR_ENV;					// reserved
			  end;
	PDRAWENV = ^DRAWENV;
	       
	DISPENV = packed record
				disp : RECT;						// display area
				screen : RECT;						// display start point
				isinter : byte;						// interlace 0: off 1: on
				isrgb24 : byte;						// RGB24 bit mode
				pad0, pad1 : byte;					// reserved
			  end;
	PDISPENV = ^DISPENV;

// Polygon Primitive Definitions
	P_TAG = bitpacked record
        		addr: 0..16777215; 						// 24-bit address (24 bits can represent numbers from 0 to 16777215)
        		len: 0..255;       						// 8-bit length (8 bits can represent numbers from 0 to 255)
        		r0, g0, b0, code: Byte;
          	end;
	
	P_CODE = packed record
				r0, g0, b0, code : byte;
			 end;
	
	POLY_F3 = packed record							// Flat Triangle
			tag : dword;
			r0, g0, b0, code : byte;
			x0,	y0 : smallint;
			x1,	y1 : smallint;
			x2,	y2 : smallint;
		  end;
	PPOLY_F3 = ^POLY_F3;

	POLY_F4 = packed record 						// Flat Quadrangle
			tag : dword;
			r0, g0, b0, code : byte;
			x0,	y0 : smallint;
			x1,	y1 : smallint;
			x2,	y2 : smallint;
			x3,	y3 : smallint;
		  end;
	PPOLY_F4 = ^POLY_F4;



	POLY_FT3 = packed record	  					// Flat Textured Triangle
			tag : dword;
			r0, g0, b0, code : byte;
			x0,	y0 : smallint;
			u0, v0 : byte;
			clut : word;
			x1,	y1 : smallint;
			u1, v1 : byte;
			tpage : word;
			x2,	y2 : smallint;
			u2, v2 : byte;
			pad1 : word;
		   end;
	PPOLY_FT3 = ^POLY_FT3;

	POLY_FT4 = packed record						// Flat Textured Quadrangle
			tag : dword;
			r0, g0, b0, code : byte;
			x0, y0 : smallint;
			u0, v0 : byte;	
			clut : word;
			x1,	y1 : smallint;
			u1, v1 : byte;
			tpage : word;
			x2,	y2 : smallint;
			u2, v2 : byte;
			pad1 : word;
			x3,	y3 : smallint;
			u3, v3 : byte;
			pad2 : word;
		   end;
	PPOLY_FT4 = ^POLY_FT4;

	POLY_G3 = packed record							// Gouraud Triangle
			tag : dword;
			r0, g0, b0, code : byte;
			x0,	y0 : smallint;
			r1, g1, b1, pad1 : byte;
			x1,	y1 : smallint;
			r2, g2, b2, pad2 : byte;
			x2,	y2 : smallint;
		  end;
	PPOLY_G3 = ^POLY_G3;

	POLY_G4 = packed record							// Gouraud Quadrangle
			tag : dword;
			r0, g0, b0, code : byte;
			x0,	y0 : smallint;
			r1, g1, b1, pad1 : byte;
			x1,	y1 : smallint;
			r2, g2, b2, pad2 : byte;
			x2,	y2 : smallint;
			r3, g3, b3, pad3 : byte;
			x3,	y3 : smallint;
		  end;
	PPOLY_G4 = ^POLY_G4;

	POLY_GT3 = packed record						// Gouraud Textured Triangle
			tag : dword;
			r0, g0, b0, code : byte;
			x0, y0 : smallint;
			u0, v0 : byte;
			clut : word;
			r1, g1, b1, p1 : byte;
			x1,	y1 : smallint;
			u1, v1 : byte;	
			tpage : word;
			r2, g2, b2, p2 : byte;
			x2,	y2 : smallint;
			u2, v2 : byte;	
			pad2 : word;
		  end;
	PPOLY_GT3 = ^POLY_GT3;

	POLY_GT4 = packed record 						// Gouraud Textured Quadrangle
			tag : dword;
			r0, g0, b0, code : byte;
			x0, y0 : smallint;
			u0, v0 : byte;
			clut : word;
			r1, g1, b1, p1 : byte;
			x1, y1 : smallint;
			u1, v1 : byte;	
			tpage : word;
			r2, g2, b2, p2 : byte;
			x2, y2 : smallint;
			u2, v2 : byte;	
			pad2 : word;
			r3, g3, b3, p3 : byte;
			x3, y3 : smallint;
			u3, v3 : byte;	
			pad3 : word;
		 end;
	PPOLY_GT4 = ^POLY_GT4;


// Line Primitive Definitions
	LINE_F2 = packed record							// Unconnected Flat Line
			tag : dword;
			r0, g0, b0, code : byte;
			x0, y0 : smallint;
			x1, y1 : smallint;
		  end;
	PLINE_F2 = ^LINE_F2;

	LINE_G2 = packed record							// Unconnected Gouraud Line
			tag : dword;
			r0, g0, b0, code : byte;
			x0, y0 : smallint;
			r1, g1, b1, p1 : byte;
			x1, y1 : smallint;
		  end;
	PLINE_G2 = ^LINE_G2;

	LINE_F3 = packed record							// 2 connected Flat Line
			tag : dword;
			r0, g0, b0, code : byte;
			x0, y0 : smallint;
			x1, y1 : smallint;
			x2, y2 : smallint;
			pad : dword;
		  end;
	PLINE_F3 = ^LINE_F3;


	LINE_G3 = packed record							// 2 connected Gouraud Line
			tag : dword;
			r0, g0, b0, code : byte;
			x0, y0 : smallint;
			r1, g1, b1, p1 : byte;
			x1, y1 : smallint;
			r2, g2, b2, p2 : byte;
			x2, y2 : smallint;
			pad : dword;
		  end;
	PLINE_G3 = ^LINE_G3;

	LINE_F4 = packed record							// 3 connected Flat Line Quadrangle
				tag : dword;
				r0, g0, b0, code : byte;
				x0, y0 : smallint;
				x1, y1 : smallint;
				x2, y2 : smallint;
				x3, y3 : smallint;
				pad : dword;
		  end;
	PLINE_F4 = ^LINE_F4;

	LINE_G4 = packed record							// 3 connected Gouraud Line
			tag : dword;
			r0, g0, b0, code : byte;
			x0, y0 : smallint;
			r1, g1, b1, p1 : byte;
			x1, y1 : smallint;
			r2, g2, b2, p2 : byte;
			x2, y2 : smallint;
			r3, g3, b3, p3 : byte;
			x3, y3 : smallint;
			pad : dword;
		  end;
	PLINE_G4 = ^LINE_G4;


// Sprite Primitive Definitions
	SPRT = packed record							// free size Sprite
			tag : dword;
			r0, g0, b0, code : byte;
			x0, y0 : smallint;
			u0, v0 : byte;	
			clut : word;
			w, h : smallint;
	      end;
	PSPRT = ^SPRT;

	SPRT_16 = packed record						// 16x16 Sprite
			tag : dword;
			r0, g0, b0, code : byte;
			x0, y0 : smallint;
			u0, v0 : byte;
			clut : word;
		  end;
	PSPRT_16 = ^SPRT_16;

	       
 	SPRT_8 = packed record							// 8x8 Sprite
			tag : dword;
			r0, g0, b0, code : byte;
			x0,	y0 : smallint;
			u0, v0 : byte;	
			clut : word;
		  end;
	PSPRT_8 = ^SPRT_8;
	       
// Tile Primitive Definitions
	TILE = packed record							// free size Tile
			tag : dword;
			r0, g0, b0, code : byte;
			x0, y0 : smallint;
			w, h : smallint;
	       end;
	PTILE = ^TILE;

	TILE_16 = packed record							// 16x16 Tile
			tag : dword;
			r0, g0, b0, code : byte;
			x0, y0 : smallint;
		  end;
	PTILE_16 = ^TILE_16;



	TILE_8 = packed record							// 8x8 Tile
			tag : dword;
			r0, g0, b0, code : byte;
			x0, y0 : smallint;
		 end;
	PTILE_8 = ^TILE_8;


	TILE_1 = packed record							// 1x1 Tile
			tag : dword;
			r0, g0, b0, code : byte;
			x0, y0 : smallint;
		 end;
	PTILE_1 = TILE_1;

//  Special Primitive Definitions
	DR_MODE = packed record							// Drawing Mode
			tag : dword;
			code : array [0..1] of dword;
		  end;
	PDR_MODE = ^DR_MODE;

	DR_TWIN = packed record							// Texture Window
			tag : dword;
			code : array [0..1] of dword;
		  end;
	PDR_TWIN =^DR_TWIN;

	DR_AREA = packed record							// Drawing Area
			tag : dword;
			code : array [0..1] of dword;
		  end;
	PDR_AREA = ^DR_AREA;

	       
	DR_OFFSET = packed record						// Drawing Offset
			tag : dword;
			code : array [0..1] of dword;
	 	    end;
	PDR_OFFSET = ^DR_OFFSET;

	       
 	DR_MOVE = packed record							// MoveImage
			tag : dword;
			code : array [0..4] of dword;
		  end;
	PDR_MOVE = ^DR_MOVE;


	DR_LOAD	= packed record							// LoadImage
			tag : dword;
			code : array [0..2] of dword;
			p : array [0..12] of dword;
		  end;
	PDR_LOAD = ^DR_LOAD;

	DR_TPAGE = packed record						// Drawing TPage
			tag : dword;
			code : array [0..0] of dword;
		   end;
	PDR_TPAGE = DR_TPAGE;

	DR_STP = packed record                          // Drawing STP
        		tag : dword;
       			code : array [0..1] of dword;
        	 end;
        PDR_STP = DR_STP;


//	Font Stream Parameters
const 
	FNT_MAX_ID = 8;									// max number of stream ID
	FNT_MAX_SPRT = 1024;							// max number of sprites in all streams


//	Multi-purpose Sony-TMD primitive
type
	TMD_PRIM = packed record
				id : dword;	
				r0, g0, b0, p0 : byte;				// Color of vertex 0
				r1, g1, b1, p1 : byte;				// Color of vertex 1
				r2, g2, b2, p2 : byte;				// Color of vertex 2
				r3, g3, b3, p3 : byte;				// Color of vertex 3
				tpage, clut : word;					// texture page ID, clut ID
				u0, v0, u1, v1 : byte;				// texture corner point
				u2, v2, u3, v3 : byte;
				
				// independent vertex model
				x0, x1, x2, x3 : SVECTOR;			// 3D corner point
				n0, n1, n2, n3 : SVECTOR;			// 3D corner normal vector
				
				// Common vertex model
				v_ofs : PSVECTOR;					// offset to vertex database
				n_ofs : PSVECTOR;					// offset to normal database
				
				vert0, vert1 : word; 				// index of vertex
				vert2, vert3 : word;		
				norm0, norm1 : word; 				// index of normal
				norm2, norm3 : word;
		   end;
	PTMD_PRIM = ^TMD_PRIM;
	


// Multi-purpose TIM image
	TIM_IMAGE = packed record
			mode : dword;					// pixel mode
			crect : PRECT;					// CLUT rectangle on frame buffer
			caddr : pdword;					// CLUT address on main memory
			prect : PRECT;					// texture image rectangle on frame buffer
			paddr : pdword;					// texture image address on main memory
		    end;
	PTIM_IMAGE = ^TIM_IMAGE;

function FntPrint(Args: pchar): longint; varargs; stdcall; external;
function KanjiFntPrint(Args: pchar): longint; varargs; stdcall; external;
function FntPrint: longint; stdcall; external;
function KanjiFntPrint: longint; stdcall; external;
	
function GetDispEnv(env: PDISPENV): PDISPENV; stdcall; external;
function PutDispEnv(env: PDISPENV): PDISPENV; stdcall; external;
function SetDefDispEnv(env: PDISPENV; x, y, w, h: longint): PDISPENV; stdcall; external;
function GetDrawEnv(env: PDRAWENV): PDRAWENV; stdcall; external;
function PutDrawEnv(env: PDRAWENV): PDRAWENV; stdcall; external;
function SetDefDrawEnv(env: PDRAWENV; x, y, w, h: longint): PDRAWENV; stdcall; external;
function ReadTIM(timimg: PTIM_IMAGE): PTIM_IMAGE; stdcall; external;
function ReadTMD(tmdprim: PTMD_PRIM): PTMD_PRIM; stdcall; external;
function CheckPrim(s: pchar; p: pdword): longint; stdcall; external;
function ClearImage(rect: PRECT; r, g, b: byte): longint; stdcall; external;
function ClearImage2(rect: PRECT; r, g, b: byte): longint; stdcall; external;
function DrawSync(mode: longint): longint; stdcall; external;
function FntOpen(x, y, w, h: longint; isbg, n: longint): longint; stdcall; external;
function GetGraphDebug: longint; stdcall; external;
function GetTimSize(sjis: pbyte): longint; stdcall; external;
function IsEndPrim(p: pointer): longint; stdcall; external;
function KanjiFntOpen(x, y, w, h: longint; dx, dy, cx, cy: longint; isbg, n: longint): longint; stdcall; external;
procedure KanjiFntClose; stdcall; external;
function Krom2Tim(sjis: pbyte; taddr:pdword; dx, dy, cdx, cdy: longint; fg, bg: dword): longint; stdcall; external;
function LoadImage(rect: PRECT; p: pointer): longint; stdcall; external;
function MargePrim(p0, p1: pointer): longint; stdcall; external;
function MoveImage(rect: PRECT; x, y: longint): longint; stdcall; external;
function OpenTIM(addr: pointer): longint; stdcall; external;
function OpenTMD(tmd: pdword; obj_no: longint): longint; stdcall; external;
function ResetGraph(mode: longint): longint; stdcall; external;
function SetGraphDebug(level: longint): longint; stdcall; external;
function StoreImage(rect: PRECT; p: pointer): longint; stdcall; external;
function ClearOTag(ot: pointer; n: longint): pdword; stdcall; external;
function ClearOTagR(ot: pointer; n: longint): pdword; stdcall; external;
function FntFlush(id: longint): pdword; stdcall; external;
function KanjiFntFlush(id: longint): pdword; stdcall; external;
function DrawSyncCallback(func: pointer): longint; stdcall; external;
function GetClut(x, y: longint): word; stdcall; external;
function GetTPage(tp, abr: longint; x, y: longint): word; stdcall; external;
function LoadClut(clut: pointer; x, y: longint): word; stdcall; external;
function LoadClut2(clut: pointer; x, y: longint): word; stdcall; external;
function LoadTPage(pix: pointer; tp, abr: longint; x, y, w, h: longint): word; stdcall; external;
function NextPrim(p: pointer): pointer; stdcall; external;
procedure AddPrim(ot: pointer; p: pointer); stdcall; external;
procedure AddPrims(ot: pointer; p0, p1: pointer); stdcall; external;
procedure CatPrim(p0, p1: pointer); stdcall; external;
procedure DrawOTag(p: pointer); stdcall; external;
procedure DrawOTagIO(p: pointer); stdcall; external;
procedure DrawOTagEnv(p: pointer; env: PDRAWENV); stdcall; external;
procedure DrawPrim(p: pointer); stdcall; external;
procedure DumpClut(clut: word); stdcall; external;
procedure DumpDispEnv(env: PDISPENV); stdcall; external;
procedure DumpDrawEnv(env: PDRAWENV); stdcall; external;
procedure DumpOTag(p: pointer); stdcall; external;
procedure DumpTPage(tpage: word); stdcall; external;
procedure FntLoad(tx, ty: longint); stdcall; external;
procedure SetDispMask(mask: longint); stdcall; external;
procedure SetDrawArea(p: PDR_AREA; r: PRECT); stdcall; external;
procedure SetDrawEnv(dr_env: PDR_ENV; env: PDRAWENV); stdcall; external;
procedure SetDrawLoad(p: PDR_LOAD; rect: PRECT); stdcall; external;
procedure SetDrawMode(p: PDR_MODE; dfe, dtd: longint; tpage: longint; tw: PRECT); stdcall; external;
procedure SetDrawTPage(p: PDR_TPAGE; dfe, dtd: longint; tpage: longint); stdcall; external;
procedure SetDrawMove(p: PDR_MOVE; r: PRECT; x, y: longint); stdcall; external;
procedure SetDrawOffset(p: PDR_OFFSET; ofs: pword); stdcall; external;
procedure SetDrawStp(p: PDR_STP; pbw: longint); stdcall; external;
procedure SetDumpFnt(id: longint); stdcall; external;
procedure SetLineF2(p: PLINE_F2); stdcall; external;
procedure SetLineF3(p: PLINE_F3); stdcall; external;
procedure SetLineF4(p: PLINE_F4); stdcall; external;
procedure SetLineG2(p: PLINE_G2); stdcall; external;
procedure SetLineG3(p: PLINE_G3); stdcall; external;
procedure SetLineG4(p: PLINE_G4); stdcall; external;
procedure SetPolyF3(p: PPOLY_F3); stdcall; external;
procedure SetPolyF4(p: PPOLY_F4); stdcall; external;
procedure SetPolyFT3(p: PPOLY_FT3); stdcall; external;
procedure SetPolyFT4(p: PPOLY_FT4); stdcall; external;
procedure SetPolyG3(p: PPOLY_G3); stdcall; external;
procedure SetPolyG4(p: PPOLY_G4); stdcall; external;
procedure SetPolyGT3(p: PPOLY_GT3); stdcall; external;
procedure SetPolyGT4(p: PPOLY_GT4); stdcall; external;
procedure SetSemiTrans(p: pointer; abe: longint); stdcall; external;
procedure SetShadeTex(p: pointer; tge: longint); stdcall; external;
procedure SetSprt(p: PSPRT); stdcall; external;
procedure SetSprt16(p: PSPRT_16); stdcall; external;
procedure SetSprt8(p: PSPRT_8); stdcall; external;
procedure SetTexWindow(p: PDR_TWIN; tw: PRECT); stdcall; external;
procedure SetTile(p: PTILE); stdcall; external;
procedure SetTile1(p: PTILE_1); stdcall; external;
procedure SetTile16(p: PTILE_16); stdcall; external;
procedure SetTile8(p: PTILE_8); stdcall; external;
procedure TermPrim(p: pointer); stdcall; external;
function BreakDraw: pdword; stdcall; external;
procedure ContinueDraw(insaddr: pdword; contaddr: pdword); stdcall; external;
function IsIdleGPU(max_count: longint): longint; stdcall; external;
function GetODE: longint; stdcall; external;
function LoadImage2(_rect: PRECT; p: pointer): longint; stdcall; external;
function StoreImage2(_rect: PRECT; p: pointer): longint; stdcall; external;
function MoveImage2(_rect: PRECT; x, y: longint): longint; stdcall; external;
function DrawOTag2(p: pointer): longint; stdcall; external;
procedure GetDrawMode(p: PDR_MODE); stdcall; external;
procedure GetTexWindow(p: PDR_TWIN); stdcall; external;
procedure GetDrawArea(p: PDR_AREA); stdcall; external;
procedure GetDrawOffset(p: PDR_OFFSET); stdcall; external;
procedure GetDrawEnv2(p: PDR_ENV); stdcall; external;





//	Set/Add Vector/Rectangle Attributes
procedure setVector(var v: VECTOR; const x, y, z: longint);
procedure setVector(var v: SVECTOR; const x, y, z: smallint);
procedure setVector(var v: DVECTOR; const x, y, z: smallint);

{
#define applyVector(v, _x, _y, _z, op) \
	(v)->vx op _x, (v)->vy op _y, (v)->vz op _z	
}

procedure copyVector(var v0: VECTOR; const v1: VECTOR);
procedure copyVector(var v0: SVECTOR; const v1: SVECTOR);
procedure copyVector(var v0: DVECTOR; const v1: DVECTOR);

procedure addVector(var v0: VECTOR; const v1: VECTOR);
procedure addVector(var v0: SVECTOR; const v1: SVECTOR);
procedure addVector(var v0: DVECTOR; const v1: DVECTOR);


procedure dumpVector(str: pchar; const v: VECTOR);
procedure dumpVector(str: pchar; const v: SVECTOR);
procedure dumpVector(str: pchar; const v: DVECTOR);

procedure dumpMatrix(const x: MATRIX);

procedure setRECT(var r: RECT; const x, y, w, h: longint);

// Set Primitive Attributes
procedure setTPage(var p: DRAWENV; tp, abr, x, y: longint);
procedure setTPage(var p: POLY_FT3; tp, abr, x, y: longint);
procedure setTPage(var p: POLY_FT4; tp, abr, x, y: longint);
procedure setTPage(var p: POLY_GT3; tp, abr, x, y: longint);
procedure setTPage(var p: POLY_GT4; tp, abr, x, y: longint);
procedure setTPage(var p: TMD_PRIM; tp, abr, x, y: longint);


procedure setClut(var p: POLY_FT3; x, y: longint);
procedure setClut(var p: POLY_FT4; x, y: longint);
procedure setClut(var p: POLY_GT3; x, y: longint);
procedure setClut(var p: POLY_GT4; x, y: longint);
procedure setClut(var p: SPRT; x, y: longint);
procedure setClut(var p: SPRT_16; x, y: longint);
procedure setClut(var p: SPRT_8; x, y: longint);
procedure setClut(var p: TMD_PRIM; x, y: longint);

(*
#define setClut(p,x,y) \
	((p)->clut = getClut(x,y))
*)					   

// Set Primitive Colors
procedure setRGB0(var p: POLY_F3; const r, g, b: byte);
procedure setRGB0(var p: POLY_F4; const r, g, b: byte);
procedure setRGB0(var p: POLY_FT3; const r, g, b: byte);
procedure setRGB0(var p: POLY_FT4; const r, g, b: byte);
procedure setRGB0(var p: POLY_G3; const r, g, b: byte);
procedure setRGB0(var p: POLY_G4; const r, g, b: byte);
procedure setRGB0(var p: POLY_GT3; const r, g, b: byte);
procedure setRGB0(var p: POLY_GT4; const r, g, b: byte);
procedure setRGB0(var c: DRAWENV; r, g, b: byte);

procedure setRGB1(var p: POLY_G3; const r, g, b: byte);
procedure setRGB1(var p: POLY_G4; const r, g, b: byte);
procedure setRGB1(var p: POLY_GT3; const r, g, b: byte);
procedure setRGB1(var p: POLY_GT4; const r, g, b: byte);

procedure setRGB2(var p: POLY_G3; const r, g, b: byte);
procedure setRGB2(var p: POLY_G4; const r, g, b: byte);
procedure setRGB2(var p: POLY_GT3; const r, g, b: byte);
procedure setRGB2(var p: POLY_GT4; const r, g, b: byte);

procedure setRGB3(var p: POLY_G4; const r, g, b: byte);
procedure setRGB3(var p: POLY_GT4; const r, g, b: byte);


// Set Primitive Screen Points
procedure setXY0(var p: POLY_F3; const x, y: smallint);
procedure setXY0(var p: POLY_F4; const x, y: smallint);
procedure setXY0(var p: POLY_FT3; const x, y: smallint);
procedure setXY0(var p: POLY_FT4; const x, y: smallint);
procedure setXY0(var p: POLY_G3; const x, y: smallint);
procedure setXY0(var p: POLY_G4; const x, y: smallint);
procedure setXY0(var p: POLY_GT3; const x, y: smallint);
procedure setXY0(var p: POLY_GT4; const x, y: smallint);
procedure setXY0(var p: LINE_F2; const x, y: smallint);
procedure setXY0(var p: LINE_G2; const x, y: smallint);	
procedure setXY0(var p: LINE_F3; const x, y: smallint);
procedure setXY0(var p: LINE_G3; const x, y: smallint);
procedure setXY0(var p: LINE_F4; const x, y: smallint);
procedure setXY0(var p: LINE_G4; const x, y: smallint);
procedure setXY0(var p: SPRT; const x, y: smallint);
procedure setXY0(var p: SPRT_16; const x, y: smallint);
procedure setXY0(var p: SPRT_8; const x, y: smallint);
procedure setXY0(var p: TILE; const x, y: smallint);
procedure setXY0(var p: TILE_16; const x, y: smallint);
procedure setXY0(var p: TILE_8; const x, y: smallint);
procedure setXY0(var p: TILE_1; const x, y: smallint);

procedure setXY2(var p: POLY_F3; const x0, y0, x1, y1: smallint);
procedure setXY2(var p: POLY_F4; const x0, y0, x1, y1: smallint);
procedure setXY2(var p: POLY_FT3; const x0, y0, x1, y1: smallint);
procedure setXY2(var p: POLY_FT4; const x0, y0, x1, y1: smallint);
procedure setXY2(var p: POLY_G3; const x0, y0, x1, y1: smallint);
procedure setXY2(var p: POLY_G4; const x0, y0, x1, y1: smallint);
procedure setXY2(var p: POLY_GT3; const x0, y0, x1, y1: smallint);
procedure setXY2(var p: POLY_GT4; const x0, y0, x1, y1: smallint);
procedure setXY2(var p: LINE_F2; const x0, y0, x1, y1: smallint);
procedure setXY2(var p: LINE_G2; const x0, y0, x1, y1: smallint);
procedure setXY2(var p: LINE_F3; const x0, y0, x1, y1: smallint);
procedure setXY2(var p: LINE_G3; const x0, y0, x1, y1: smallint);
procedure setXY2(var p: LINE_F4; const x0, y0, x1, y1: smallint);
procedure setXY2(var p: LINE_G4; const x0, y0, x1, y1: smallint);

procedure setXY3(var p: POLY_F3; const x0, y0, x1, y1, x2, y2: smallint);
procedure setXY3(var p: POLY_F4; const x0, y0, x1, y1, x2, y2: smallint);
procedure setXY3(var p: POLY_FT3; const x0, y0, x1, y1, x2, y2: smallint);
procedure setXY3(var p: POLY_FT4; const x0, y0, x1, y1, x2, y2: smallint);
procedure setXY3(var p: POLY_G3; const x0, y0, x1, y1, x2, y2: smallint);
procedure setXY3(var p: POLY_G4; const x0, y0, x1, y1, x2, y2: smallint);
procedure setXY3(var p: POLY_GT3; const x0, y0, x1, y1, x2, y2: smallint);
procedure setXY3(var p: POLY_GT4; const x0, y0, x1, y1, x2, y2: smallint);
procedure setXY3(var p: LINE_F3; const x0, y0, x1, y1, x2, y2: smallint);
procedure setXY3(var p: LINE_G3; const x0, y0, x1, y1, x2, y2: smallint);
procedure setXY3(var p: LINE_F4; const x0, y0, x1, y1, x2, y2: smallint);
procedure setXY3(var p: LINE_G4; const x0, y0, x1, y1, x2, y2: smallint);

procedure setXY4(var p: POLY_F4; const x0, y0, x1, y1, x2, y2, x3, y3: smallint);
procedure setXY4(var p: POLY_FT4; const x0, y0, x1, y1, x2, y2, x3, y3: smallint);
procedure setXY4(var p: POLY_G4; const x0, y0, x1, y1, x2, y2, x3, y3: smallint);
procedure setXY4(var p: POLY_GT4; const x0, y0, x1, y1, x2, y2, x3, y3: smallint);


procedure setXYWH(var p: POLY_F4; x0, y0, w, h: smallint);
procedure setXYWH(var p: POLY_FT4; x0, y0, w, h: smallint);
procedure setXYWH(var p: POLY_G4; x0, y0, w, h: smallint);
procedure setXYWH(var p: POLY_GT4; x0, y0, w, h: smallint);
procedure setXYWH(var p: LINE_F4; x0, y0, w, h: smallint);
procedure setXYWH(var p: LINE_G4; x0, y0, w, h: smallint);

// Set Primitive Width/Height
procedure setWH(var p: RECT; w, h: smallint);
procedure setWH(var p: RECT32; w, h: longint);
procedure setWH(var p: SPRT; w, h: smallint);
procedure setWH(var p: TILE; w, h: smallint);


// Set Primitive Texture Points
procedure setUV0(var p: POLY_FT3; u0, v0: smallint);
procedure setUV0(var p: POLY_FT4; u0, v0: smallint);
procedure setUV0(var p: POLY_GT3; u0, v0: smallint);
procedure setUV0(var p: POLY_GT4; u0, v0: smallint);
procedure setUV0(var p: SPRT; u0, v0: byte);
procedure setUV0(var p: SPRT_16; u0, v0: byte);
procedure setUV0(var p: SPRT_8; u0, v0: byte);
procedure setUV0(var p: TMD_PRIM; u0, v0: byte);
	
procedure setUV3(var p: POLY_FT3; u0, v0, u1, v1, u2, v2: smallint);
procedure setUV3(var p: POLY_FT4; u0, v0, u1, v1, u2, v2: smallint);
procedure setUV3(var p: POLY_GT3; u0, v0, u1, v1, u2, v2: smallint);
procedure setUV3(var p: POLY_GT4; u0, v0, u1, v1, u2, v2: smallint);
procedure setUV3(var p: TMD_PRIM; u0, v0, u1, v1, u2, v2: byte);

procedure setUV4(var p: POLY_FT4; u0, v0, u1, v1, u2, v2, u3, v3: smallint);
procedure setUV4(var p: POLY_GT4; u0, v0, u1, v1, u2, v2, u3, v3: smallint);
procedure setUV4(var p: TMD_PRIM; u0, v0, u1, v1, u2, v2, u3, v3: byte);

procedure setUVWH(var p: POLY_FT4; u0, v0, w, h: smallint);
procedure setUVWH(var p: POLY_GT4; u0, v0, w, h: smallint);
procedure setUVWH(var p: TMD_PRIM; u0, v0, w, h: smallint);
	

// Dump Primivie Parameters
procedure dumpRECT(r: RECT);

procedure dumpWH(p: RECT);
procedure dumpWH(p: RECT32);
procedure dumpWH(p: SPRT);
procedure dumpWH(p: TILE);


procedure dumpXY0(p: POLY_F3);
procedure dumpXY0(p: POLY_F4);
procedure dumpXY0(p: POLY_FT3);
procedure dumpXY0(p: POLY_FT4);
procedure dumpXY0(p: POLY_G3);
procedure dumpXY0(p: POLY_G4);
procedure dumpXY0(p: POLY_GT3);
procedure dumpXY0(p: POLY_GT4);
procedure dumpXY0(p: LINE_F2);
procedure dumpXY0(p: LINE_G2);
procedure dumpXY0(p: LINE_F3);
procedure dumpXY0(p: LINE_G3);
procedure dumpXY0(p: LINE_F4);
procedure dumpXY0(p: LINE_G4);
procedure dumpXY0(p: SPRT);
procedure dumpXY0(p: SPRT_16);
procedure dumpXY0(p: SPRT_8);
procedure dumpXY0(p: TILE);
procedure dumpXY0(p: TILE_16);
procedure dumpXY0(p: TILE_8);
procedure dumpXY0(p: TILE_1);




procedure dumpUV0(p: POLY_FT3);
procedure dumpUV0(p: POLY_FT4);
procedure dumpUV0(p: POLY_GT3);
procedure dumpUV0(p: POLY_GT4);
procedure dumpUV0(p: SPRT);
procedure dumpUV0(p: SPRT_16);
procedure dumpUV0(p: SPRT_8);
procedure dumpUV0(p: TMD_PRIM);


procedure dumpXY2(p: POLY_F3);
procedure dumpXY2(p: POLY_F4);
procedure dumpXY2(p: POLY_FT3);
procedure dumpXY2(p: POLY_FT4);
procedure dumpXY2(p: POLY_G3);
procedure dumpXY2(p: POLY_G4);
procedure dumpXY2(p: POLY_GT3);
procedure dumpXY2(p: POLY_GT4);
procedure dumpXY2(p: LINE_F2);
procedure dumpXY2(p: LINE_G2);
procedure dumpXY2(p: LINE_F3);
procedure dumpXY2(p: LINE_G3);
procedure dumpXY2(p: LINE_F4);
procedure dumpXY2(p: LINE_G4);



procedure dumpXY3(p: POLY_F3);
procedure dumpXY3(p: POLY_F4);
procedure dumpXY3(p: POLY_FT3);
procedure dumpXY3(p: POLY_FT4);
procedure dumpXY3(p: POLY_G3);
procedure dumpXY3(p: POLY_G4);
procedure dumpXY3(p: POLY_GT3);
procedure dumpXY3(p: POLY_GT4);
procedure dumpXY3(p: LINE_F3);
procedure dumpXY3(p: LINE_G3);
procedure dumpXY3(p: LINE_F4);
procedure dumpXY3(p: LINE_G4);


procedure dumpUV3(p: POLY_FT3);
procedure dumpUV3(p: POLY_FT4);
procedure dumpUV3(p: POLY_GT3);
procedure dumpUV3(p: POLY_GT4);
procedure dumpUV3(p: TMD_PRIM);

procedure dumpXY4(p: POLY_F4);
procedure dumpXY4(p: POLY_FT4);
procedure dumpXY4(p: POLY_G4);
procedure dumpXY4(p: POLY_GT4);

procedure dumpUV4(p: POLY_FT4);
procedure dumpUV4(p: POLY_GT4);
procedure dumpUV4(p: TMD_PRIM);


procedure dumpRGB0(p: POLY_F3);

procedure dumpRGB0(p: POLY_F4);
procedure dumpRGB0(p: POLY_FT3);
procedure dumpRGB0(p: POLY_FT4);
procedure dumpRGB0(p: POLY_G3);
procedure dumpRGB0(p: POLY_G4);
procedure dumpRGB0(p: POLY_GT3);
procedure dumpRGB0(p: POLY_GT4);
procedure dumpRGB0(p: DRAWENV);
		   
procedure dumpRGB1(p:POLY_G3);
procedure dumpRGB1(p:POLY_G4);
procedure dumpRGB1(p:POLY_GT3);
procedure dumpRGB1(p:POLY_GT4);

procedure dumpRGB2(p: POLY_G3);
procedure dumpRGB2(p: POLY_G4);
procedure dumpRGB2(p: POLY_GT3);
procedure dumpRGB2(p: POLY_GT4);

procedure dumpRGB3(p: POLY_G4);
procedure dumpRGB3(p: POLY_GT4);	


// Primitive Handling Macros
procedure setlen(var p: P_TAG; const len: byte);
procedure setlen(var p: POLY_F3; const len: byte);
procedure setlen(var p: POLY_FT3; const len: byte);
procedure setlen(var p: POLY_G3; const len: byte);
procedure setlen(var p: POLY_GT3; const len: byte);
procedure setlen(var p: POLY_F4; const len: byte);
procedure setlen(var p: POLY_FT4; const len: byte);
procedure setlen(var p: POLY_G4; const len: byte);
procedure setlen(var p: POLY_GT4; const len: byte);
procedure setlen(var p: SPRT_8; const len: byte);
procedure setlen(var p: SPRT_16; const len: byte);
procedure setlen(var p: SPRT; const len: byte);
procedure setlen(var p: TILE_1; const len: byte);
procedure setlen(var p: TILE_8; const len: byte);
procedure setlen(var p: TILE_16; const len: byte);
procedure setlen(var p: TILE; const len: byte);
procedure setlen(var p: LINE_F2; const len: byte);
procedure setlen(var p: LINE_G2; const len: byte);
procedure setlen(var p: LINE_F3; const len: byte);
procedure setlen(var p: LINE_G3; const len: byte);
procedure setlen(var p: LINE_F4; const len: byte);
procedure setlen(var p: LINE_G4; const len: byte);


procedure setaddr(var p: P_TAG; const addr: dword);



procedure setcode(var p: P_TAG; const code: byte);
procedure setcode(var p: POLY_F3; const code: byte);
procedure setcode(var p: POLY_FT3; const code: byte);
procedure setcode(var p: POLY_G3; const code: byte);
procedure setcode(var p: POLY_GT3; const code: byte);
procedure setcode(var p: POLY_F4; const code: byte);
procedure setcode(var p: POLY_FT4; const code: byte);
procedure setcode(var p: POLY_G4; const code: byte);
procedure setcode(var p: POLY_GT4; const code: byte);
procedure setcode(var p: SPRT_8; const code: byte);
procedure setcode(var p: SPRT_16; const code: byte);
procedure setcode(var p: SPRT; const code: byte);
procedure setcode(var p: TILE_1; const code: byte);
procedure setcode(var p: TILE_8; const code: byte);
procedure setcode(var p: TILE_16; const code: byte);
procedure setcode(var p: TILE; const code: byte);
procedure setcode(var p: LINE_F2; const code: byte);
procedure setcode(var p: LINE_G2; const code: byte);
procedure setcode(var p: LINE_F3; const code: byte);
procedure setcode(var p: LINE_G3; const code: byte);
procedure setcode(var p: LINE_F4; const code: byte);
procedure setcode(var p: LINE_G4; const code: byte);


function getlen(p: P_TAG): byte;
function getcode(p: P_TAG): byte;
function getaddr(p: P_TAG): dword;


{
#define nextPrim(p)  		(void *)((((P_TAG *)(p))->addr)|0x80000000)
#define isendprim(p) 		((((P_TAG *)(p))->addr)==0xffffff)

#define addPrim(ot, p)		setaddr(p, getaddr(ot)), setaddr(ot, p)
#define addPrims(ot, p0, p1)	setaddr(p1, getaddr(ot)),setaddr(ot, p0)

#define catPrim(p0, p1)		setaddr(p0, p1)
#define termPrim(p)		setaddr(p, 0xffffffff)

#define setSemiTrans(p, abe) \
	((abe)?setcode(p, getcode(p)|0x02):setcode(p, getcode(p)&~0x02))

#define setShadeTex(p, tge) \
	((tge)?setcode(p, getcode(p)|0x01):setcode(p, getcode(p)&~0x01))
}
{
function getTPage(tp, abr, x, y: longint): longint;
	 ((((tp)&0x3)<<7)|(((abr)&0x3)<<5)|(((y)&0x100)>>4)|(((x)&0x3ff)>>6)| \
	 (((y)&0x200)<<2))

function getClut(x, y): longint;
	(((y)<<6)|(((x)>>4)&0x3f))
}

procedure dumpTPage(tpage: longint);

procedure dumpClut(clut: longint);

{
#define _get_mode(dfe, dtd, tpage)	\
		((0xe1000000)|((dtd)?0x0200:0)| \
		((dfe)?0x0400:0)|((tpage)&0x9ff))

#define setDrawTPage(p, dfe, dtd, tpage)	\
	setlen(p, 1),	\
	((u_long *)(p))[1] = _get_mode(dfe, dtd, tpage)

#define _get_tw(tw)	\
		(tw ? ((0xe2000000)|((((tw)->y&0xff)>>3)<<15)| \
		((((tw)->x&0xff)>>3)<<10)|(((~((tw)->h-1)&0xff)>>3)<<5)| \
		(((~((tw)->w-1)&0xff)>>3))) : 0)

#define setTexWindow(p, tw)			\
	setlen(p, 2),				\
	((u_long *)(p))[1] = _get_tw(tw),	\
	((u_long *)(p))[2] = 0

}
{
#define _get_len(rect)	\
		(((rect)->w*(rect)->h+1)/2+4)

#define setDrawLoad(pt, rect)					\
	(_get_len(rect) <= 16) ? (				\
		(setlen(pt, _get_len(rect))),			\
		((pt)->code[0] = 0xa0000000),			\
		((pt)->code[1] = *((u_long *)&(rect)->x)),	\
		((pt)->code[2] = *((u_long *)&(rect)->w)),	\
		((pt)->p[_get_len(rect)-4] = 0x01000000)	\
	) : ( \
		(setlen(pt,0)) \
	)

#define setDrawStp(p, pbw) 				\
        setlen(p, 2),					\
        ((u_long *)p)[1] = 0xe6000000|(pbw?0x01:0),	\
        ((u_long *)p)[2] = 0

#define setDrawMode(p, dfe, dtd, tpage, tw) 		\
        setlen(p, 2),					\
        ((u_long *)p)[1] = _get_mode(dfe, dtd, tpage),	\
        ((u_long *)p)[2] = _get_tw((RECT *)tw)

	}
//	Primitive 	Lentgh		Code			
//--------------------------------------------------------------------
//								

procedure setPolyF3(var p: POLY_F3);
procedure setPolyFT3(var p: POLY_FT3);
procedure setPolyG3(var p: POLY_G3);
procedure setPolyGT3(var p: POLY_GT3);
procedure setPolyF4(var p: POLY_F4);
procedure setPolyFT4(var p: POLY_FT4);
procedure setPolyG4(var p: POLY_G4);
procedure setPolyGT4(var p: POLY_GT4);
procedure setSprt8(var p: SPRT_8);
procedure setSprt16(var p: SPRT_16);
procedure setSprt(var p: SPRT);
//procedure setTile1(var p: TILE_1);
procedure setTile8(var p: TILE_8);
procedure setTile16(var p: TILE_16);
procedure setTile(var p: TILE);
procedure setLineF2(var p: LINE_F2);
procedure setLineG2(var p: LINE_G2);
procedure setLineF3(var p: LINE_F3);
procedure setLineG3(var p: LINE_G3);
procedure setLineF4(var p: LINE_F4);
procedure setLineG4(var p: LINE_G4);


implementation


procedure limitRange(var x: longint; l, h: longint); inline;
begin
  if x < l then x := l else if x > h then x := h;
end;


procedure setVector(var v: VECTOR; const x, y, z: longint);
begin
	v.vx:= x;
	v.vy:= y;
	v.vz:= z;
end;

procedure setVector(var v: SVECTOR; const x, y, z: smallint);
begin
	v.vx:= x;
	v.vy:= y;
	v.vz:= z;
end;

procedure setVector(var v: DVECTOR; const x, y, z: smallint);
begin
	v.vx:= x;
	v.vy:= y;
end;


procedure copyVector(var v0: VECTOR; const v1: VECTOR);
begin
	v0:= v1;
end;

procedure copyVector(var v0: SVECTOR; const v1: SVECTOR);
begin
	v0:= v1;
end;

procedure copyVector(var v0: DVECTOR; const v1: DVECTOR);
begin
	v0:= v1;
end;


procedure addVector(var v0: VECTOR; const v1: VECTOR);
begin
	v0.vx:= v0.vx + v1.vx;
	v0.vy:= v0.vy + v1.vy;
	v0.vz:= v0.vz + v1.vz;
end;

procedure addVector(var v0: SVECTOR; const v1: SVECTOR);
begin
	v0.vx:= v0.vx + v1.vx;
	v0.vy:= v0.vy + v1.vy;
	v0.vz:= v0.vz + v1.vz;
end;

procedure addVector(var v0: DVECTOR; const v1: DVECTOR);
begin
	v0.vx:= v0.vx + v1.vx;
	v0.vy:= v0.vy + v1.vy;
end;


procedure setRECT(var r: RECT; const x, y, w, h: longint);
begin
	r.x:= x;
	r.y:= y;
	r.w:= w;
	r.h:= h;
end;


procedure dumpMatrix(const x: MATRIX);
begin
	GPU_printf('\t%5d,%5d,%5d\n', x.m[0,0], x.m[0,1], x.m[0,2]);
	GPU_printf('\t%5d,%5d,%5d\n', x.m[1,0], x.m[1,1], x.m[1,2]);
	GPU_printf('\t%5d,%5d,%5d\n', x.m[2,0], x.m[2,1], x.m[2,2]);
end;


procedure dumpVector(str: pchar; const v: VECTOR);
begin
	GPU_printf('%s=(%d,%d,%d)\n', str, v.vx, v.vy, v.vz)
end;

procedure dumpVector(str: pchar; const v: SVECTOR);
begin
	GPU_printf('%s=(%d,%d,%d)\n', str, v.vx, v.vy, v.vz)
end;

procedure dumpVector(str: pchar; const v: DVECTOR);
begin
	GPU_printf('%s=(%d,%d)\n', str, v.vx, v.vy)
end;



procedure setRGB0(var p: POLY_F3; const r, g, b: byte);
begin
	p.r0:= r;
	p.g0:= g;
	p.b0:= b;
end;

procedure setRGB0(var p: POLY_F4; const r, g, b: byte);
begin
	p.r0:= r;
	p.g0:= g;
	p.b0:= b;
end;


procedure setRGB0(var p: POLY_FT3; const r, g, b: byte);
begin
	p.r0:= r;
	p.g0:= g;
	p.b0:= b;
end;


procedure setRGB0(var p: POLY_FT4; const r, g, b: byte);
begin
	p.r0:= r;
	p.g0:= g;
	p.b0:= b;
end;


procedure setRGB0(var p: POLY_G3; const r, g, b: byte);
begin
	p.r0:= r;
	p.g0:= g;
	p.b0:= b;
end;


procedure setRGB0(var p: POLY_G4; const r, g, b: byte);
begin
	p.r0:= r;
	p.g0:= g;
	p.b0:= b;
end;


procedure setRGB0(var p: POLY_GT3; const r, g, b: byte);
begin
	p.r0:= r;
	p.g0:= g;
	p.b0:= b;
end;


procedure setRGB0(var p: POLY_GT4; const r, g, b: byte);
begin
	p.r0:= r;
	p.g0:= g;
	p.b0:= b;
end;

procedure setRGB0(var c: DRAWENV; r, g, b: byte);
begin
	c.r0:=r;
	c.g0:=g;
	c.b0:=b;
end;




procedure setRGB1(var p: POLY_G3; const r, g, b: byte);
begin
	p.r1:= r;
	p.g1:= g;
	p.b1:= b;
end;


procedure setRGB1(var p: POLY_G4; const r, g, b: byte);
begin
	p.r1:= r;
	p.g1:= g;
	p.b1:= b;
end;


procedure setRGB1(var p: POLY_GT3; const r, g, b: byte);
begin
	p.r1:= r;
	p.g1:= g;
	p.b1:= b;
end;


procedure setRGB1(var p: POLY_GT4; const r, g, b: byte);
begin
	p.r1:= r;
	p.g1:= g;
	p.b1:= b;
end;


procedure setRGB2(var p: POLY_G3; const r, g, b: byte);
begin
	p.r2:= r;
	p.g2:= g;
	p.b2:= b;
end;


procedure setRGB2(var p: POLY_G4; const r, g, b: byte);
begin
	p.r2:= r;
	p.g2:= g;
	p.b2:= b;
end;


procedure setRGB2(var p: POLY_GT3; const r, g, b: byte);
begin
	p.r2:= r;
	p.g2:= g;
	p.b2:= b;
end;


procedure setRGB2(var p: POLY_GT4; const r, g, b: byte);
begin
	p.r2:= r;
	p.g2:= g;
	p.b2:= b;
end;


procedure setRGB3(var p: POLY_G4; const r, g, b: byte);
begin
	p.r3:= r;
	p.g3:= g;
	p.b3:= b;
end;


procedure setRGB3(var p: POLY_GT4; const r, g, b: byte);
begin
	p.r3:= r;
	p.g3:= g;
	p.b3:= b;
end;


procedure setXY0(var p: POLY_F3; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: POLY_F4; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: POLY_FT3; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: POLY_FT4; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: POLY_G3; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: POLY_G4; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: POLY_GT3; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: POLY_GT4; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: LINE_F2; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: LINE_G2; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: LINE_F3; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: LINE_G3; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: LINE_F4; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;


procedure setXY0(var p: LINE_G4; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: SPRT; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: SPRT_16; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: SPRT_8; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: TILE; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: TILE_16; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: TILE_8; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;

procedure setXY0(var p: TILE_1; const x, y: smallint);
begin
	p.x0:= x;
	p.y0:= y;
end;


procedure setXY2(var p: POLY_F3; const x0, y0, x1, y1: smallint);
begin
	p.x1:= x0;
	p.y1:= y0;
	p.x2:= x1;
	p.y2:= y1;
end;


procedure setXY2(var p: POLY_F4; const x0, y0, x1, y1: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
end;


procedure setXY2(var p: POLY_FT3; const x0, y0, x1, y1: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
end;

procedure setXY2(var p: POLY_FT4; const x0, y0, x1, y1: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
end;


procedure setXY2(var p: POLY_G3; const x0, y0, x1, y1: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
end;


procedure setXY2(var p: POLY_G4; const x0, y0, x1, y1: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
end;


procedure setXY2(var p: POLY_GT3; const x0, y0, x1, y1: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
end;


procedure setXY2(var p: POLY_GT4; const x0, y0, x1, y1: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
end;


procedure setXY2(var p: LINE_F2; const x0, y0, x1, y1: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
end;

procedure setXY2(var p: LINE_G2; const x0, y0, x1, y1: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
end;

procedure setXY2(var p: LINE_F3; const x0, y0, x1, y1: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
end;

procedure setXY2(var p: LINE_G3; const x0, y0, x1, y1: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
end;

procedure setXY2(var p: LINE_F4; const x0, y0, x1, y1: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
end;

procedure setXY2(var p: LINE_G4; const x0, y0, x1, y1: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
end;

procedure setXY3(var p: LINE_F3; const x0, y0, x1, y1, x2, y2: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
end;

procedure setXY3(var p: LINE_G3; const x0, y0, x1, y1, x2, y2: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
end;

procedure setXY3(var p: LINE_F4; const x0, y0, x1, y1, x2, y2: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
end;

procedure setXY3(var p: LINE_G4; const x0, y0, x1, y1, x2, y2: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
end;


procedure setXY3(var p: POLY_F3; const x0, y0, x1, y1, x2, y2: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
end;


procedure setXY3(var p: POLY_F4; const x0, y0, x1, y1, x2, y2: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
end;


procedure setXY3(var p: POLY_FT3; const x0, y0, x1, y1, x2, y2: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
end;


procedure setXY3(var p: POLY_FT4; const x0, y0, x1, y1, x2, y2: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
end;


procedure setXY3(var p: POLY_G3; const x0, y0, x1, y1, x2, y2: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
end;


procedure setXY3(var p: POLY_G4; const x0, y0, x1, y1, x2, y2: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
end;


procedure setXY3(var p: POLY_GT3; const x0, y0, x1, y1, x2, y2: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
end;

procedure setXY3(var p: POLY_GT4; const x0, y0, x1, y1, x2, y2: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
end;


procedure setXY4(var p: POLY_F4; const x0, y0, x1, y1, x2, y2, x3, y3: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
	p.x3:= x3;
	p.y3:= y3;
end;

procedure setXY4(var p: POLY_FT4; const x0, y0, x1, y1, x2, y2, x3, y3: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
	p.x3:= x3;
	p.y3:= y3;
end;


procedure setXY4(var p: POLY_G4; const x0, y0, x1, y1, x2, y2, x3, y3: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
	p.x3:= x3;
	p.y3:= y3;
end;


procedure setXY4(var p: POLY_GT4; const x0, y0, x1, y1, x2, y2, x3, y3: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x1;
	p.y1:= y1;
	p.x2:= x2;
	p.y2:= y2;
	p.x3:= x3;
	p.y3:= y3;
end;


procedure setXYWH(var p: POLY_F4; x0, y0, w, h: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x0 + w;
	p.y1:= y0;	
	p.x2:= x0;
	p.y2:= y0 + h;
	p.x3:= x0 + w;
	p.y3:= y0 + h;
end;


procedure setXYWH(var p: POLY_FT4; x0, y0, w, h: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x0 + w;
	p.y1:= y0;	
	p.x2:= x0;
	p.y2:= y0 + h;
	p.x3:= x0 + w;
	p.y3:= y0 + h;
end;


procedure setXYWH(var p: POLY_G4; x0, y0, w, h: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x0 + w;
	p.y1:= y0;	
	p.x2:= x0;
	p.y2:= y0 + h;
	p.x3:= x0 + w;
	p.y3:= y0 + h;
end;


procedure setXYWH(var p: POLY_GT4; x0, y0, w, h: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x0 + w;
	p.y1:= y0;	
	p.x2:= x0;
	p.y2:= y0 + h;
	p.x3:= x0 + w;
	p.y3:= y0 + h;
end;


procedure setXYWH(var p: LINE_F4; x0, y0, w, h: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x0 + w;
	p.y1:= y0;	
	p.x2:= x0;
	p.y2:= y0 + h;
	p.x3:= x0 + w;
	p.y3:= y0 + h;
end;


procedure setXYWH(var p: LINE_G4; x0, y0, w, h: smallint);
begin
	p.x0:= x0;
	p.y0:= y0;
	p.x1:= x0 + w;
	p.y1:= y0;	
	p.x2:= x0;
	p.y2:= y0 + h;
	p.x3:= x0 + w;
	p.y3:= y0 + h;
end;



procedure setWH(var p: RECT; w, h: smallint);
begin
	p.w:= w;
	p.h:= h;
end;


procedure setWH(var p: RECT32; w, h: longint);
begin
	p.w:= w;
	p.h:= h;
end;


procedure setWH(var p: SPRT; w, h: smallint);
begin
	p.w:= w;
	p.h:= h;
end;


procedure setWH(var p: TILE; w, h: smallint);
begin
	p.w:= w;
	p.h:= h;
end;


procedure setUV0(var p: POLY_FT3; u0, v0: smallint);
begin
	p.u0:= u0;
	p.v0:= v0;
end;


procedure setUV0(var p: POLY_FT4; u0, v0: smallint);
begin
	p.u0:= u0;
	p.v0:= v0;
end;


procedure setUV0(var p: POLY_GT3; u0, v0: smallint);
begin
	p.u0:= u0;
	p.v0:= v0;
end;


procedure setUV0(var p: POLY_GT4; u0, v0: smallint);
begin
	p.u0:= u0;
	p.v0:= v0;
end;


procedure setUV0(var p: SPRT; u0, v0: byte);
begin
	p.u0:= u0;
	p.v0:= v0;
end;


procedure setUV0(var p: SPRT_16; u0, v0: byte);
begin
	p.u0:= u0;
	p.v0:= v0;
end;


procedure setUV0(var p: SPRT_8; u0, v0: byte);
begin
	p.u0:= u0;
	p.v0:= v0;
end;


procedure setUV0(var p: TMD_PRIM; u0, v0: byte);
begin
	p.u0:= u0;
	p.v0:= v0;
end;


procedure setUV3(var p: POLY_FT3; u0, v0, u1, v1, u2, v2: smallint);
begin
	p.u0:= u0;
	p.v0:= v0;
	p.u1:= u1;
	p.v1:= v1;
	p.u2:= u2;
	p.v2:= v2;
end;


procedure setUV3(var p: POLY_FT4; u0, v0, u1, v1, u2, v2: smallint);
begin
	p.u0:= u0;
	p.v0:= v0;
	p.u1:= u1;
	p.v1:= v1;
	p.u2:= u2;
	p.v2:= v2;
end;


procedure setUV3(var p: POLY_GT3; u0, v0, u1, v1, u2, v2: smallint);
begin
	p.u0:= u0;
	p.v0:= v0;
	p.u1:= u1;
	p.v1:= v1;
	p.u2:= u2;
	p.v2:= v2;
end;


procedure setUV3(var p: POLY_GT4; u0, v0, u1, v1, u2, v2: smallint);
begin
	p.u0:= u0;
	p.v0:= v0;
	p.u1:= u1;
	p.v1:= v1;
	p.u2:= u2;
	p.v2:= v2;
end;


procedure setUV3(var p: TMD_PRIM; u0, v0, u1, v1, u2, v2: byte);
begin
	p.u0:= u0;
	p.v0:= v0;
	p.u1:= u1;
	p.v1:= v1;
	p.u2:= u2;
	p.v2:= v2;
end;


procedure setUV4(var p: POLY_FT4; u0, v0, u1, v1, u2, v2, u3, v3: smallint);
begin
	p.u0:= u0;
	p.v0:= v0;
	p.u1:= u1;
	p.v1:= v1;
	p.u2:= u2;
	p.v2:= v2;
	p.u3:= u3;
	p.v3:= v3;
end;


procedure setUV4(var p: POLY_GT4; u0, v0, u1, v1, u2, v2, u3, v3: smallint);
begin
	p.u0:= u0;
	p.v0:= v0;
	p.u1:= u1;
	p.v1:= v1;
	p.u2:= u2;
	p.v2:= v2;
	p.u3:= u3;
	p.v3:= v3;
end;


procedure setUV4(var p: TMD_PRIM; u0, v0, u1, v1, u2, v2, u3, v3: byte);
begin
	p.u0:= u0;
	p.v0:= v0;
	p.u1:= u1;
	p.v1:= v1;
	p.u2:= u2;
	p.v2:= v2;
	p.u3:= u3;
	p.v3:= v3;
end;


procedure setUVWH(var p: POLY_FT4; u0, v0, w, h: smallint);
begin
	p.u0:= u0;
	p.v0:= v0;
	p.u1:= u0 + w;
	p.v1:= v0;
	p.u2:= u0;
	p.v2:= v0 + h;
	p.u3:= u0 + w;
	p.v3:= v0 + h;
end;


procedure setUVWH(var p: POLY_GT4; u0, v0, w, h: smallint);
begin
	p.u0:= u0;
	p.v0:= v0;
	p.u1:= u0 + w;
	p.v1:= v0;
	p.u2:= u0;
	p.v2:= v0 + h;
	p.u3:= u0 + w;
	p.v3:= v0 + h;
end;


procedure setUVWH(var p: TMD_PRIM; u0, v0, w, h: smallint);
begin
	p.u0:= u0;
	p.v0:= v0;
	p.u1:= u0 + w;
	p.v1:= v0;
	p.u2:= u0;
	p.v2:= v0 + h;
	p.u3:= u0 + w;
	p.v3:= v0 + h;
end;


procedure dumpWH(p: RECT);
begin
	GPU_printf('(%d,%d)\n', p.w, p.h);
end;


procedure dumpWH(p: RECT32);begin
	GPU_printf('(%d,%d)\n', p.w, p.h);
end;

procedure dumpWH(p: SPRT);begin
	GPU_printf('(%d,%d)\n', p.w, p.h);
end;

procedure dumpWH(p: TILE);begin
	GPU_printf('(%d,%d)\n', p.w, p.h);
end;


procedure dumpXY0(p: POLY_F3);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;


procedure dumpXY0(p: POLY_F4);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;


procedure dumpXY0(p: POLY_FT3);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;


procedure dumpXY0(p: POLY_FT4);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;


procedure dumpXY0(p: POLY_G3);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;


procedure dumpXY0(p: POLY_G4);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;


procedure dumpXY0(p: POLY_GT3);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;


procedure dumpXY0(p: POLY_GT4);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;


procedure dumpXY0(p: LINE_F2);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;

procedure dumpXY0(p: LINE_G2);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;
	
procedure dumpXY0(p: LINE_F3);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;
	
procedure dumpXY0(p: LINE_G3);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;
	
procedure dumpXY0(p: LINE_F4);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;
	
procedure dumpXY0(p: LINE_G4);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;
	
procedure dumpXY0(p: SPRT);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;
	
procedure dumpXY0(p: SPRT_16);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;
	
procedure dumpXY0(p: SPRT_8);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;
	
procedure dumpXY0(p: TILE);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;
	
procedure dumpXY0(p: TILE_16);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;
	
procedure dumpXY0(p: TILE_8);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;
	
procedure dumpXY0(p: TILE_1);
begin
	GPU_printf('(%d,%d)\n', p.x0, p.y0);
end;
	


procedure dumpUV0(p: POLY_FT3);
begin
	GPU_printf('(%d,%d)\n', p.u0, p.v0);
end;


procedure dumpUV0(p: POLY_FT4);
begin
	GPU_printf('(%d,%d)\n', p.u0, p.v0);
end;

procedure dumpUV0(p: POLY_GT3);
begin
	GPU_printf('(%d,%d)\n', p.u0, p.v0);
end;

procedure dumpUV0(p: POLY_GT4);
begin
	GPU_printf('(%d,%d)\n', p.u0, p.v0);
end;

procedure dumpUV0(p: SPRT);
begin
	GPU_printf('(%d,%d)\n', p.u0, p.v0);
end;

procedure dumpUV0(p: SPRT_16);
begin
	GPU_printf('(%d,%d)\n', p.u0, p.v0);
end;

procedure dumpUV0(p: SPRT_8);
begin
	GPU_printf('(%d,%d)\n', p.u0, p.v0);
end;

procedure dumpUV0(p: TMD_PRIM);
begin
	GPU_printf('(%d,%d)\n', p.u0, p.v0);
end;

procedure dumpXY2(p: POLY_F3);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1);
end;

procedure dumpXY2(p: POLY_F4);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1);
end;

procedure dumpXY2(p: POLY_FT3);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1);
end;

procedure dumpXY2(p: POLY_FT4);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1);
end;

procedure dumpXY2(p: POLY_G3);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1);
end;

procedure dumpXY2(p: POLY_G4);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1);
end;

procedure dumpXY2(p: POLY_GT3);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1);
end;

procedure dumpXY2(p: POLY_GT4);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1);
end;

procedure dumpXY2(p: LINE_F2);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1);
end;

procedure dumpXY2(p: LINE_G2);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1);
end;

procedure dumpXY2(p: LINE_F3);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1);
end;

procedure dumpXY2(p: LINE_G3);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1);
end;

procedure dumpXY2(p: LINE_F4);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1);
end;

procedure dumpXY2(p: LINE_G4);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1);
end;


procedure dumpXY3(p: POLY_F3);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2);
end;


procedure dumpXY3(p: POLY_F4);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2);
end;


procedure dumpXY3(p: POLY_FT3);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2);
end;


procedure dumpXY3(p: POLY_FT4);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2);
end;


procedure dumpXY3(p: POLY_G3);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2);
end;


procedure dumpXY3(p: POLY_G4);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2);
end;


procedure dumpXY3(p: POLY_GT3);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2);
end;


procedure dumpXY3(p: POLY_GT4);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2);
end;

procedure dumpXY3(p: LINE_F3);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2);
end;

procedure dumpXY3(p: LINE_G3);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2);
end;

procedure dumpXY3(p: LINE_F4);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2);
end;

procedure dumpXY3(p: LINE_G4);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2);
end;

procedure dumpUV3(p: POLY_FT3);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.u0, p.v0, p.u1, p.v1, p.u2, p.v2);
end;


procedure dumpUV3(p: POLY_FT4);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.u0, p.v0, p.u1, p.v1, p.u2, p.v2);
end;

procedure dumpUV3(p: POLY_GT3);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.u0, p.v0, p.u1, p.v1, p.u2, p.v2);
end;

procedure dumpUV3(p: POLY_GT4);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.u0, p.v0, p.u1, p.v1, p.u2, p.v2);
end;

procedure dumpUV3(p: TMD_PRIM);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)\n', p.u0, p.v0, p.u1, p.v1, p.u2, p.v2);
end;


procedure dumpXY4(p: POLY_F4);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2, p.x3, p.y3);
end;

procedure dumpXY4(p: POLY_FT4);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2, p.x3, p.y3);
end;

procedure dumpXY4(p: POLY_G4);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2, p.x3, p.y3);
end;

procedure dumpXY4(p: POLY_GT4);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)-(%d,%d)\n', p.x0, p.y0, p.x1, p.y1, p.x2, p.y2, p.x3, p.y3);
end;

	
procedure dumpUV4(p: POLY_FT4);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)-(%d,%d)\n', p.u0, p.v0, p.u1, p.v1, p.u2, p.v2, p.u3, p.v3);
end;

procedure dumpUV4(p: POLY_GT4);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)-(%d,%d)\n', p.u0, p.v0, p.u1, p.v1, p.u2, p.v2, p.u3, p.v3);
end;

procedure dumpUV4(p: TMD_PRIM);
begin
	GPU_printf('(%d,%d)-(%d,%d)-(%d,%d)-(%d,%d)\n', p.u0, p.v0, p.u1, p.v1, p.u2, p.v2, p.u3, p.v3);
end;
	

procedure dumpRGB0(p: POLY_F3);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r0, p.g0, p.b0);
end;

procedure dumpRGB0(p: POLY_F4);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r0, p.g0, p.b0);
end;

procedure dumpRGB0(p: POLY_FT3);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r0, p.g0, p.b0);
end;

procedure dumpRGB0(p: POLY_FT4);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r0, p.g0, p.b0);
end;

procedure dumpRGB0(p: POLY_G3);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r0, p.g0, p.b0);
end;

procedure dumpRGB0(p: POLY_G4);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r0, p.g0, p.b0);
end;

procedure dumpRGB0(p: POLY_GT3);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r0, p.g0, p.b0);
end;

procedure dumpRGB0(p: POLY_GT4);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r0, p.g0, p.b0);
end;

procedure dumpRGB0(p: DRAWENV);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r0, p.g0, p.b0);
end;


procedure dumpRGB1(p:POLY_G3);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r1, p.g1, p.b1);
end;

procedure dumpRGB1(p:POLY_G4);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r1, p.g1, p.b1);
end;

procedure dumpRGB1(p:POLY_GT3);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r1, p.g1, p.b1);
end;

procedure dumpRGB1(p:POLY_GT4);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r1, p.g1, p.b1);
end;

procedure dumpRGB2(p: POLY_G3);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r2, p.g2, p.b2);
end;

procedure dumpRGB2(p: POLY_G4);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r2, p.g2, p.b2);
end;

procedure dumpRGB2(p: POLY_GT3);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r2, p.g2, p.b2);
end;

procedure dumpRGB2(p: POLY_GT4);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r2, p.g2, p.b2);
end;


procedure dumpRGB3(p: POLY_G4);
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r3, p.g3, p.b3);
end;

procedure dumpRGB3(p: POLY_GT4);	
begin
	GPU_printf('(%3d,%3d,%3d)\n', p.r3, p.g3, p.b3);
end;



procedure setTPage(var p: DRAWENV; tp, abr, x, y: longint);
begin
	p.tpage:= GetTPage(tp, abr, x, y);
end;


procedure setTPage(var p: POLY_FT3; tp, abr, x, y: longint);
begin
	p.tpage:= GetTPage(tp, abr, x, y);
end;


procedure setTPage(var p: POLY_FT4; tp, abr, x, y: longint);
begin
	p.tpage:= GetTPage(tp, abr, x, y);
end;


procedure setTPage(var p: POLY_GT3; tp, abr, x, y: longint);
begin
	p.tpage:= GetTPage(tp, abr, x, y);
end;


procedure setTPage(var p: POLY_GT4; tp, abr, x, y: longint);
begin
	p.tpage:= GetTPage(tp, abr, x, y);
end;


procedure setTPage(var p: TMD_PRIM; tp, abr, x, y: longint);
begin
	p.tpage:= GetTPage(tp, abr, x, y);
end;


procedure setClut(var p: POLY_FT3; x, y: longint);
begin
	p.clut:= getClut(x, y);
end;

procedure setClut(var p: POLY_FT4; x, y: longint);
begin
	p.clut:= getClut(x, y);
end;

procedure setClut(var p: POLY_GT3; x, y: longint);
begin
	p.clut:= getClut(x, y);
end;

procedure setClut(var p: POLY_GT4; x, y: longint);
begin
	p.clut:= getClut(x, y);
end;

procedure setClut(var p: SPRT; x, y: longint);
begin
	p.clut:= getClut(x, y);
end;

procedure setClut(var p: SPRT_16; x, y: longint);
begin
	p.clut:= getClut(x, y);
end;

procedure setClut(var p: SPRT_8; x, y: longint);
begin
	p.clut:= getClut(x, y);
end;

procedure setClut(var p: TMD_PRIM; x, y: longint);
begin
	p.clut:= getClut(x, y);
end;





procedure dumpRECT(r: RECT);
begin
	GPU_printf('(%d,%d)-(%d,%d)\n', r.x, r.y, r.w, r.h);
end;





procedure setlen(var p: P_TAG; const len: byte);
begin
 	p.len:= len;
end;

procedure setlen(var p: POLY_F3; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: POLY_FT3; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: POLY_G3; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: POLY_GT3; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: POLY_F4; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: POLY_FT4; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: POLY_G4; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: POLY_GT4; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: SPRT_8; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: SPRT_16; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: SPRT; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: TILE_1; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: TILE_8; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: TILE_16; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: TILE; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: LINE_F2; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: LINE_G2; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: LINE_F3; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: LINE_G3; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: LINE_F4; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;

procedure setlen(var p: LINE_G4; const len: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.len:= len;
end;







procedure setaddr(var p: P_TAG; const addr: dword);
begin
	p.addr:= addr;
end;

procedure setcode(var p: P_TAG; const code: byte);
begin
	p.code:= code;
end;


procedure setcode(var p: POLY_F3; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: POLY_FT3; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: POLY_G3; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: POLY_GT3; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: POLY_F4; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: POLY_FT4; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: POLY_G4; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: POLY_GT4; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: SPRT_8; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: SPRT_16; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: SPRT; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: TILE_1; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: TILE_8; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: TILE_16; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: TILE; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: LINE_F2; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: LINE_G2; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: LINE_F3; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: LINE_G3; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: LINE_F4; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;

procedure setcode(var p: LINE_G4; const code: byte);
var x : ^P_TAG;
begin
	x:= @p;
 	x^.code:= code;
end;


function getlen(p: P_TAG): byte;
begin
	getlen:= p.len;
end;

function getcode(p: P_TAG): byte;
begin
   	getcode:= p.code;
end;

function getaddr(p: P_TAG): dword;
begin
	getaddr:= p.addr;
end;





//	Primitive 	Lentgh		Code			
//--------------------------------------------------------------------
//								

//#define setPolyF3(p)	setlen(p, 4),  setcode(p, 0x20)
procedure setPolyF3(var p: POLY_F3);
begin
	setlen(p, 4);
	setcode(p, $20);
end;

//#define setPolyFT3(p)	setlen(p, 7),  setcode(p, 0x24)
procedure setPolyFT3(var p: POLY_FT3);
begin
	setlen(p, 7);
	setcode(p, $24);
end;

//#define setPolyG3(p)	setlen(p, 6),  setcode(p, 0x30)
procedure setPolyG3(var p: POLY_G3);
begin
	setlen(p, 6);
	setcode(p, $30);
end;

//#define setPolyGT3(p)	setlen(p, 9),  setcode(p, 0x34)
procedure setPolyGT3(var p: POLY_GT3);
begin
	setlen(p, 9);
	setcode(p, $34);
end;


//#define setPolyF4(p)	setlen(p, 5),  setcode(p, 0x28)
procedure setPolyF4(var p: POLY_F4);
begin
	setlen(p, 5);
	setcode(p, $28);
end;

//#define setPolyFT4(p)	setlen(p, 9),  setcode(p, 0x2c)
procedure setPolyFT4(var p: POLY_FT4);
begin
	setlen(p, 9);
	setcode(p, $2c);
end;


//#define setPolyG4(p)	setlen(p, 8),  setcode(p, 0x38)
procedure setPolyG4(var p: POLY_G4);
begin
	setlen(p, 8);
	setcode(p, $38);
end;

//#define setPolyGT4(p)	setlen(p, 12), setcode(p, 0x3c)
procedure setPolyGT4(var p: POLY_GT4);
begin
	setlen(p, 12);
	setcode(p, $3c);
end;


//#define setSprt8(p)	setlen(p, 3),  setcode(p, 0x74)
procedure setSprt8(var p: SPRT_8);
begin
	setlen(p, 3);
	setcode(p, $74);
end;

//#define setSprt16(p)	setlen(p, 3),  setcode(p, 0x7c)
procedure setSprt16(var p: SPRT_16);
begin
	setlen(p, 3);
	setcode(p, $7c);

end;

//#define setSprt(p)	setlen(p, 4),  setcode(p, 0x64)
procedure setSprt(var p: SPRT);
begin
	setlen(p, 4);
	setcode(p, $64);
end;

//#define setTile1(p)	setlen(p, 2),  setcode(p, 0x68)
{
procedure setTile1(var p: TILE_1);
begin
	setlen(p, 2);
	setcode(p, $68);
end;
}
//#define setTile8(p)	setlen(p, 2),  setcode(p, 0x70)
procedure setTile8(var p: TILE_8);
begin
	setlen(p, 2);
	setcode(p, $70);
end;

//#define setTile16(p)	setlen(p, 2),  setcode(p, 0x78)
procedure setTile16(var p: TILE_16);
begin
	setlen(p, 2);
	setcode(p, $78);
end;

//#define setTile(p)	setlen(p, 3),  setcode(p, 0x60)
procedure setTile(var p: TILE);
begin
	setlen(p, 3);
	setcode(p, $60);
end;

//#define setLineF2(p)	setlen(p, 3),  setcode(p, 0x40)
procedure setLineF2(var p: LINE_F2);
begin
	setlen(p, 3);
	setcode(p, $40);
end;

//#define setLineG2(p)	setlen(p, 4),  setcode(p, 0x50)
procedure setLineG2(var p: LINE_G2);
begin
	setlen(p, 4);
	setcode(p, $50);
end;

//#define setLineF3(p)	setlen(p, 5),  setcode(p, 0x48),(p)->pad = 0x55555555
procedure setLineF3(var p: LINE_F3);
begin
	setlen(p, 5);  
	setcode(p, $48);
	p.pad:= $55555555;
end;

//#define setLineG3(p)	setlen(p, 7),  setcode(p, 0x58),(p)->pad = 0x55555555, \
//			(p)->p2 = 0
procedure setLineG3(var p: LINE_G3);
begin
	setlen(p, 7);
	setcode(p, $58);
	p.pad:= $55555555;
	p.p2:= 0;
end;

//#define setLineF4(p)	setlen(p, 6),  setcode(p, 0x4c),(p)->pad = 0x55555555
procedure setLineF4(var p: LINE_F4);
begin
	setlen(p, 6);
	setcode(p, $4c);
	p.pad:= $55555555;
end;

//#define setLineG4(p)	setlen(p, 9),  setcode(p, 0x5c),(p)->pad = 0x55555555, \
//			(p)->p2 = 0, (p)->p3 = 0
procedure setLineG4(var p: LINE_G4);
begin
	setlen(p, 9);
	setcode(p, $5c);
	p.pad:= $55555555;
	p.p2:= 0;
	p.p3:= 0;
end;

{
function getTPage(tp, abr, x, y: longint): longint;
begin
	result:= ((((tp) and $3)shl 7)or(((abr) and $3) shl 5) or (((y) and $100) shr 4)|(((x)and $3ff) shr6) or (((y) and $200) shl 2));
end;

function getClut(x, y): longint;
begin
	result:= (((y) shl 6)or(((x) shr 4) and $3f));
end;
}
procedure dumpTPage(tpage: longint);
begin
	GPU_printf('tpage: (%d,%d,%d,%d)\n', ((tpage) shr 7) and $003, ((tpage) shr 5) and $003, ((tpage) shl 6) and $7c0, (((tpage) shl 4) and $100)+(((tpage)shr 2) and $200));
end;

procedure dumpClut(clut: longint);
begin
	GPU_printf('clut: (%d,%d)\n', (clut and $3f) shl 4, (clut shr 6));
end;


end.