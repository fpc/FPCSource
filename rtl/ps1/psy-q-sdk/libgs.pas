// libgs.h: Graphic Library Header 
unit libgs;
interface

type
		PACKET = byte;						// packet peripheral pointer

const
		PSBANK 			= $80000000;
		ZRESOLUTION     = $3fff;			// --- Zsort resolution ---

		WORLD 			= 0;

// function SCREEN ((GsCOORDINATE2 *)0x0001)

type

		GsCOORD2PARAM = packed record
							scale : VECTOR;
							rotate : SVECTOR;
							trans : VECTOR;
		end;
		GsCOORD2PARAM = ^GsCOORD2PARAM;

		PGsCOORDINATE2 = ^GsCOORDINATE2;
		GsCOORDINATE2 = packed record
							flg : dword;
							coord : MATRIX;
							workm : MATRIX;
							param : PGsCOORD2PARAM;
							super : PGsCOORDINATE2;
							_sub : PGsCOORDINATE2;
		end;

		
		GsVIEW2 = packed record
							view : MATRIX;
							super : GsCOORDINATE2;
		end;

		GsRVIEW2 = packed record
							vpx, vpy, vpz : longint;
							vrx, vry, vrz : longint;
							rz : longint;
							super : PGsCOORDINATE2;
		end;
       
		GsF_LIGHT = packed record
							vx, vy, vz : longint;
							r, g, b : byte;
		end;
       


		GsOT_TAG = packed record
							p:0..16777215
							num:0..255;
		end;


		PGsOT_TAG = ^GsOT_TAG;
		GsOT = packed record
							len : dword;
							org : PGsOT_TAG;
							offset : dword;
							point : dword;
							tag : PGsOT_TAG;
		end;

		GsDOBJ2 = packed record
							attr : dword;					// pers,trans,rotate,disp
							coord2 : PGsCOORDINATE2;		// local dmatrix
							tmd : pdword;
							id : dword;
		end;

		GsDOBJ3 = packed record
							attr : dword;					// pers,trans,rotate,disp
							coord2 : PGsCOORDINATE2;		// local dmatrix
							pmd : pdword;					// pmd top address
							base : pdword;					// object base address
							sv : pdword;					// shared vertex base
							id : dword;
		end;

		GsDOBJ4 = packed record
							attr : dword; 					// pers,trans,rotate,disp
							coord2 : PGsCOORDINATE2;		// local dmatrix
							tmd : pdword;
							id : dword;
		end;

		GsDOBJ5 = packed record
							attr : dword;
							coord2 : PGsCOORDINATE2;
							tmd : pdword;
							packet : pdword;
							id : dword;
		end;


		GsSPRITE = packed record
							attr : dword;
							x, y : smallint;
							w, h : word;
							tpage : word;
							u, v : byte;
							cx, cy : smallint;
							r, g, b : byte;
							mx, my : smallint;
							scalex, scaley : smallint;
							rotate : longint;
		end;


		GsCELL = packed record
							u, v : byte;
							cba : word;
							flag : word;
							tpage : word;
		end;
		PGsCell = ^GsCELL;

		GsMAP = packed record
							cellw, cellh : byte;
							ncellw, ncellh : word;
							base : PGsCELL;
							index : pword;
		end;
		PGsMAP = ^GsMAP;


		GsBG = packed record
							attr : dword;
							x, y : smallint;
							w, h : smallint;
							scrollx, scrolly : smallint;
							r, g, b : byte;
							map : PGsMAP;
							mx, my : smallint;
							scalex, scaley : smallint;
							rotate : longint;
		end;


		GsLINE = packed record
							attr : dword;
							x0, y0 : smallint;
							x1, y1 : smallint;
							r, g, b : byte;
		end;

		GsGLINE = packed record
							attr : dword;
							x0, y0 : smallint;
							x1, y1 : smallint;
							r0, g0, b0 : byte;
							r1, g1, b1 : byte;
		end;

		GsBOXF = packed record
							attr : dword;
							x, y : smallint;
							w, h : word;
							r, g, b : byte;
       	end;

       	GsFOGPARAM = packed record
							dqa : smallint;
							dqb : longint;
							rfc, gfc, bfc : byte;
       	end;


       	GsIMAGE = packed record
							pmode : dword;
							px, py : smallint;
							pw, ph : word;
							pixel : pointer;
							cx, cy : smallint;
							cw, ch : word;
							clut : pointer;
		end;


		_GsPOSITION = packed record
							offx, offy : smallint;
		end;


		GsOBJTABLE2 = packed record
							top : PGsDOBJ2;
							nobj : longint;
							maxobj : longint;
		end;

(*
typedef struct {
	PACKET
	* (*f3[2][3]) ();
	PACKET
	* (*nf3[2]) ();
	PACKET
	* (*g3[2][3]) ();
	PACKET
	* (*ng3[2]) ();
	PACKET
	* (*tf3[2][3]) ();
	PACKET
	* (*ntf3[2]) ();
	PACKET
	* (*tg3[2][3]) ();
	PACKET
	* (*ntg3[2]) ();
	PACKET
	* (*f4[2][3]) ();
	PACKET
	* (*nf4[2]) ();
	PACKET
	* (*g4[2][3]) ();
	PACKET
	* (*ng4[2]) ();
	PACKET
	* (*tf4[2][3]) ();
	PACKET
	* (*ntf4[2]) ();
	PACKET
	* (*tg4[2][3]) ();
	PACKET
	* (*ntg4[2]) ();
	PACKET
	* (*f3g[3])();
	PACKET
	* (*g3g[3])();
	PACKET
	* (*f4g[3])();
	PACKET
	* (*g4g[3])();
}       _GsFCALL;
*)


const
		GsDivMODE_NDIV  = 0;
		GsDivMODE_DIV   = 1;
		GsLMODE_NORMAL 	= 0;
		GsLMODE_FOG    	= 1;
		GsLMODE_LOFF   	= 2;

// libgs macro 
		GsOFSGTE 		= 0;
		GsOFSGPU 		= 4;
		GsINTER  		= 1;
		GsNONINTER 		= 0;
		GsRESET0 		= 0;
		GsRESET3 		= 3 shl 4;

// object attribute set macro 
		GsLDIM0 		= 0;
		GsLDIM1 		= 1;
		GsLDIM2 		= 2;
		GsLDIM3 		= 3;
		GsLDIM4 		= 4;
		GsLDIM5 		= 5;
		GsLDIM6 		= 6;
		GsLDIM7 		= 7;
		GsFOG   		= 1 shl 3;
		GsMATE  		= 1 shl 4;
		GsLLMOD 		= 1 shl 5;
		GsLOFF  		= 1 shl 6;
		GsZIGNR 		= 1 shl 7;
		GsNBACKC 		= 1 shl 8;
		GsDIV1   		= 1 shl 9;
		GsDIV2   		= 2 shl 9;
		GsDIV3   		= 3 shl 9;
		GsDIV4	 		= 4 shl 9;
		GsDIV5	 		= 5 shl 9;
		GsAZERO  		= 0 shl 28;
		GsAONE   		= 1 shl 28;
		GsATWO   		= 2 shl 28;
		GsATHREE 		= 3 shl 28;
		GsALON   		= 1 shl 30;
		GsDOFF   		= 1 shl 31;


// BG/sprite attribute set macro 

		GsPERS   		= 1 shl 26;
		GsROTOFF 		= 1 shl 27;
{
#define GsIncFrame()  (PSDCNT++, PSDCNT= PSDCNT?PSDCNT:1, \
                      (PSDIDX= (PSDIDX==0?1:0)))

#define GsUpdateCoord()  (PSDCNT++, PSDCNT= PSDCNT?PSDCNT:1)

#define GsSetAzwh(z,w,h)    GsADIVZ = (z),GsADIVW = (w),GsADIVH = (h);
}

		GsTMDFlagGRD	= $04;

// FLIP macro for GsSort[Fast]SpriteB
		GsHFLIP			= $01;
		GsVFLIP			= $02;


// TMD structure 
// GTE PACKET to-GPU command '<packet-name>.code'
		GPU_COM_F3    	= $20;
		GPU_COM_TF3   	= $24;
		GPU_COM_G3    	= $30;
		GPU_COM_TG3   	= $34;

		GPU_COM_F4    	= $28;
		GPU_COM_TF4   	= $2c;
		GPU_COM_G4    	= $38;
		GPU_COM_TG4   	= $3c;

		GPU_COM_NF3   	= $21;
		GPU_COM_NTF3  	= $25;
		GPU_COM_NG3   	= $31;
		GPU_COM_NTG3  	= $35;

		GPU_COM_NF4   	= $29;
		GPU_COM_NTF4  	= $2d;
		GPU_COM_NG4   	= $39;
		GPU_COM_NTG4  	= $3d;


// TMD structure
		TMD_P_F3 = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						n0, v0 : word;
						v1, v2 : word;
		end;

		TMD_P_G3 = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						n0, v0 : word;
						n1, v1 : word;
						n2, v2 : word;
		end;


		TMD_P_F3G = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						r1, g1, b1, dummy1 : byte;
						r2, g2, b2, dummy2 : byte;
						n0, v0 : word;
						v1, v2 : word;
		end;


		TMD_P_G3G = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						r1, g1, b1, dummy1 : byte;
						r2, g2, b2, dummy2 : byte;
						n0, v0 : word;
						n1, v1 : word;
						n2, v2 : word;
		end;

		TMD_P_NF3 = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						v0, v1 : word;
						v2, p : word;
		end;


		TMD_P_NG3 = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						r1, g1, b1, p1 : byte;
						r2, g2, b2, p2 : byte;
						v0, v1 : word;
						v2, p : word;
		end;

		TMD_P_F4 = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						n0, v0 : word;
						v1, v2 : word;
						v3, p : word;
		end;

		TMD_P_G4 = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						n0, v0 : word;
						u1, v1 : word;
						n2, v2 : word;
						n3, v3 : word;
		end;

typedef struct {
	u_char	out, in, dummy, cd;
	u_char	r0, g0, b0, code;
	u_char	r1, g1, b1, dummy1;
	u_char	r2, g2, b2, dummy2;
	u_char	r3, g3, b3, dummy3;
	u_short n0, v0;
	u_short v1, v2;
	u_short v3, dummy4;
}       TMD_P_F4G;

typedef struct {
	u_char	out, in, dummy, cd;
	u_char	r0, g0, b0, code;
	u_char	r1, g1, b1, dummy1;
	u_char	r2, g2, b2, dummy2;
	u_char	r3, g3, b3, dummy3;
	u_short n0, v0;
	u_short n1, v1;
	u_short n2, v2;
	u_short n3, v3;
}       TMD_P_G4G;

typedef struct {
	u_char  out, in, dummy, cd;
	u_char  r0, g0, b0, code;
	u_short v0, v1;
	u_short v2, v3;
}       TMD_P_NF4;

typedef struct {
	u_char  out, in, dummy, cd;
	u_char  r0, g0, b0, code;
	u_char  r1, g1, b1, p1;
	u_char  r2, g2, b2, p2;
	u_char  r3, g3, b3, p3;
	u_short v0, v1;
	u_short v2, v3;
}       TMD_P_NG4;

typedef struct {
	u_char  out, in, dummy, cd;
	u_char  tu0, tv0;
	u_short clut;
	u_char  tu1, tv1;
	u_short tpage;
	u_char  tu2, tv2;
	u_short p;
	u_short n0, v0;
	u_short v1, v2;
}       TMD_P_TF3;

typedef struct {
	u_char  out, in, dummy, cd;
	u_char  tu0, tv0;
	u_short clut;
	u_char  tu1, tv1;
	u_short tpage;
	u_char  tu2, tv2;
	u_short p;
	u_short n0, v0;
	u_short n1, v1;
	u_short n2, v2;
}       TMD_P_TG3;

typedef struct {
	u_char  out, in, dummy, cd;
	u_char  tu0, tv0;
	u_short clut;
	u_char  tu1, tv1;
	u_short tpage;
	u_char  tu2, tv2;
	u_short p0;
	u_char  r0, g0, b0, p1;
	u_short v0, v1;
	u_short v2, p2;
}       TMD_P_TNF3;

typedef struct {
	u_char  out, in, dummy, cd;
	u_char  tu0, tv0;
	u_short clut;
	u_char  tu1, tv1;
	u_short tpage;
	u_char  tu2, tv2;
	u_short p0;
	u_char  r0, g0, b0, p1;
	u_char  r1, g1, b1, p2;
	u_char  r2, g2, b2, p3;
	u_short v0, v1;
	u_short v2, p4;
}       TMD_P_TNG3;

typedef struct {
	u_char  out, in, dummy, cd;
	u_char  tu0, tv0;
	u_short clut;
	u_char  tu1, tv1;
	u_short tpage;
	u_char  tu2, tv2;
	u_short p0;
	u_char  tu3, tv3;
	u_short p1;
	u_short n0, v0;
	u_short v1, v2;
	u_short v3, p2;
}       TMD_P_TF4;

typedef struct {
	u_char  out, in, dummy, cd;
	u_char  tu0, tv0;
	u_short clut;
	u_char  tu1, tv1;
	u_short tpage;
	u_char  tu2, tv2;
	u_short p0;
	u_char  tu3, tv3;
	u_short p1;
	u_short n0, v0;
	u_short n1, v1;
	u_short n2, v2;
	u_short n3, v3;
}       TMD_P_TG4;

typedef struct {
	u_char  out, in, dummy, cd;
	u_char  tu0, tv0;
	u_short clut;
	u_char  tu1, tv1;
	u_short tpage;
	u_char  tu2, tv2;
	u_short p0;
	u_char  tu3, tv3;
	u_short p1;
	u_char  r0, g0, b0, p2;
	u_short v0, v1;
	u_short v2, v3;
}       TMD_P_TNF4;

typedef struct {
	u_char  out, in, dummy, cd;
	u_char  tu0, tv0;
	u_short clut;
	u_char  tu1, tv1;
	u_short tpage;
	u_char  tu2, tv2;
	u_short p0;
	u_char  tu3, tv3;
	u_short p1;
	u_char  r0, g0, b0, p2;
	u_char  r1, g1, b1, p3;
	u_char  r2, g2, b2, p4;
	u_char  r3, g3, b3, p5;
	u_short v0, v1;
	u_short v2, v3;
}       TMD_P_TNG4;

struct TMD_STRUCT {
	u_long *vertop;         /* vertex top address of TMD format */
	u_long  vern;           /* the number of vertex of TMD format */
	u_long *nortop;         /* normal top address of TMD format */
	u_long  norn;           /* the number of normal of TMD format */
	u_long *primtop;        /* primitive top address of TMD format */
	u_long  primn;          /* the number of primitives of TMD format */
	u_long  scale;          /* the scale factor of TMD format */
};

/*
 * active sub divide structure 
 *
 */

#define minmax4(x1,x2,x3,x4,x5,x6) x1>x2?(x6=x1,x5=x2):(x5=x1,x6=x2),\
                                   x3>x6?x6=x3:x3<x5?x5=x3:0,\
                                   x4>x6?x6=x4:x4<x5?x5=x4:0

#define minmax3(x1,x2,x3,x4,x5)    x1>x2?(x5=x1,x4=x2):(x4=x1,x5=x2),\
                                   x3>x5?x5=x3:x3<x4?x4=x3:0


typedef struct {
	short   vx, vy, vz;
	u_char  tu, tv;
}       VERT;

typedef struct {
	short   vx, vy, vz;
	u_char  tu, tv;
	CVECTOR col;
}       VERTC;


typedef struct {
	u_long  limit;		/* divide limit */
	long    hwd, vwd;	/* dummy */
	int     shift;		/* OT shift */
	u_long *org;		/* OT org */
	u_long *pk;		/* packet base */
	long    otz;		/* gte otz */
	long    adivz;		/* active divide codition z */
	short   adivw, adivh;	/* active divide condition w,h */
	long    flg0;		/* gte flag */
	long    flg;		/* gte flag */
	short   minx, miny, maxx, maxy;	/* polygon min-max */
	short   hwd0, vwd0;	/* resolution of screen */
	u_long *tag;		/* work temprly for addPrim */
	POLY_FT4 si;		/* work packet */
}       GsADIV_FT4;

typedef struct {
	VERT    vt[4];
}       GsADIV_P_FT4;



typedef struct {
	u_long  limit;		/* divide limit */
	long    hwd, vwd;	/* dummy */
	int     shift;		/* OT shift */
	u_long *org;		/* OT org */
	u_long *pk;		/* packet base */
	long    otz;		/* gte otz */
	long    adivz;		/* active divide codition z */
	short   adivw, adivh;	/* active divide condition w,h */
	long    flg0;		/* gte flag */
	long    flg;		/* gte flag */
	short   minx, miny, maxx, maxy;	/* polygon min-max */
	short   hwd0, vwd0;	/* resolution of screen */
	u_long *tag;		/* work temprly for addPrim */
	POLY_GT4 si;		/* work packet */
}       GsADIV_GT4;

typedef struct {
	VERTC   vt[4];
}       GsADIV_P_GT4;


typedef struct {
	u_long  limit;		/* divide limit */
	long    hwd, vwd;	/* dummy */
	int     shift;		/* OT shift */
	u_long *org;		/* OT org */
	u_long *pk;		/* packet base */
	long    otz;		/* gte otz */
	long    adivz;		/* active divide codition z */
	short   adivw, adivh;	/* active divide condition w,h */
	long    flg0;		/* gte flag */
	long    flg;		/* gte flag */
	short   minx, miny, maxx, maxy;	/* polygon min-max */
	short   hwd0, vwd0;	/* resolution of screen */
	u_long *tag;		/* work temprly for addPrim */
	POLY_G4 si;		/* work packet */
}       GsADIV_G4;

typedef struct {
	VERTC   vt[4];
}       GsADIV_P_G4;

typedef struct {
	u_long  limit;		/* divide limit */
	long    hwd, vwd;	/* dummy */
	int     shift;		/* OT shift */
	u_long *org;		/* OT org */
	u_long *pk;		/* packet base */
	long    otz;		/* gte otz */
	long    adivz;		/* active divide codition z */
	short   adivw, adivh;	/* active divide condition w,h */
	long    flg0;		/* gte flag */
	long    flg;		/* gte flag */
	short   minx, miny, maxx, maxy;	/* polygon min-max */
	short   hwd0, vwd0;	/* resolution of screen */
	u_long *tag;		/* work temprly for addPrim */
	POLY_F4 si;		/* work packet */
}       GsADIV_F4;

typedef struct {
	VERT    vt[4];
}       GsADIV_P_F4;


typedef struct {
	u_long  limit;		/* divide limit */
	long    hwd, vwd;	/* dummy */
	int     shift;		/* OT shift */
	u_long *org;		/* OT org */
	u_long *pk;		/* packet base */
	long    otz;		/* gte otz */
	long    adivz;		/* active divide codition z */
	short   adivw, adivh;	/* active divide condition w,h */
	long    flg;		/* gte flag */
	short   minx, miny, maxx, maxy;	/* polygon min-max */
	short   hwd0, vwd0;	/* resolution of screen */
	u_long *tag;		/* work temprly for addPrim */
	POLY_FT3 si;		/* work packet */
}       GsADIV_FT3;

typedef struct {
	VERT    vt[3];
}       GsADIV_P_FT3;

typedef struct {
	u_long  limit;		/* divide limit */
	long    hwd, vwd;	/* dummy */
	int     shift;		/* OT shift */
	u_long *org;		/* OT org */
	u_long *pk;		/* packet base */
	long    otz;		/* gte otz */
	long    adivz;		/* active divide codition z */
	short   adivw, adivh;	/* active divide condition w,h */
	long    flg;		/* gte flag */
	short   minx, miny, maxx, maxy;	/* polygon min-max */
	short   hwd0, vwd0;	/* resolution of screen */
	u_long *tag;		/* work temprly for addPrim */
	POLY_GT3 si;		/* work packet */
}       GsADIV_GT3;

typedef struct {
	VERTC   vt[3];
}       GsADIV_P_GT3;

typedef struct {
	u_long  limit;		/* divide limit */
	long    hwd, vwd;	/* dummy */
	int     shift;		/* OT shift */
	u_long *org;		/* OT org */
	u_long *pk;		/* packet base */
	long    otz;		/* gte otz */
	long    adivz;		/* active divide codition z */
	short   adivw, adivh;	/* active divide condition w,h */
	long    flg;		/* gte flag */
	short   minx, miny, maxx, maxy;	/* polygon min-max */
	short   hwd0, vwd0;	/* resolution of screen */
	u_long *tag;		/* work temprly for addPrim */
	POLY_G3 si;		/* work packet */
}       GsADIV_G3;

typedef struct {
	VERTC   vt[3];
}       GsADIV_P_G3;

typedef struct {
	u_long  limit;		/* divide limit */
	long    hwd, vwd;	/* dummy */
	int     shift;		/* OT shift */
	u_long *org;		/* OT org */
	u_long *pk;		/* packet base */
	long    otz;		/* gte otz */
	long    adivz;		/* active divide codition z */
	short   adivw, adivh;	/* active divide condition w,h */
	long    flg;		/* gte flag */
	short   minx, miny, maxx, maxy;	/* polygon min-max */
	short   hwd0, vwd0;	/* resolution of screen */
	u_long *tag;		/* work temprly for addPrim */
	POLY_F3 si;		/* work packet */
}       GsADIV_F3;

typedef struct {
	VERT    vt[3];
}       GsADIV_P_F3;

/*
 * PROTOTYPE DIFINITIONS 
 */
#if defined(_LANGUAGE_C_PLUS_PLUS)||defined(__cplusplus)||defined(c_plusplus)
extern  "C" {
#endif

	void    GsInitGraph(unsigned short x, unsigned short y, unsigned short intmode,
		              unsigned short dith, unsigned short varmmode);
	void    GsInit3D(void);
	void    GsMapModelingData(unsigned long *p);

	void    GsSetProjection(long h);
	int     GsSetFlatLight(int id, GsF_LIGHT * lt);
	void    GsSetLightMode(int mode);
	void    GsSetFogParam(GsFOGPARAM * fogparm);
	void    GsSetAmbient(long r, long g, long b);
	void    GsDrawOt(GsOT * ot);
	void    GsSetWorkBase(PACKET * outpacketp);

	void    GsSortObject3(GsDOBJ3 * objp, GsOT * ot, int shift);
	void    GsSortObject4(GsDOBJ2 * objp, GsOT * ot, int shift, u_long * scratch);
	void    GsSortObject5(GsDOBJ5 * objp, GsOT * ot, int shift, u_long * scratch);
	void    GsSortObject5J(GsDOBJ5 * objp, GsOT * ot, int shift, u_long * scratch);

	void    GsSortSprite(GsSPRITE * sp, GsOT * ot, unsigned short pri);
	void    GsSortSpriteB(GsSPRITE * sp, GsOT * ot, unsigned short pri,
				unsigned short flip);
	void    GsSortFastSprite(GsSPRITE * sp, GsOT * ot, unsigned short pri);
	void    GsSortFastSpriteB(GsSPRITE * sp, GsOT * ot, unsigned short pri,
				unsigned short flip);
	void    GsSortFlipSprite(GsSPRITE * sp, GsOT * ot, unsigned short pri);
	void    GsSortBg(GsBG * bg, GsOT * ot, unsigned short pri);
	void    GsSortFastBg(GsBG * bg, GsOT * ot, unsigned short pri);
	void    GsInitFixBg16(GsBG * bg, u_long * work);
	void    GsSortFixBg16(GsBG * bg, u_long * work, GsOT * otp, unsigned short pri);
	void    GsInitFixBg32(GsBG * bg, u_long * work);
	void    GsSortFixBg32(GsBG * bg, u_long * work, GsOT * otp, unsigned short pri);
	void    GsSortLine(GsLINE * lp, GsOT * ot, unsigned short pri);
	void    GsSortGLine(GsGLINE * lp, GsOT * ot, unsigned short pri);
	void    GsSortBoxFill(GsBOXF * bp, GsOT * ot, unsigned short pri);
	void    GsSortPoly(void *pp, GsOT * ot, unsigned short pri);

	void    GsClearOt(unsigned short offset, unsigned short point, GsOT * otp);
	GsOT   *GsSortOt(GsOT * ot_src, GsOT * ot_dest);
	GsOT   *GsCutOt(GsOT * ot_src, GsOT * ot_dest);
	void    GsDefDispBuff(unsigned short x0, unsigned short y0, unsigned short x1, unsigned short y1);
	void    GsSortClear(unsigned char, unsigned char, unsigned char, GsOT *);
	void    GsGetTimInfo(unsigned long *im, GsIMAGE * tim);
	void    GsSwapDispBuff(void);
	int     GsGetActiveBuff(void);
	void    GsSetDrawBuffClip(void);
	void    GsSetDrawBuffOffset(void);
	void    GsSetClip(RECT * clip);
	DRAWENV *GsSetClip2(RECT * clip);
	void    GsSetOffset(long x, long y);
	void    GsSetOrign(long x, long y);

	void    GsInitCoordinate2(GsCOORDINATE2 * super, GsCOORDINATE2 * base);
	void    GsMulCoord0(MATRIX * m1, MATRIX * m2, MATRIX * m3);
	void    GsMulCoord2(MATRIX * m1, MATRIX * m2);
	void    GsMulCoord3(MATRIX * m1, MATRIX * m2);
	void    GsGetLw(GsCOORDINATE2 * m, MATRIX * out);
	void    GsGetLs(GsCOORDINATE2 * m, MATRIX * out);
	void    GsGetLws(GsCOORDINATE2 * m, MATRIX * outw, MATRIX * outs);

	u_long  GsLinkObject3(unsigned long pmd_base, GsDOBJ3 * objp);
	void    GsLinkObject4(unsigned long tmd_base, GsDOBJ2 * objp, int n);
	void    GsLinkObject5(unsigned long tmd_base, GsDOBJ5 * objp, int n);

	void    GsSetLightMatrix(MATRIX * mp);
	void    GsSetLightMatrix2(MATRIX * mp);
	int     GsSetRefView2(GsRVIEW2 * pv);
	int     GsSetRefView2L(GsRVIEW2 * pv);
	int     GsSetView2(GsVIEW2 * pv);
	void    GsSetLsMatrix(MATRIX * mp);
	void    GsSetClip2D(RECT * rectp);
	void    GsInitVcount();
	long    GsGetVcount();
	void    GsClearVcount();
	void	GsDefDispBuff2(u_short x0, u_short y0, u_short x1, u_short y1);
	void	GsDrawOtIO(GsOT *ot);
	PACKET *GsGetWorkBase();
	void	GsInitGraph2( u_short x, u_short y, u_short intmode, u_short dith, u_short vrammode);
	void	GsSortObject4J(GsDOBJ2 *objp, GsOT *otp, int shift, u_long *scratch);
	void    GsClearDispArea(unsigned char r, unsigned char g, unsigned char b);

	u_long *GsPresetObject(GsDOBJ5 * objp, u_long * base_addr);
	void    GsScaleScreen(SVECTOR * scale);

	PACKET *GsA4divF3L(TMD_P_F3 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			           int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divF3LFG(TMD_P_F3 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			             int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divF3NL(TMD_P_F3 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divNF3(TMD_P_NF3 * op, VERT * vp, PACKET * pk, int n,
			           int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divF4L(TMD_P_F4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			           int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divF4LFG(TMD_P_F4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			             int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divF4NL(TMD_P_F4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divNF4(TMD_P_NF4 * op, VERT * vp, PACKET * pk, int n,
			           int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divG3L(TMD_P_G3 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			           int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divG3LFG(TMD_P_G3 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			             int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divG3NL(TMD_P_G3 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divNG3(TMD_P_NG3 * op, VERT * vp, PACKET * pk, int n,
			           int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divG4L(TMD_P_G4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			           int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divG4LFG(TMD_P_G4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			             int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divG4NL(TMD_P_G4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divNG4(TMD_P_NG4 * op, VERT * vp, PACKET * pk, int n,
			           int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTF3L(TMD_P_TF3 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTF3LFG(TMD_P_TF3 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTF3NL(TMD_P_TF3 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			             int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTNF3(TMD_P_TNF3 * op, VERT * vp, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTF4L(TMD_P_TF4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTF4LFG(TMD_P_TF4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTF4NL(TMD_P_TF4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			             int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTNF4(TMD_P_TNF4 * op, VERT * vp, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTF4LM(TMD_P_TF4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTF4LFGM(TMD_P_TF4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTF4NLM(TMD_P_TF4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			             int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTNF4M(TMD_P_TNF4 * op, VERT * vp, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTG3L(TMD_P_TG3 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTG3LFG(TMD_P_TG3 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTG3NL(TMD_P_TG3 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			             int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTNG3(TMD_P_TNG3 * op, VERT * vp, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTG4L(TMD_P_TG4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTG4LFG(TMD_P_TG4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTG4NL(TMD_P_TG4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			             int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTNG4(TMD_P_TNG4 * op, VERT * vp, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTG4LM(TMD_P_TG4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTG4LFGM(TMD_P_TG4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTG4NLM(TMD_P_TG4 * op, VERT * vp, VERT * np, PACKET * pk, int n,
			             int shift, GsOT * ot, u_long * scratch);
	PACKET *GsA4divTNG4M(TMD_P_TNG4 * op, VERT * vp, PACKET * pk, int n,
			            int shift, GsOT * ot, u_long * scratch);
	PACKET *GsTMDfastF3GL(TMD_P_F3G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastF3GLFG(TMD_P_F3G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastF3GNL(TMD_P_F3G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastG3GL(TMD_P_G3G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastG3GLFG(TMD_P_G3G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastG3GNL(TMD_P_G3G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsPrstF3GL(TMD_P_F3G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsPrstF3GLFG(TMD_P_F3G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsPrstF3GNL(TMD_P_F3G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsPrstG3GL(TMD_P_G3G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsPrstG3GLFG(TMD_P_G3G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsPrstG3GNL(TMD_P_G3G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastG3M(TMD_P_G3 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastG3MFG(TMD_P_G3 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastTG3M(TMD_P_TG3 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastTG3MFG(TMD_P_TG3 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastF4GL(TMD_P_F4G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastF4GLFG(TMD_P_F4G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastF4GNL(TMD_P_F4G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastG4GL(TMD_P_G4G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastG4GLFG(TMD_P_G4G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastG4GNL(TMD_P_G4G *op, VERT *vp, VERT *np, PACKET *pk,
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastG4M(TMD_P_G4 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastG4MFG(TMD_P_G4 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastTG4M(TMD_P_TG4 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastTG4MFG(TMD_P_TG4 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastF3M(TMD_P_F3 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastF3MFG(TMD_P_F3 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastTF3M(TMD_P_TF3 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastTF3MFG(TMD_P_TF3 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastF4M(TMD_P_F4 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastF4MFG(TMD_P_F4 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastTF4M(TMD_P_TF4 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);
	PACKET *GsTMDfastTF4MFG(TMD_P_TF4 *op, VERT *vp, VERT *np, PACKET *pk, 
						int n, int shift, GsOT *ot, u_long *scratch);

#if defined(_LANGUAGE_C_PLUS_PLUS)||defined(__cplusplus)||defined(c_plusplus)
}
#endif



/* EXTERN */
extern RECT CLIP2;		/* clipping area */
extern short PSDBASEX[2], PSDBASEY[2];	/* double buffer base */
extern short PSDIDX;		/* double buffer index */
extern u_long PSDCNT;		/* frame counter for using matrix cache */
extern _GsPOSITION POSITION;	/* 2d offset */
extern DRAWENV GsDRAWENV;	/* DRAWENV of Gs */
extern DISPENV GsDISPENV;	/* DISPENV of Gs */
extern MATRIX GsWSMATRIX;	/* Current World-Screen Matrix of Gs */
extern MATRIX GsWSMATRIX_ORG;	/* Original World-Screen Matrix of Gs */
extern long HWD0, VWD0;		/* rezolution of Holyzontal and Vertical */
extern MATRIX GsLIGHTWSMATRIX;	/* World-Screen Light Matrix of Gs */
extern MATRIX GsIDMATRIX;	/* Unit Matrix */
extern MATRIX GsIDMATRIX2;	/* Unit Matrix including Aspect retio */
extern PACKET *GsOUT_PACKET_P;	/* Work Base pointer */
extern long GsADIVZ;		/* Active sub divide condition (z) */
extern short GsADIVW, GsADIVH;	/* Active sub divide condition (w,h) */
extern int GsLIGHT_MODE;	/* lighting mode global */
extern u_long GsMATE_C, GsLMODE, GsLIGNR, GsLIOFF, GsZOVER, GsBACKC, GsNDIV;
extern u_long GsTRATE, GsTON, GsDISPON;


#if 0
extern _GsFCALL GsFCALL5;	/* GsSortObject5J Func Table */
/* hook only functions to use */
jt_init5()
{				/* GsSortObject5J Hook Func */
	PACKET *GsPrstF3NL(), *GsPrstF3LFG(), *GsPrstF3L(), *GsPrstNF3();
	PACKET *GsTMDdivF3NL(), *GsTMDdivF3LFG(), *GsTMDdivF3L(), *GsTMDdivNF3();
	PACKET *GsPrstG3NL(), *GsPrstG3LFG(), *GsPrstG3L(), *GsPrstNG3();
	PACKET *GsTMDdivG3NL(), *GsTMDdivG3LFG(), *GsTMDdivG3L(), *GsTMDdivNG3();
	PACKET *GsPrstTF3NL(), *GsPrstTF3LFG(), *GsPrstTF3L(), *GsPrstTNF3();
	PACKET *GsTMDdivTF3NL(), *GsTMDdivTF3LFG(), *GsTMDdivTF3L(),*GsTMDdivTNF3();
	PACKET *GsPrstTG3NL(), *GsPrstTG3LFG(), *GsPrstTG3L(), *GsPrstTNG3();
	PACKET *GsTMDdivTG3NL(), *GsTMDdivTG3LFG(), *GsTMDdivTG3L(),*GsTMDdivTNG3();
	PACKET *GsPrstF4NL(), *GsPrstF4LFG(), *GsPrstF4L(), *GsPrstNF4();
	PACKET *GsTMDdivF4NL(), *GsTMDdivF4LFG(), *GsTMDdivF4L(), *GsTMDdivNF4();
	PACKET *GsPrstG4NL(), *GsPrstG4LFG(), *GsPrstG4L(), *GsPrstNG4();
	PACKET *GsTMDdivG4NL(), *GsTMDdivG4LFG(), *GsTMDdivG4L(), *GsTMDdivNG4();
	PACKET *GsPrstTF4NL(), *GsPrstTF4LFG(), *GsPrstTF4L(), *GsPrstTNF4();
	PACKET *GsTMDdivTF4NL(), *GsTMDdivTF4LFG(), *GsTMDdivTF4L(),*GsTMDdivTNF4();
	PACKET *GsPrstTG4NL(), *GsPrstTG4LFG(), *GsPrstTG4L(), *GsPrstTNG4();
	PACKET *GsTMDdivTG4NL(), *GsTMDdivTG4LFG(), *GsTMDdivTG4L(),*GsTMDdivTNG4();
	PACKET *GsPrstF3GNL(), *GsPrstF3GLFG(), *GsPrstF3GL();
	PACKET *GsPrstG3GNL(), *GsPrstG3GLFG(), *GsPrstG3GL();

	/* flat triangle */
	GsFCALL5.f3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsPrstF3L;
	GsFCALL5.f3[GsDivMODE_NDIV][GsLMODE_FOG] = GsPrstF3LFG;
	GsFCALL5.f3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsPrstF3NL;
	GsFCALL5.f3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivF3L;
	GsFCALL5.f3[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivF3LFG;
	GsFCALL5.f3[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivF3NL;
	GsFCALL5.nf3[GsDivMODE_NDIV] = GsPrstNF3;
	GsFCALL5.nf3[GsDivMODE_DIV] = GsTMDdivNF3;
	/* gour triangle */
	GsFCALL5.g3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsPrstG3L;
	GsFCALL5.g3[GsDivMODE_NDIV][GsLMODE_FOG] = GsPrstG3LFG;
	GsFCALL5.g3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsPrstG3NL;
	GsFCALL5.g3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivG3L;
	GsFCALL5.g3[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivG3LFG;
	GsFCALL5.g3[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivG3NL;
	GsFCALL5.ng3[GsDivMODE_NDIV] = GsPrstNG3;
	GsFCALL5.ng3[GsDivMODE_DIV] = GsTMDdivNG3;
	/* texture flat triangle */
	GsFCALL5.tf3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsPrstTF3L;
	GsFCALL5.tf3[GsDivMODE_NDIV][GsLMODE_FOG] = GsPrstTF3LFG;
	GsFCALL5.tf3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsPrstTF3NL;
	GsFCALL5.tf3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivTF3L;
	GsFCALL5.tf3[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivTF3LFG;
	GsFCALL5.tf3[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivTF3NL;
	GsFCALL5.ntf3[GsDivMODE_NDIV] = GsPrstTNF3;
	GsFCALL5.ntf3[GsDivMODE_DIV] = GsTMDdivTNF3;
	/* texture gour triangle */
	GsFCALL5.tg3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsPrstTG3L;
	GsFCALL5.tg3[GsDivMODE_NDIV][GsLMODE_FOG] = GsPrstTG3LFG;
	GsFCALL5.tg3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsPrstTG3NL;
	GsFCALL5.tg3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivTG3L;
	GsFCALL5.tg3[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivTG3LFG;
	GsFCALL5.tg3[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivTG3NL;
	GsFCALL5.ntg3[GsDivMODE_NDIV] = GsPrstTNG3;
	GsFCALL5.ntg3[GsDivMODE_DIV] = GsTMDdivTNG3;
	/* flat quad */
	GsFCALL5.f4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsPrstF4L;
	GsFCALL5.f4[GsDivMODE_NDIV][GsLMODE_FOG] = GsPrstF4LFG;
	GsFCALL5.f4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsPrstF4NL;
	GsFCALL5.f4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivF4L;
	GsFCALL5.f4[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivF4LFG;
	GsFCALL5.f4[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivF4NL;
	GsFCALL5.nf4[GsDivMODE_NDIV] = GsPrstNF4;
	GsFCALL5.nf4[GsDivMODE_DIV] = GsTMDdivNF4;
	/* gour quad */
	GsFCALL5.g4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsPrstG4L;
	GsFCALL5.g4[GsDivMODE_NDIV][GsLMODE_FOG] = GsPrstG4LFG;
	GsFCALL5.g4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsPrstG4NL;
	GsFCALL5.g4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivG4L;
	GsFCALL5.g4[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivG4LFG;
	GsFCALL5.g4[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivG4NL;
	GsFCALL5.ng4[GsDivMODE_NDIV] = GsPrstNG4;
	GsFCALL5.ng4[GsDivMODE_DIV] = GsTMDdivNG4;
	/* texture flat quad */
	GsFCALL5.tf4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsPrstTF4L;
	GsFCALL5.tf4[GsDivMODE_NDIV][GsLMODE_FOG] = GsPrstTF4LFG;
	GsFCALL5.tf4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsPrstTF4NL;
	GsFCALL5.tf4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivTF4L;
	GsFCALL5.tf4[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivTF4LFG;
	GsFCALL5.tf4[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivTF4NL;
	GsFCALL5.ntf4[GsDivMODE_NDIV] = GsPrstTNF4;
	GsFCALL5.ntf4[GsDivMODE_DIV] = GsTMDdivTNF4;
	/* texture gour quad */
	GsFCALL5.tg4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsPrstTG4L;
	GsFCALL5.tg4[GsDivMODE_NDIV][GsLMODE_FOG] = GsPrstTG4LFG;
	GsFCALL5.tg4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsPrstTG4NL;
	GsFCALL5.tg4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivTG4L;
	GsFCALL5.tg4[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivTG4LFG;
	GsFCALL5.tg4[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivTG4NL;
	GsFCALL5.ntg4[GsDivMODE_NDIV] = GsPrstTNG4;
	GsFCALL5.ntg4[GsDivMODE_DIV] = GsTMDdivTNG4;
	/* gradation triangle */
	GsFCALL5.f3g[GsLMODE_NORMAL] = GsPrstF3GL;
	GsFCALL5.f3g[GsLMODE_FOG] = GsPrstF3GLFG;
	GsFCALL5.f3g[GsLMODE_LOFF] = GsPrstF3GNL;
	GsFCALL5.g3g[GsLMODE_NORMAL] = GsPrstG3GL;
	GsFCALL5.g3g[GsLMODE_FOG] = GsPrstG3GLFG;
	GsFCALL5.g3g[GsLMODE_LOFF] = GsPrstG3GNL;
}
#endif

#if 0
extern _GsFCALL GsFCALL4;	/* GsSortObject4J Func Table */
/* hook only functions to use*/
jt_init4()
{				/* GsSortObject4J Hook Func */
	PACKET *GsTMDfastF3NL(), *GsTMDfastF3LFG(), *GsTMDfastF3L(),*GsTMDfastNF3();
	PACKET *GsTMDdivF3NL(), *GsTMDdivF3LFG(), *GsTMDdivF3L(), *GsTMDdivNF3();
	PACKET *GsTMDfastG3NL(), *GsTMDfastG3LFG(), *GsTMDfastG3L(),*GsTMDfastNG3();
	PACKET *GsTMDdivG3NL(), *GsTMDdivG3LFG(), *GsTMDdivG3L(), *GsTMDdivNG3();
	PACKET *GsTMDfastTF3NL(), *GsTMDfastTF3LFG(), *GsTMDfastTF3L(), *GsTMDfastTNF3();
	PACKET *GsTMDdivTF3NL(), *GsTMDdivTF3LFG(), *GsTMDdivTF3L(), *GsTMDdivTNF3();
	PACKET *GsTMDfastTG3NL(), *GsTMDfastTG3LFG(), *GsTMDfastTG3L(), *GsTMDfastTNG3();
	PACKET *GsTMDdivTG3NL(), *GsTMDdivTG3LFG(), *GsTMDdivTG3L(), *GsTMDdivTNG3();
	PACKET *GsTMDfastF4NL(), *GsTMDfastF4LFG(), *GsTMDfastF4L(), *GsTMDfastNF4();
	PACKET *GsTMDdivF4NL(), *GsTMDdivF4LFG(), *GsTMDdivF4L(), *GsTMDdivNF4();
	PACKET *GsTMDfastG4NL(), *GsTMDfastG4LFG(), *GsTMDfastG4L(), *GsTMDfastNG4();
	PACKET *GsTMDdivG4NL(), *GsTMDdivG4LFG(), *GsTMDdivG4L(), *GsTMDdivNG4();
	PACKET *GsTMDfastTF4NL(), *GsTMDfastTF4LFG(), *GsTMDfastTF4L(), *GsTMDfastTNF4();
	PACKET *GsTMDdivTF4NL(), *GsTMDdivTF4LFG(), *GsTMDdivTF4L(), *GsTMDdivTNF4();
	PACKET *GsTMDfastTG4NL(), *GsTMDfastTG4LFG(), *GsTMDfastTG4L(), *GsTMDfastTNG4();
	PACKET *GsTMDdivTG4NL(), *GsTMDdivTG4LFG(), *GsTMDdivTG4L(), *GsTMDdivTNG4();

	/* flat triangle */
	GsFCALL4.f3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastF3L;
	GsFCALL4.f3[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastF3LFG;
	GsFCALL4.f3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastF3NL;
	GsFCALL4.f3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivF3L;
	GsFCALL4.f3[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivF3LFG;
	GsFCALL4.f3[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivF3NL;
	GsFCALL4.nf3[GsDivMODE_NDIV] = GsTMDfastNF3;
	GsFCALL4.nf3[GsDivMODE_DIV] = GsTMDdivNF3;
	/* gour triangle */
	GsFCALL4.g3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastG3L;
	GsFCALL4.g3[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastG3LFG;
	GsFCALL4.g3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastG3NL;
	GsFCALL4.g3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivG3L;
	GsFCALL4.g3[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivG3LFG;
	GsFCALL4.g3[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivG3NL;
	GsFCALL4.ng3[GsDivMODE_NDIV] = GsTMDfastNG3;
	GsFCALL4.ng3[GsDivMODE_DIV] = GsTMDdivNG3;
	/* texture flat triangle */
	GsFCALL4.tf3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastTF3L;
	GsFCALL4.tf3[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastTF3LFG;
	GsFCALL4.tf3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastTF3NL;
	GsFCALL4.tf3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivTF3L;
	GsFCALL4.tf3[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivTF3LFG;
	GsFCALL4.tf3[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivTF3NL;
	GsFCALL4.ntf3[GsDivMODE_NDIV] = GsTMDfastTNF3;
	GsFCALL4.ntf3[GsDivMODE_DIV] = GsTMDdivTNF3;
	/* texture gour triangle */
	GsFCALL4.tg3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastTG3L;
	GsFCALL4.tg3[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastTG3LFG;
	GsFCALL4.tg3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastTG3NL;
	GsFCALL4.tg3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivTG3L;
	GsFCALL4.tg3[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivTG3LFG;
	GsFCALL4.tg3[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivTG3NL;
	GsFCALL4.ntg3[GsDivMODE_NDIV] = GsTMDfastTNG3;
	GsFCALL4.ntg3[GsDivMODE_DIV] = GsTMDdivTNG3;
	/* flat quad */
	GsFCALL4.f4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastF4L;
	GsFCALL4.f4[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastF4LFG;
	GsFCALL4.f4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastF4NL;
	GsFCALL4.f4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivF4L;
	GsFCALL4.f4[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivF4LFG;
	GsFCALL4.f4[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivF4NL;
	GsFCALL4.nf4[GsDivMODE_NDIV] = GsTMDfastNF4;
	GsFCALL4.nf4[GsDivMODE_DIV] = GsTMDdivNF4;
	/* gour quad */
	GsFCALL4.g4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastG4L;
	GsFCALL4.g4[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastG4LFG;
	GsFCALL4.g4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastG4NL;
	GsFCALL4.g4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivG4L;
	GsFCALL4.g4[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivG4LFG;
	GsFCALL4.g4[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivG4NL;
	GsFCALL4.ng4[GsDivMODE_NDIV] = GsTMDfastNG4;
	GsFCALL4.ng4[GsDivMODE_DIV] = GsTMDdivNG4;
	/* texture flat quad */
	GsFCALL4.tf4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastTF4L;
	GsFCALL4.tf4[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastTF4LFG;
	GsFCALL4.tf4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastTF4NL;
	GsFCALL4.tf4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivTF4L;
	GsFCALL4.tf4[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivTF4LFG;
	GsFCALL4.tf4[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivTF4NL;
	GsFCALL4.ntf4[GsDivMODE_NDIV] = GsTMDfastTNF4;
	GsFCALL4.ntf4[GsDivMODE_DIV] = GsTMDdivTNF4;
	/* texture gour quad */
	GsFCALL4.tg4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastTG4L;
	GsFCALL4.tg4[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastTG4LFG;
	GsFCALL4.tg4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastTG4NL;
	GsFCALL4.tg4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivTG4L;
	GsFCALL4.tg4[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivTG4LFG;
	GsFCALL4.tg4[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivTG4NL;
	GsFCALL4.ntg4[GsDivMODE_NDIV] = GsTMDfastTNG4;
	GsFCALL4.ntg4[GsDivMODE_DIV] = GsTMDdivTNG4;
	/* gradation  triangle */
	GsFCALL4.f3g[GsLMODE_NORMAL] = GsTMDfastF3GL;
	GsFCALL4.f3g[GsLMODE_FOG] = GsTMDfastF3GLFG;
	GsFCALL4.f3g[GsLMODE_LOFF] = GsTMDfastF3GNL;
	GsFCALL4.g3g[GsLMODE_NORMAL] = GsTMDfastG3GL;
	GsFCALL4.g3g[GsLMODE_FOG] = GsTMDfastG3GLFG;
	GsFCALL4.g3g[GsLMODE_LOFF] = GsTMDfastG3GNL;
	/* gradation  quad */
	GsFCALL4.f4g[GsLMODE_NORMAL] = GsTMDfastF4GL;
	GsFCALL4.f4g[GsLMODE_FOG] = GsTMDfastF4GLFG;
	GsFCALL4.f4g[GsLMODE_LOFF] = GsTMDfastF4GNL;
	GsFCALL4.g4g[GsLMODE_NORMAL] = GsTMDfastG4GL;
	GsFCALL4.g4g[GsLMODE_FOG] = GsTMDfastG4GLFG;
	GsFCALL4.g4g[GsLMODE_LOFF] = GsTMDfastG4GNL;
}
#endif

#if 0
extern _GsFCALL GsFCALL4;	/* GsSortObject4J Func Table */
jt_init4()
{				/* Gs SortObject4J Active sub divide Func */
	PACKET *GsTMDfastF3NL(), *GsTMDfastF3LFG(), *GsTMDfastF3L(), *GsTMDfastNF3();
	PACKET *GsA4divF3NL(), *GsA4divF3LFG(), *GsA4divF3L(), *GsA4divNF3();
	PACKET *GsTMDfastG3NL(), *GsTMDfastG3LFG(), *GsTMDfastG3L(), *GsTMDfastNG3();
	PACKET *GsA4divG3NL(), *GsA4divG3LFG(), *GsA4divG3L(), *GsA4divNG3();
	PACKET *GsTMDfastTF3NL(), *GsTMDfastTF3LFG(), *GsTMDfastTF3L(), *GsTMDfastTNF3();
	PACKET *GsA4divTF3NL(), *GsA4divTF3LFG(), *GsA4divTF3L(), *GsA4divTNF3();
	PACKET *GsTMDfastTG3NL(), *GsTMDfastTG3LFG(), *GsTMDfastTG3L(), *GsTMDfastTNG3();
	PACKET *GsA4divTG3NL(), *GsA4divTG3LFG(), *GsA4divTG3L(), *GsA4divTNG3();
	PACKET *GsTMDfastF4NL(), *GsTMDfastF4LFG(), *GsTMDfastF4L(), *GsTMDfastNF4();
	PACKET *GsA4divF4NL(), *GsA4divF4LFG(), *GsA4divF4L(), *GsA4divNF4();
	PACKET *GsTMDfastG4NL(), *GsTMDfastG4LFG(), *GsTMDfastG4L(), *GsTMDfastNG4();
	PACKET *GsA4divG4NL(), *GsA4divG4LFG(), *GsA4divG4L(), *GsA4divNG4();
	PACKET *GsTMDfastTF4NL(), *GsTMDfastTF4LFG(), *GsTMDfastTF4L(), *GsTMDfastTNF4();
	PACKET *GsA4divTF4NL(), *GsA4divTF4LFG(), *GsA4divTF4L(), *GsA4divTNF4();
	PACKET *GsTMDfastTG4NL(), *GsTMDfastTG4LFG(), *GsTMDfastTG4L(), *GsTMDfastTNG4();
	PACKET *GsA4divTG4NL(), *GsA4divTG4LFG(), *GsA4divTG4L(), *GsA4divTNG4();
	PACKET *GsA4divTF4L();

	/* flat triangle */
	GsFCALL4.f3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastF3L;
	GsFCALL4.f3[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastF3LFG;
	GsFCALL4.f3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastF3NL;
	GsFCALL4.f3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsA4divF3L;
	GsFCALL4.f3[GsDivMODE_DIV][GsLMODE_FOG] = GsA4divF3LFG;
	GsFCALL4.f3[GsDivMODE_DIV][GsLMODE_LOFF] = GsA4divF3NL;
	GsFCALL4.nf3[GsDivMODE_NDIV] = GsTMDfastNF3;
	GsFCALL4.nf3[GsDivMODE_DIV] = GsA4divNF3;
	/* gour triangle */
	GsFCALL4.g3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastG3L;
	GsFCALL4.g3[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastG3LFG;
	GsFCALL4.g3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastG3NL;
	GsFCALL4.g3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsA4divG3L;
	GsFCALL4.g3[GsDivMODE_DIV][GsLMODE_FOG] = GsA4divG3LFG;
	GsFCALL4.g3[GsDivMODE_DIV][GsLMODE_LOFF] = GsA4divG3NL;
	GsFCALL4.ng3[GsDivMODE_NDIV] = GsTMDfastNG3;
	GsFCALL4.ng3[GsDivMODE_DIV] = GsA4divNG3;
	/* texture flat triangle */
	GsFCALL4.tf3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastTF3L;
	GsFCALL4.tf3[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastTF3LFG;
	GsFCALL4.tf3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastTF3NL;
	GsFCALL4.tf3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsA4divTF3L;
	GsFCALL4.tf3[GsDivMODE_DIV][GsLMODE_FOG] = GsA4divTF3LFG;
	GsFCALL4.tf3[GsDivMODE_DIV][GsLMODE_LOFF] = GsA4divTF3NL;
	GsFCALL4.ntf3[GsDivMODE_NDIV] = GsTMDfastTNF3;
	GsFCALL4.ntf3[GsDivMODE_DIV] = GsA4divTNF3;
	/* texture gour triangle */
	GsFCALL4.tg3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastTG3L;
	GsFCALL4.tg3[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastTG3LFG;
	GsFCALL4.tg3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastTG3NL;
	GsFCALL4.tg3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsA4divTG3L;
	GsFCALL4.tg3[GsDivMODE_DIV][GsLMODE_FOG] = GsA4divTG3LFG;
	GsFCALL4.tg3[GsDivMODE_DIV][GsLMODE_LOFF] = GsA4divTG3NL;
	GsFCALL4.ntg3[GsDivMODE_NDIV] = GsTMDfastTNG3;
	GsFCALL4.ntg3[GsDivMODE_DIV] = GsA4divTNG3;
	/* flat quad */
	GsFCALL4.f4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastF4L;
	GsFCALL4.f4[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastF4LFG;
	GsFCALL4.f4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastF4NL;
	GsFCALL4.f4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsA4divF4L;
	GsFCALL4.f4[GsDivMODE_DIV][GsLMODE_FOG] = GsA4divF4LFG;
	GsFCALL4.f4[GsDivMODE_DIV][GsLMODE_LOFF] = GsA4divF4NL;
	GsFCALL4.nf4[GsDivMODE_NDIV] = GsTMDfastNF4;
	GsFCALL4.nf4[GsDivMODE_DIV] = GsA4divNF4;
	/* gour quad */
	GsFCALL4.g4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastG4L;
	GsFCALL4.g4[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastG4LFG;
	GsFCALL4.g4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastG4NL;
	GsFCALL4.g4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsA4divG4L;
	GsFCALL4.g4[GsDivMODE_DIV][GsLMODE_FOG] = GsA4divG4LFG;
	GsFCALL4.g4[GsDivMODE_DIV][GsLMODE_LOFF] = GsA4divG4NL;
	GsFCALL4.ng4[GsDivMODE_NDIV] = GsTMDfastNG4;
	GsFCALL4.ng4[GsDivMODE_DIV] = GsA4divNG4;
	/* texture flat quad */
	GsFCALL4.tf4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastTF4L;
	GsFCALL4.tf4[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastTF4LFG;
	GsFCALL4.tf4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastTF4NL;
	GsFCALL4.tf4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsA4divTF4L;
	GsFCALL4.tf4[GsDivMODE_DIV][GsLMODE_FOG] = GsA4divTF4LFG;
	GsFCALL4.tf4[GsDivMODE_DIV][GsLMODE_LOFF] = GsA4divTF4NL;
	GsFCALL4.ntf4[GsDivMODE_NDIV] = GsTMDfastTNF4;
	GsFCALL4.ntf4[GsDivMODE_DIV] = GsA4divTNF4;
	/* texture gour quad */
	GsFCALL4.tg4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastTG4L;
	GsFCALL4.tg4[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastTG4LFG;
	GsFCALL4.tg4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastTG4NL;
	GsFCALL4.tg4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsA4divTG4L;
	GsFCALL4.tg4[GsDivMODE_DIV][GsLMODE_FOG] = GsA4divTG4LFG;
	GsFCALL4.tg4[GsDivMODE_DIV][GsLMODE_LOFF] = GsA4divTG4NL;
	GsFCALL4.ntg4[GsDivMODE_NDIV] = GsTMDfastTNG4;
	GsFCALL4.ntg4[GsDivMODE_DIV] = GsA4divTNG4;
	/* gradation triangle */
	GsFCALL4.f3g[GsLMODE_NORMAL] = GsTMDfastF3GL;
	GsFCALL4.f3g[GsLMODE_FOG] = GsTMDfastF3GLFG;
	GsFCALL4.f3g[GsLMODE_LOFF] = GsTMDfastF3GNL;
	GsFCALL4.g3g[GsLMODE_NORMAL] = GsTMDfastG3GL;
	GsFCALL4.g3g[GsLMODE_FOG] = GsTMDfastG3GLFG;
	GsFCALL4.g3g[GsLMODE_LOFF] = GsTMDfastG3GNL;
	/* gradation  quad */
	GsFCALL4.f4g[GsLMODE_NORMAL] = GsTMDfastF4GL;
	GsFCALL4.f4g[GsLMODE_FOG] = GsTMDfastF4GLFG;
	GsFCALL4.f4g[GsLMODE_LOFF] = GsTMDfastF4GNL;
	GsFCALL4.g4g[GsLMODE_NORMAL] = GsTMDfastG4GL;
	GsFCALL4.g4g[GsLMODE_FOG] = GsTMDfastG4GLFG;
	GsFCALL4.g4g[GsLMODE_LOFF] = GsTMDfastG4GNL;
}
#endif

#if 0
extern _GsFCALL GsFCALL4;	/* GsSortObject4J Func Table */
/* hook only functions to use */
jt_init4()
{				/* GsSortObject4J Hook Func (for material attenuation)*/
	PACKET *GsTMDfastF3NL(), *GsTMDfastF3MFG(), *GsTMDfastF3M(),*GsTMDfastNF3();
	PACKET *GsTMDdivF3NL(), *GsTMDdivF3LFG(), *GsTMDdivF3L(), *GsTMDdivNF3();
	PACKET *GsTMDfastG3NL(), *GsTMDfastG3MFG(), *GsTMDfastG3M(),*GsTMDfastNG3();
	PACKET *GsTMDdivG3NL(), *GsTMDdivG3LFG(), *GsTMDdivG3L(), *GsTMDdivNG3();
	PACKET *GsTMDfastTF3NL(), *GsTMDfastTF3MFG(), *GsTMDfastTF3M(), *GsTMDfastTNF3();
	PACKET *GsTMDdivTF3NL(), *GsTMDdivTF3LFG(), *GsTMDdivTF3L(), *GsTMDdivTNF3();
	PACKET *GsTMDfastTG3NL(), *GsTMDfastTG3MFG(), *GsTMDfastTG3M(), *GsTMDfastTNG3();
	PACKET *GsTMDdivTG3NL(), *GsTMDdivTG3LFG(), *GsTMDdivTG3L(), *GsTMDdivTNG3();
	PACKET *GsTMDfastF4NL(), *GsTMDfastF4MFG(), *GsTMDfastF4M(), *GsTMDfastNF4();
	PACKET *GsTMDdivF4NL(), *GsTMDdivF4LFG(), *GsTMDdivF4L(), *GsTMDdivNF4();
	PACKET *GsTMDfastG4NL(), *GsTMDfastG4MFG(), *GsTMDfastG4M(), *GsTMDfastNG4();
	PACKET *GsTMDdivG4NL(), *GsTMDdivG4LFG(), *GsTMDdivG4L(), *GsTMDdivNG4();
	PACKET *GsTMDfastTF4NL(), *GsTMDfastTF4MFG(), *GsTMDfastTF4M(), *GsTMDfastTNF4();
	PACKET *GsTMDdivTF4NL(), *GsTMDdivTF4LFG(), *GsTMDdivTF4L(), *GsTMDdivTNF4();
	PACKET *GsTMDfastTG4NL(), *GsTMDfastTG4MFG(), *GsTMDfastTG4M(), *GsTMDfastTNG4();
	PACKET *GsTMDdivTG4NL(), *GsTMDdivTG4LFG(), *GsTMDdivTG4L(), *GsTMDdivTNG4();

	/* flat triangle */
	GsFCALL4.f3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastF3M;
	GsFCALL4.f3[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastF3MFG;
	GsFCALL4.f3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastF3NL;
	GsFCALL4.f3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivF3L;
	GsFCALL4.f3[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivF3LFG;
	GsFCALL4.f3[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivF3NL;
	GsFCALL4.nf3[GsDivMODE_NDIV] = GsTMDfastNF3;
	GsFCALL4.nf3[GsDivMODE_DIV] = GsTMDdivNF3;
	/* gour triangle */
	GsFCALL4.g3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastG3M;
	GsFCALL4.g3[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastG3MFG;
	GsFCALL4.g3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastG3NL;
	GsFCALL4.g3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivG3L;
	GsFCALL4.g3[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivG3LFG;
	GsFCALL4.g3[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivG3NL;
	GsFCALL4.ng3[GsDivMODE_NDIV] = GsTMDfastNG3;
	GsFCALL4.ng3[GsDivMODE_DIV] = GsTMDdivNG3;
	/* texture flat triangle */
	GsFCALL4.tf3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastTF3M;
	GsFCALL4.tf3[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastTF3MFG;
	GsFCALL4.tf3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastTF3NL;
	GsFCALL4.tf3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivTF3L;
	GsFCALL4.tf3[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivTF3LFG;
	GsFCALL4.tf3[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivTF3NL;
	GsFCALL4.ntf3[GsDivMODE_NDIV] = GsTMDfastTNF3;
	GsFCALL4.ntf3[GsDivMODE_DIV] = GsTMDdivTNF3;
	/* texture gour triangle */
	GsFCALL4.tg3[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastTG3M;
	GsFCALL4.tg3[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastTG3MFG;
	GsFCALL4.tg3[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastTG3NL;
	GsFCALL4.tg3[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivTG3L;
	GsFCALL4.tg3[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivTG3LFG;
	GsFCALL4.tg3[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivTG3NL;
	GsFCALL4.ntg3[GsDivMODE_NDIV] = GsTMDfastTNG3;
	GsFCALL4.ntg3[GsDivMODE_DIV] = GsTMDdivTNG3;
	/* flat quad */
	GsFCALL4.f4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastF4M;
	GsFCALL4.f4[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastF4MFG;
	GsFCALL4.f4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastF4NL;
	GsFCALL4.f4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivF4L;
	GsFCALL4.f4[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivF4LFG;
	GsFCALL4.f4[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivF4NL;
	GsFCALL4.nf4[GsDivMODE_NDIV] = GsTMDfastNF4;
	GsFCALL4.nf4[GsDivMODE_DIV] = GsTMDdivNF4;
	/* gour quad */
	GsFCALL4.g4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastG4M;
	GsFCALL4.g4[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastG4MFG;
	GsFCALL4.g4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastG4NL;
	GsFCALL4.g4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivG4L;
	GsFCALL4.g4[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivG4LFG;
	GsFCALL4.g4[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivG4NL;
	GsFCALL4.ng4[GsDivMODE_NDIV] = GsTMDfastNG4;
	GsFCALL4.ng4[GsDivMODE_DIV] = GsTMDdivNG4;
	/* texture flat quad */
	GsFCALL4.tf4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastTF4M;
	GsFCALL4.tf4[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastTF4MFG;
	GsFCALL4.tf4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastTF4NL;
	GsFCALL4.tf4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivTF4L;
	GsFCALL4.tf4[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivTF4LFG;
	GsFCALL4.tf4[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivTF4NL;
	GsFCALL4.ntf4[GsDivMODE_NDIV] = GsTMDfastTNF4;
	GsFCALL4.ntf4[GsDivMODE_DIV] = GsTMDdivTNF4;
	/* texture gour quad */
	GsFCALL4.tg4[GsDivMODE_NDIV][GsLMODE_NORMAL] = GsTMDfastTG4M;
	GsFCALL4.tg4[GsDivMODE_NDIV][GsLMODE_FOG] = GsTMDfastTG4MFG;
	GsFCALL4.tg4[GsDivMODE_NDIV][GsLMODE_LOFF] = GsTMDfastTG4NL;
	GsFCALL4.tg4[GsDivMODE_DIV][GsLMODE_NORMAL] = GsTMDdivTG4L;
	GsFCALL4.tg4[GsDivMODE_DIV][GsLMODE_FOG] = GsTMDdivTG4LFG;
	GsFCALL4.tg4[GsDivMODE_DIV][GsLMODE_LOFF] = GsTMDdivTG4NL;
	GsFCALL4.ntg4[GsDivMODE_NDIV] = GsTMDfastTNG4;
	GsFCALL4.ntg4[GsDivMODE_DIV] = GsTMDdivTNG4;
	/* gradation  triangle */
	GsFCALL4.f3g[GsLMODE_NORMAL] = GsTMDfastF3GL;
	GsFCALL4.f3g[GsLMODE_FOG] = GsTMDfastF3GLFG;
	GsFCALL4.f3g[GsLMODE_LOFF] = GsTMDfastF3GNL;
	GsFCALL4.g3g[GsLMODE_NORMAL] = GsTMDfastG3GL;
	GsFCALL4.g3g[GsLMODE_FOG] = GsTMDfastG3GLFG;
	GsFCALL4.g3g[GsLMODE_LOFF] = GsTMDfastG3GNL;
	/* gradation  quad */
	GsFCALL4.f4g[GsLMODE_NORMAL] = GsTMDfastF4GL;
	GsFCALL4.f4g[GsLMODE_FOG] = GsTMDfastF4GLFG;
	GsFCALL4.f4g[GsLMODE_LOFF] = GsTMDfastF4GNL;
	GsFCALL4.g4g[GsLMODE_NORMAL] = GsTMDfastG4GL;
	GsFCALL4.g4g[GsLMODE_FOG] = GsTMDfastG4GLFG;
	GsFCALL4.g4g[GsLMODE_LOFF] = GsTMDfastG4GNL;
}
#endif

#endif				/* _LIBGS_H_ */
