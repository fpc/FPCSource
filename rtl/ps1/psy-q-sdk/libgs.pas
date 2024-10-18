// libgs.h: Graphic Library Header 
unit libgs;
interface
uses libgte, libgpu;

type
		PACKET = byte;						// packet peripheral pointer
		PPACKET = ^PACKET;

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
		PGsCOORD2PARAM = ^GsCOORD2PARAM;

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
		PGsRVIEW2 = ^GsRVIEW2;
       
		GsF_LIGHT = packed record
							vx, vy, vz : longint;
							r, g, b : byte;
		end;
		PGsF_LIGHT = ^GsF_LIGHT;
       


		GsOT_TAG = packed record
							p : 0..16777215;
							num : 0..255;
		end;
		PGsOT_TAG = ^GsOT_TAG;


		GsOT = packed record
							len : dword;
							org : PGsOT_TAG;
							offset : dword;
							point : dword;
							tag : PGsOT_TAG;
		end;
		PGsOT = ^GsOT;

		GsDOBJ2 = packed record
							attr : dword;					// pers,trans,rotate,disp
							coord2 : PGsCOORDINATE2;		// local dmatrix
							tmd : pdword;
							id : dword;
		end;
		PGsDOBJ2 = ^GsDOBJ2;

		GsDOBJ3 = packed record
							attr : dword;					// pers,trans,rotate,disp
							coord2 : PGsCOORDINATE2;		// local dmatrix
							pmd : pdword;					// pmd top address
							base : pdword;					// object base address
							sv : pdword;					// shared vertex base
							id : dword;
		end;
		PGsDOBJ3 = ^GsDOBJ3;

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
		PGsDOBJ5 = ^GsDOBJ5;


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
		PGsSPRITE = ^GsSPRITE;


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
		PGsBG = ^GsBG;


		GsLINE = packed record
							attr : dword;
							x0, y0 : smallint;
							x1, y1 : smallint;
							r, g, b : byte;
		end;
		PGsLINE = ^GsLINE;

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
		PGsBOXF = ^GsBOXF;

       	GsFOGPARAM = packed record
							dqa : smallint;
							dqb : longint;
							rfc, gfc, bfc : byte;
       	end;
		PGsFOGPARAM = ^GsFOGPARAM;


       	GsIMAGE = packed record
							pmode : dword;
							px, py : smallint;
							pw, ph : word;
							pixel : pointer;
							cx, cy : smallint;
							cw, ch : word;
							clut : pointer;
		end;
		PGsIMAGE = ^GsIMAGE;


		_GsPOSITION = packed record
							offx, offy : smallint;
		end;


		GsOBJTABLE2 = packed record
							top : PGsDOBJ2;
							nobj : longint;
							maxobj : longint;
		end;


//typedef struct {
//	PACKET
//	* (*f3[2][3]) ();
//	PACKET
//	* (*nf3[2]) ();
//	PACKET
//	* (*g3[2][3]) ();
//	PACKET
//	* (*ng3[2]) ();
//	PACKET
//	* (*tf3[2][3]) ();
//	PACKET
//	* (*ntf3[2]) ();
//	PACKET
//	* (*tg3[2][3]) ();
//	PACKET
//	* (*ntg3[2]) ();
//	PACKET
//	* (*f4[2][3]) ();
//	PACKET
//	* (*nf4[2]) ();
//	PACKET
//	* (*g4[2][3]) ();
//	PACKET
//	* (*ng4[2]) ();
//	PACKET
//	* (*tf4[2][3]) ();
//	PACKET
//	* (*ntf4[2]) ();
//	PACKET
//	* (*tg4[2][3]) ();
//	PACKET
//	* (*ntg4[2]) ();
//	PACKET
//	* (*f3g[3])();
//	PACKET
//	* (*g3g[3])();
//	PACKET
//	* (*f4g[3])();
//	PACKET
//	* (*g4g[3])();
//       _GsFCALL;



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

type
// TMD structure
		TMD_P_F3 = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						n0, v0 : word;
						v1, v2 : word;
		end;
		PTMD_P_F3 = ^TMD_P_F3;

		TMD_P_G3 = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						n0, v0 : word;
						n1, v1 : word;
						n2, v2 : word;
		end;
		PTMD_P_G3 = ^TMD_P_G3;


		TMD_P_F3G = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						r1, g1, b1, dummy1 : byte;
						r2, g2, b2, dummy2 : byte;
						n0, v0 : word;
						v1, v2 : word;
		end;
		PTMD_P_F3G = ^TMD_P_F3G;


		TMD_P_G3G = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						r1, g1, b1, dummy1 : byte;
						r2, g2, b2, dummy2 : byte;
						n0, v0 : word;
						n1, v1 : word;
						n2, v2 : word;
		end;
		PTMD_P_G3G = ^TMD_P_G3G;

		TMD_P_NF3 = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						v0, v1 : word;
						v2, p : word;
		end;
		PTMD_P_NF3 = ^TMD_P_NF3;


		TMD_P_NG3 = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						r1, g1, b1, p1 : byte;
						r2, g2, b2, p2 : byte;
						v0, v1 : word;
						v2, p : word;
		end;
		PTMD_P_NG3 = ^TMD_P_NG3;

		TMD_P_F4 = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						n0, v0 : word;
						v1, v2 : word;
						v3, p : word;
		end;
		PTMD_P_F4 = ^TMD_P_F4;

		TMD_P_G4 = packed record
						out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						n0, v0 : word;
						u1, v1 : word;
						n2, v2 : word;
						n3, v3 : word;
		end;
		PTMD_P_G4 = ^TMD_P_G4;

		TMD_P_F4G = packed record
						_out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						r1, g1, b1, dummy1 : byte;
						r2, g2, b2, dummy2 : byte;
						r3, g3, b3, dummy3 : byte;
						n0, v0 : word;
						v1, v2 : word;
						v3, dummy4 : word;
		end;
		PTMD_P_F4G = ^TMD_P_F4G;

		TMD_P_G4G = packed record
						_out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						r1, g1, b1, dummy1 : byte;
						r2, g2, b2, dummy2 : byte;
						r3, g3, b3, dummy3 : byte;
						n0, v0 : word;
						n1, v1 : word;
						n2, v2 : word;
						n3, v3 : word;
		end;
		PTMD_P_G4G = ^TMD_P_G4G;

		TMD_P_NF4 = packed record
						_out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						v0, v1 : word;
						v2, v3 : word;
		end;
		PTMD_P_NF4 = ^TMD_P_NF4;

		TMD_P_NG4 = packed record
						_out, _in, dummy, cd : byte;
						r0, g0, b0, code : byte;
						r1, g1, b1, p1 : byte;
						r2, g2, b2, p2 : byte;
						r3, g3, b3, p3 : byte;
						v0, v1 : word;
						v2, v3 : word;
		end;
		PTMD_P_NG4 = ^TMD_P_NG4;

		TMD_P_TF3 = packed record
						_out, _in, dummy, cd : byte;
						tu0, tv0 : byte;
						clut : word;
						tu1, tv1 : byte;
						tpage : word;
						tu2, tv2 : byte;
						p : word;
						n0, v0 : word;
						v1, v2 : word;
		end;
		PTMD_P_TF3 = ^TMD_P_TF3;

		TMD_P_TG3 = packed record
						_out, _in, dummy, cd : byte;
						tu0, tv0 : byte;
						clut : word;
						tu1, tv1 : byte;
						tpage : word;
						tu2, tv2 : byte;
						p : word;
						u0, v0 : word;
						n1, v1 : word;
						n2, v2 : word;
		end;
		PTMD_P_TG3 = ^TMD_P_TG3;


		TMD_P_TNF3 = packed record
						_out, _in, dummy, cd : byte;
						tu0, tv0 : byte;
						clut : word;
						tu1, tv1 : byte;
						tpage : word;
						tu2, tv2 : byte;
						p0 : word;
						r0, g0, b0, p1 : byte;
						v0, v1 : word;
						v2, p2 : word;
		end;
		PTMD_P_TNF3 = ^TMD_P_TNF3;


		TMD_P_TNG3 = packed record
						_out, _in, dummy, cd : byte;
						tu0, tv0 : byte;
						clut : word;
						tu1, tv1 : byte;
						tpage : word;
						tu2, tv2 : byte;
						p0 : word;
						r0, g0, b0, p1 : byte;
						r1, g1, b1, p2 : byte;
						r2, g2, b2, p3 : byte;
						v0, v1 : word;
						v2, p4 : word;
		end;
		PTMD_P_TNG3 = ^TMD_P_TNG3;

		TMD_P_TF4 = packed record
						_out, _in, dummy, cd : byte;
						tu0, tv0 : byte;
						clut : word;
						tu1, tv1 : byte;
						tpage : word;
						tu2, tv2 : byte;
						p0 : word;
						tu3, tv3 : byte;
						p1 : word;
						n0, v0 : word;
						v1, v2 : word;
						v3, p2 : word;
		end;
		PTMD_P_TF4 = ^TMD_P_TF4;

		TMD_P_TG4 = packed record
						_out, _in, dummy, cd : byte;
						tu0, tv0 : byte;
						clut : word;
						tu1, tv1 : byte;
						tpage : word;
						tu2, tv2 : byte;
						p0 : word;
						tu3, tv3 : byte;
						p1 : word;
						n0, v0 : word;
						n1, v1 : word;
						n2, v2 : word;
						n3, v3 : word;
		end;
		PTMD_P_TG4 = ^TMD_P_TG4;

		TMD_P_TNF4 = packed record
						_out, _in, dummy, cd : byte;
						tu0, tv0 : byte;
						clut : word;
						tu1, tv1 : byte;
						tpage : word;
						tu2, tv2 : byte;
						p0 : word;
						tu3, tv3 : byte;
						p1 : word;
						r0, g0, b0, p2 : byte;
						v0, v1 : word;
						v2, v3 : word;
		end;
		PTMD_P_TNF4 = ^TMD_P_TNF4;

		TMD_P_TNG4 = packed record
						_out, _in, dummy, cd : byte;
						tu0, tv0 : byte;
						clut : word;
						tu1, tv1 : byte;
						tpage : word;
						tu2, tv2 : byte;
						p0 : word;
						tu3, tv3 : byte;
						p1 : word;
						r0, g0, b0, p2 : byte;
						r1, g1, b1, p3 : byte;
						r2, g2, b2, p4 : byte;
						r3, g3, b3, p5 : byte;
						v0, v1 : word;
						v2, v3 : word;
		end;
		PTMD_P_TNG4 = ^TMD_P_TNG4;

		TMD_STRUCT = packed record
						vertop : pdword;        // vertex top address of TMD format
						vern : dword;           // the number of vertex of TMD format
						nortop : pdword;        // normal top address of TMD format
						norn : dword;           // the number of normal of TMD format
						primtop : pdword;       // primitive top address of TMD format
						primn : dword;          // the number of primitives of TMD format
						scale : dword;          // the scale factor of TMD format
		end;



		VERT = packed record
						vx, vy, vz : smallint;
						tu, tv : byte;
		end;
		PVERT = ^VERT;

		VERTC = packed record
						vx, vy, vz : smallint;
						tu, tv : byte;
						col : CVECTOR;
		end;


		GsADIV_FT4 = packed record
						limit : dword;						// divide limit
						hwd, vwd : longint;					// dummy
						shift : longint;					// OT shift
						org : pdword;						// OT org
						pk : pdword;						// packet base
						otz : longint;						// gte otz
						adivz : longint;					// active divide codition z
						adivw, adivh : smallint;			// active divide condition w,h
						flg0 : longint;						// gte flag
						flg : longint;						// gte flag
						minx, miny, maxx, maxy : smallint;	// polygon min-max
						hwd0, vwd0 : smallint;				// resolution of screen
						tag : pdword;						// work temprly for addPrim
						si : POLY_FT4;						// work packet
		end;


		GsADIV_P_FT4 = packed record
						vt : array [0..3] of VERT;
		end;


		GsADIV_GT4 = packed record
						limit : dword;						// divide limit
						hwd, vwd : longint;					// dummy
						shift : longint;					// OT shift
						org : pdword;						// OT org
						pk : pdword;						// packet base
						otz : longint;						// gte otz
						adivz : longint;					// active divide codition z
						adivw, adivh : smallint;			// active divide condition w,h
						flg0 : longint;						// gte flag
						flg : longint;						// gte flag
						minx, miny, maxx, maxy : smallint;	// polygon min-max
						hwd0, vwd0 : smallint;				// resolution of screen
						tag : pdword;						// work temprly for addPrim
						si : POLY_GT4;						// work packet
		end;

		GsADIV_P_GT4 = packed record
						vt : array [0..3] of VERTC;
		end;


		GsADIV_G4 = packed record
						limit : dword;						// divide limit
						hwd, vwd : longint;					// dummy
						shift : longint;					// OT shift
						org : pdword;						// OT org
						pk : pdword;						// packet base
						otz : longint;						// gte otz
						adivz : longint;					// active divide codition z
						adivw, adivh : smallint;			// active divide condition w,h
						flg0 : longint;						// gte flag
						flg : longint;						// gte flag
						minx, miny, maxx, maxy : smallint;	// polygon min-max
						hwd0, vwd0: smallint;				// resolution of screen
						tag : pdword;						// work temprly for addPrim
						si : POLY_G4;						// work packet
		end;

		GsADIV_P_G4 = packed record
						vt : array [0..3] of VERTC;
		end;

		GsADIV_F4 = packed record
						limit : dword;				// divide limit
						hwd, vwd : longint;					// dummy
						shift : longint;					// OT shift
						org : pdword;						// OT org
						pk : pdword;						// packet base
						otz : longint;						// gte otz
						adivz : longint;					// active divide codition z
						adivw, adivh : smallint;			// active divide condition w,h
						flg0 : longint;						// gte flag
						flg : longint;						// gte flag
						minx, miny, maxx, maxy : smallint;	// polygon min-max
						hwd0, vwd0 : smallint;				// resolution of screen
						tag : pdword;						// work temprly for addPrim
						si : POLY_F4;						// work packet
	end;

	GsADIV_P_F4 = packed record
						vt : array [0..3] of VERT;
	end;


	GsADIV_FT3 = packed record
						limit : dword;						// divide limit
						hwd, vwd : longint;					// dummy
						shift : longint;					// OT shift
						org : pdword;						// OT org
						pk : pdword;						// packet base
						otz : longint;						// gte otz
						adivz : longint;					// active divide codition z
						adivw, adivh : smallint;			// active divide condition w,h
						flg : longint;						// gte flag
						minx, miny, maxx, maxy : smallint;	// polygon min-max
						hwd0, vwd0 : smallint;				// resolution of screen
						tag : pdword;						// work temprly for addPrim
						si : POLY_FT3;						// work packet
	end;

	GsADIV_P_FT3 = packed record
						vt : array [0..2] of VERT;
	end;


	GsADIV_GT3 = packed record
						limit : dword;						// divide limit
						hwd, vwd : longint;					// dummy
						shift : longint;					// OT shift
						org : pdword;						// OT org
						pk : pdword;						// packet base
						otz : longint;						// gte otz
						adivz : longint;					// active divide codition z
						adivw, adivh : smallint;			// active divide condition w,h
						flg : longint;						// gte flag
						minx, miny, maxx, maxy : smallint;	// polygon min-max
						hwd0, vwd0 : smallint;				// resolution of screen
						tag : pdword;						// work temprly for addPrim
						si : POLY_GT3;						// work packet
	end;

	GsADIV_P_GT3 = packed record
						vt : array [0..2] of VERTC;
	end;

	GsADIV_G3 = packed record
						limit : dword;						// divide limit
						hwd, vwd : longint;					// dummy
						shift : longint;					// OT shift
						org : pdword;						// OT org
						pk : pdword;						// packet base
						otz : longint;						// gte otz
						adivz : longint;					// active divide codition z
						adivw, adivh : smallint;			// active divide condition w,h
						flg : longint;						// gte flag
						minx, miny, maxx, maxy : smallint;	// polygon min-max
						hwd0, vwd0 : smallint;				// resolution of screen
						tag : pdword;						// work temprly for addPrim
						si : POLY_G3;						// work packet
	end;

	GsADIV_P_G3 = packed record
						vt : array [0..2] of VERTC;
	end;

	GsADIV_F3 = packed record
						limit : dword;						// divide limit
						hwd, vwd : longint;					// dummy
						shift : longint;					// OT shift
						org : pdword;						// OT org
						pk : pdword;						// packet base
						otz : longint;						// gte otz
						adivz : longint;					// active divide codition z
						adivw, adivh : smallint;			// active divide condition w,h
						flg : longint;						// gte flag
						minx, miny, maxx, maxy : smallint;	// polygon min-max
						hwd0, vwd0 : smallint;				// resolution of screen
						tag : pdword;						// work temprly for addPrim
						si : POLY_F3;						// work packet
	end;

	GsADIV_P_F3 = packed record
						vt : array [0..2] of VERT;
	end;


	procedure GsInitGraph(x, y: word; intmode: word; dith: word; varmmode: word); external;
	procedure GsInit3D; external;
	procedure GsMapModelingData(p: dword); external;

	procedure GsSetProjection(h: longint); external;
	function GsSetFlatLight(id: longint; lt: PGsF_LIGHT): longint; external;
	procedure GsSetLightMode(mode: longint); external;
	procedure GsSetFogParam(fogparm: PGsFOGPARAM); external;
	procedure GsSetAmbient(r, g, b: longint); external;
	procedure GsDrawOt(ot: PGsOT); external;
	procedure GsSetWorkBase(outpacketp: PPACKET); external;

	procedure GsSortObject3(objp: PGsDOBJ3; ot: PGsOT; shift: longint); external;
	procedure GsSortObject4(objp: PGsDOBJ2; ot: PGsOT; shift: longint; scratch: pdword); external;
	procedure GsSortObject5(objp: PGsDOBJ5; ot: PGsOT; shift: longint; scratch: pdword); external;
	procedure GsSortObject5J(objp: PGsDOBJ5; ot: PGsOT; shift: longint; scratch: pdword); external;

	procedure GsSortSprite(sp: PGsSPRITE; ot: PGsOT; pri: word); external;
	procedure GsSortSpriteB(sp: PGsSPRITE; ot: PGsOT; pri: word; flip: word); external;
	procedure GsSortFastSprite(sp: PGsSPRITE; ot: PGsOT; pri: word); external;
	procedure GsSortFastSpriteB(sp: PGsSPRITE; ot: PGsOT; pri: word; flip: word); external;
	procedure GsSortFlipSprite(sp: PGsSPRITE; ot: PGsOT; pri: word); external;
	procedure GsSortBg(bg: PGsBG; ot: PGsOT; pri: word); external;
	procedure GsSortFastBg(bg: PGsBG; ot: PGsOT; pri: word); external;
	procedure GsInitFixBg16(bg: PGsBG; work: pdword); external;
	procedure GsSortFixBg16(bg: PGsBG; work: pdword; otp: PGsOT; pri: word); external;
	procedure GsInitFixBg32(bg: PGsBG; work: pdword); external;
	procedure GsSortFixBg32(bg: PGsBG; work: pdword; otp: PGsOT; pri: word); external;
	procedure GsSortLine(lp: PGsLINE; ot: PGsOT; pri: word); external;
	procedure GsSortGLine(lp: PGsLINE; ot: PGsOT; pri: word); external;
	procedure GsSortBoxFill(bp: PGsBOXF; ot: PGsOT; pri: word); external;
	procedure GsSortPoly(pp: pointer; ot: PGsOT; pri: word); external;

	procedure GsClearOt(offs: dword; point: word; otp: PGsOT); external;
	function GsSortOt(ot_src: PGsOT; ot_dest: PGsOT): PGsOT; external;
	function GsCutOt(ot_src: PGsOT; ot_dest: PGsOT): PGsOT; external;
	procedure GsDefDispBuff(x0, y0, x1, y1: word); external;
	procedure GsSortClear(r, g, b: byte; otp: PGsOT); external;
	procedure GsGetTimInfo(im: pdword; tim: PGsIMAGE); external;
	procedure GsSwapDispBuff; external;
	function GsGetActiveBuff: longint; external;
	procedure GsSetDrawBuffClip; external;
	procedure GsSetDrawBuffOffset; external;
	procedure GsSetClip(clip: PRECT); external;
	function GsSetClip2(clip: PRECT): PDRAWENV; external;
	procedure GsSetOffset(x, y: longint); external;
	procedure GsSetOrign(x, y: longint); external;

	procedure GsInitCoordinate2(super: PGsCOORDINATE2; base: PGsCOORDINATE2); external;
	procedure GsMulCoord0(m1, m2, m3: PMATRIX); external;
	procedure GsMulCoord2(m1, m2: PMATRIX); external;
	procedure GsMulCoord3(m1, m2: PMATRIX); external;
	procedure GsGetLw(m: PGsCOORDINATE2; out: PMATRIX); external;
	procedure GsGetLs(m: PGsCOORDINATE2; out: PMATRIX); external;
	procedure GsGetLws(m: PGsCOORDINATE2; outw, outs: PMATRIX); external;

	function GsLinkObject3(pmd_base: dword; objp: PGsDOBJ3): dword; external;
	procedure GsLinkObject4(tmd_base: dword; objp: PGsDOBJ2; n: longint); external;
	procedure GsLinkObject5(tmd_base: dword; objp: PGsDOBJ5; n: longint); external;

	procedure GsSetLightMatrix(mp: PMATRIX); external;
	procedure GsSetLightMatrix2(mp: PMATRIX); external;
	function GsSetRefView2(pv: PGsRVIEW2): longint; external;
	function GsSetRefView2L(pv: PGsRVIEW2): longint; external;
	function GsSetView2(pv: PGsRVIEW2): longint; external;
	procedure GsSetLsMatrix(mp: PMATRIX); external;
	procedure GsSetClip2D(rectp: PRECT); external;
	procedure GsInitVcount; external;
	procedure GsGetVcount; external;
	procedure GsClearVcount; external;
	procedure GsDefDispBuff2(x0, y0, x1, y1: word); external;
	procedure GsDrawOtIO(ot: PGsOT); external;
	function GsGetWorkBase: PPACKET; external;
	procedure GsInitGraph2(x, y: word; intmode: word; dith: word; vrammode: word); external;
	procedure GsSortObject4J(objp: PGsDOBJ2; otp: PGsOT; shift: longint; scratch: pdword); external;
	procedure GsClearDispArea(r, g, b: byte); external;

	function GsPresetObject(objp: PGsDOBJ5; base_addr: pdword): pdword; external;
	procedure GsScaleScreen(scale: PSVECTOR); external;

	function GsA4divF3L(op: PTMD_P_F3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divF3LFG(op: PTMD_P_F3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divF3NL(op: PTMD_P_F3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divNF3(op: PTMD_P_NF3; vp: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divF4L(op: PTMD_P_F4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divF4LFG(op: PTMD_P_F4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divF4NL(op: PTMD_P_F4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divNF4(op: PTMD_P_NF4; vp: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divG3L(op: PTMD_P_G3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divG3LFG(op: PTMD_P_G3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divG3NL(op: PTMD_P_G3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divNG3(op: PTMD_P_NG3; vp: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divG4L(op: PTMD_P_G4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divG4LFG(op: PTMD_P_G4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divG4NL(op: PTMD_P_G4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divNG4(op: PTMD_P_NG4; vp: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTF3L(op: PTMD_P_TF3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTF3LFG(op: PTMD_P_TF3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTF3NL(op: PTMD_P_TF3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTNF3(op: PTMD_P_TNF3; vp: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTF4L(op: PTMD_P_TF4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTF4LFG(op: PTMD_P_TF4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTF4NL(op: PTMD_P_TF4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTNF4(op: PTMD_P_TNF4; vp: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTF4LM(op: PTMD_P_TF4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTF4LFGM(op: PTMD_P_TF4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTF4NLM(op: PTMD_P_TF4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTNF4M(op: PTMD_P_TNF4; vp: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTG3L(op: PTMD_P_TG3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTG3LFG(op: PTMD_P_TG3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTG3NL(op: PTMD_P_TG3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTNG3(op: PTMD_P_TNG3; vp: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTG4L(op: PTMD_P_TG4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTG4LFG(op: PTMD_P_TG4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTG4NL(op: PTMD_P_TG4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTNG4(op: PTMD_P_TNG4; vp: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTG4LM(op: PTMD_P_TG4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTG4LFGM(op: PTMD_P_TG4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTG4NLM(op: PTMD_P_TG4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsA4divTNG4M(op: PTMD_P_TNG4; vp: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastF3GL(op: PTMD_P_F3G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastF3GLFG(op: PTMD_P_F3G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastF3GNL(op: PTMD_P_F3G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastG3GL(op: PTMD_P_G3G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastG3GLFG(op: PTMD_P_G3G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastG3GNL(op: PTMD_P_G3G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsPrstF3GL(op: PTMD_P_F3G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsPrstF3GLFG(op: PTMD_P_F3G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsPrstF3GNL(op: PTMD_P_F3G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsPrstG3GL(op: PTMD_P_G3G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsPrstG3GLFG(op: PTMD_P_G3G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsPrstG3GNL(op: PTMD_P_G3G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastG3M(op: PTMD_P_G3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastG3MFG(op: PTMD_P_G3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastTG3M(op: PTMD_P_TG3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastTG3MFG(op: PTMD_P_TG3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastF4GL(op: PTMD_P_F4G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastF4GLFG(op: PTMD_P_F4G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastF4GNL(op: PTMD_P_F4G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastG4GL(op: PTMD_P_G4G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastG4GLFG(op: PTMD_P_G4G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastG4GNL(op: PTMD_P_G4G; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastG4M(op: PTMD_P_G4; vp, np: PVERT; pk: PPACKET;  n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastG4MFG(op: PTMD_P_G4; vp, np: PVERT; pk: PPACKET;  n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastTG4M(op: PTMD_P_TG4; vp, np: PVERT; pk: PPACKET;  n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastTG4MFG(op: PTMD_P_TG4; vp, np: PVERT; pk: PPACKET;  n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastF3M(op: PTMD_P_F3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastF3MFG(op: PTMD_P_F3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastTF3M(op: PTMD_P_TF3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastTF3MFG(op: PTMD_P_TF3; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastF4M(op: PTMD_P_F4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastF4MFG(op: PTMD_P_F4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastTF4M(op: PTMD_P_TF4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;
	function GsTMDfastTF4MFG(op: PTMD_P_TF4; vp, np: PVERT; pk: PPACKET; n, shift: longint; ot: PGsOT; scratch: pdword): PPACKET; external;



var
	CLIP2 : RECT; external;									// clipping area
	PSDBASEX : array [0..1] of smallint; external;			// double buffer base
	PSDBASEY : array [0..1] of smallint; external;
	PSDIDX : smallint; external;							// double buffer index
	PSDCNT : dword; external;								// frame counter for using matrix cache
	POSITION : _GsPOSITION; external;						// 2d offset
	GsDRAWENV : DRAWENV; external;							// DRAWENV of Gs
	GsDISPENV : DISPENV; external;							// DISPENV of Gs
	GsWSMATRIX : MATRIX; external; 							// Current World-Screen Matrix of Gs
	GsWSMATRIX_ORG : MATRIX; external; 						// Original World-Screen Matrix of Gs
	HWD0 : longint; external;
	VWD0 : longint; external;							// rezolution of Holyzontal and Vertical
	GsLIGHTWSMATRIX : MATRIX; external;						// World-Screen Light Matrix of Gs
	GsIDMATRIX : MATRIX; external;							// Unit Matrix
	GsIDMATRIX2 : MATRIX; external;							// Unit Matrix including Aspect retio
	GsOUT_PACKET_P : PPACKET; external;						// Work Base pointer
	GsADIVZ : longint; external;							// Active sub divide condition (z)
	GsADIVW : smallint; external;
	GsADIVH : smallint; external; 					// Active sub divide condition (w,h)
	GsLIGHT_MODE : longint; external;						// lighting mode global
	GsMATE_C : dword; external;
	GsLMODE : dword; external;
	GsLIGNR : dword; external;
	GsLIOFF : dword; external;
	GsZOVER : dword; external;
	GsBACKC : dword; external;
	GsNDIV : dword; external;
	GsTRATE : dword; external;
	GsTON : dword; external;
	GsDISPON : dword; external;


procedure GsIncFrame;
procedure GsUpdateCoord;
procedure GsSetAzwh(z: longint; w, h: smallint);

procedure minmax4(var x1,x2,x3,x4,x5,x6: longint);
procedure minmax3(var x1,x2,x3,x4,x5: longint);


implementation


procedure GsIncFrame;
begin

	inc(PSDCNT);

	if PSDCNT > 0 then PSDCNT:= PSDCNT else PSDCNT:= 1;
	if PSDIDX = 0 then PSDIDX:=1 else PSDIDX:= 0;

end;


procedure GsUpdateCoord;
begin

	inc(PSDCNT);
	if PSDCNT > 0 then PSDCNT:= PSDCNT else PSDCNT:= 1;

end;


procedure GsSetAzwh(z: longint; w, h: smallint);
begin

  GsADIVZ:= z;
  GsADIVW:= w;
  GsADIVH:= h;

end;


procedure minmax4(var x1,x2,x3,x4,x5,x6: longint);
begin

	if x1 > x2 then begin x6:= x1; x5:= x2; end else begin x5:= x1; x6:= x2; end;
	if x3 > x6 then begin x6:=x3; end else begin if x3 < x5 then x5:=x3; end;
	if x4 > x6 then begin x6:=x4; end else begin if x4 < x5 then x5:=x4; end;

end;


procedure minmax3(var x1,x2,x3,x4,x5: longint);
begin

	if x1 > x2 then begin x5:= x1; x4:= x2; end else begin x4:= x1; x5:=x2; end;
	if x3 > x5 then begin x5:=x3; end else begin if x3 < x4 then x4:=x3; end;

end;


begin
end.