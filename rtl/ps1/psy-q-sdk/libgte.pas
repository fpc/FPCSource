{$MODE OBJFPC}
unit libgte; 
interface


// Geometry Structures:
const
		ONE	= 4096;	// GTE regards 4096 as 1.0
		TMPVTXNUM = 16;	// Clip Function Header
		OUTVTXNUM = 10;


type
	PMATRIX = ^MATRIX;
	MATRIX = packed record
			m : array [0..2, 0..2] of smallint;		// 3x3 rotation matrix
        		t : array [0..2] of longint;			// transfer vector
       end;
	

	VECTOR = packed record							// long word type 3D vector
			vx, vy : longint;
			vz, pad : longint;
	end;

	PVECTOR = ^VECTOR;
	SVECTOR = packed record							// short word type 3D vector
				vx, vy : smallint;
				vz, pad : smallint;
			  end;
	PSVECTOR = ^SVECTOR;
	       
	CVECTOR = packed record							// color type vector
			  	r, g, b, cd : byte;
			  end;
	PCVECTOR = ^CVECTOR;
	       
	DVECTOR = packed record							// 2D short vector
				vx, vy : smallint;
			  end;
	PDVECTOR = ^DVECTOR;

	EVECTOR = packed record
				v : SVECTOR;						// Object(Local) 3D Vertex
				sxyz : VECTOR;						// Screen 3D Vertex
				sxy : DVECTOR;						// Screen 2D Vertex
				rgb : CVECTOR;						// Vertex Color Data
				txuv, pad : smallint;				// Texture Mapping Data
				chx, chy : longint;					// Clip Window Data
		     end;
	PEVECTOR = ^EVECTOR;
	PPEVECTOR = ^PEVECTOR;

	RVECTOR = packed record							// division vertex data vector
				v : SVECTOR;
				uv : array [0..1] of byte;
	 			pad : word;
				c : CVECTOR;
				sxy : DVECTOR;
				sz : dword;							// clip z-data
			  end;
	PRVECTOR = ^RVECTOR;

	CRVECTOR3 = packed record						// recursive vector for triangles
					r01, r12, r20 : RVECTOR;
					r0, r1, r2 : PRVECTOR;
					rtn : pdword;
				end;

	DIVPOLYGON3 = packed record						// division buffer for triangles
					ndiv : dword;					// number of divisions
					pih, piv : dword;				// clipping area
					clut, tpage : word;
					rgbc : CVECTOR;
					ot : pdword;
					r0, r1, r2 : RVECTOR;
					cr : array [0..4] of CRVECTOR3;
				  end;

	CRVECTOR4 = packed record						// recursive vector for four-sided polygons
					r01, r02, r31, r32, rc : RVECTOR;
					r0, r1, r2, r3 : PRVECTOR;
					rtn : pdword;
				end;

	DIVPOLYGON4 = packed record						// division buffer for four-sided polygons
					ndiv : dword;					// number of divisions
					pih, piv : dword;				// clipping area
					clut, tpage : word;
					rgbc : CVECTOR;
					ot : pdword;
					r0, r1, r2, r3 : RVECTOR;
					cr : array [0..4] of CRVECTOR4;
				 end;

	SPOL = packed record
        		xy : array [0..2] of smallint;
        		uv : array [0..1] of smallint;
        		rgb : array [0..2] of smallint;
           end;
    PSPOL = ^SPOL;

                                        
    POL4 = packed record							// polygon: 41 bytes/1 polygon
        	sxy : array [0..3, 0..1] of smallint;   //0..7
        	sz : array [0..3, 0..1] of smallint;    // 8..15sz[][1] is dummy
        	uv  : array [0..3, 0..1] of smallint;   // 16..23
        	rgb : array [0..3, 0..2] of smallint;   // 23..34
        	code : smallint;                   		// 35...  F4:5, TF4:6, G4:7, TG4:8
           end;
    PPOL4 = ^POL4;

    POL3 = packed record
        	sxy : array [0..2, 0..1] of smallint;
        	sz : array [0..2, 0..1] of smallint;   	// sz[][1] is dummy
        	uv : array [0..2, 0..1] of smallint;
        	rgb : array [0..2, 0..2] of smallint;
        	code : smallint;                   		// F3:1, TF3:2, G3:3, TG3:4
    	  end;
    PPOL3 = ^POL3;

   	TMESH = packed record
	   			v : PSVECTOR;                     	// shared vertices
        		n : PSVECTOR;                     	// shared normals
        		u : PSVECTOR;                     	// shared texture addresses
        		c : PCVECTOR;                     	// shared colors
        		len : dword;                    	// mesh length(=#vertex)
        	end;


    QMESH = packed record
        		v : PSVECTOR;                     	// shared vertices
        		n : PSVECTOR;                     	// shared normals
        		u : PSVECTOR;                     	// shared texture addresses
        		c : PCVECTOR;                     	// shared colors
        		lenv : dword;                   	// mesh length_V(=#vertex_V)
        		lenh : dword;                   	// mesh length_H(=#vertex_H)
    		end;



procedure InitGeom; stdcall; external;

procedure EigenMatrix(m, t: PMATRIX); stdcall; external;
function IsIdMatrix(m: PMATRIX): longint; stdcall; external;
function MulMatrix0(m0, m1, m2: PMATRIX): PMATRIX; stdcall; external;
function MulRotMatrix0(m0, m1: PMATRIX): PMATRIX; stdcall; external;
function MulMatrix(m0, m1: PMATRIX): PMATRIX; stdcall; external;
function MulMatrix2(m0, m1: PMATRIX): PMATRIX; stdcall; external;
function MulRotMatrix(m0: PMATRIX): PMATRIX; stdcall; external;
function SetMulMatrix(m0, m1: PMATRIX): PMATRIX; stdcall; external;
function SetMulRotMatrix(m0: PMATRIX): PMATRIX; stdcall; external;

function ApplyMatrix(m: PMATRIX; v0: PSVECTOR; v1: PVECTOR): PVECTOR; stdcall; external;
function ApplyRotMatrix(v0: PSVECTOR; v1: PVECTOR): PVECTOR; stdcall; external;
function ApplyRotMatrixLV(v0, v1: PVECTOR): PVECTOR; stdcall; external;
function ApplyMatrixLV(m: PMATRIX; v0, v1: PVECTOR): PVECTOR; stdcall; external;

function ApplyMatrixSV(m: PMATRIX; v0, v1: PSVECTOR): PSVECTOR; stdcall; external;

function ApplyTransposeMatrixLV(m: PMATRIX; v0, v1: PVECTOR): PVECTOR; stdcall; external;

function RotMatrix(r: PSVECTOR; m: PMATRIX): PMATRIX; stdcall; external;
function RotMatrixXZY(r: PSVECTOR; m: PMATRIX): PMATRIX; stdcall; external;
function RotMatrixYXZ(r: PSVECTOR; m: PMATRIX): PMATRIX; stdcall; external;
function RotMatrixYZX(r: PSVECTOR; m: PMATRIX): PMATRIX; stdcall; external;
function RotMatrixZXY(r: PSVECTOR; m: PMATRIX): PMATRIX; stdcall; external;
function RotMatrixZYX(r: PSVECTOR; m: PMATRIX): PMATRIX; stdcall; external;
function RotMatrix_gte(r: PSVECTOR; m: PMATRIX): PMATRIX; stdcall; external;
function RotMatrixYXZ_gte(r: PSVECTOR; m: PMATRIX): PMATRIX; stdcall; external;
function RotMatrixZYX_gte(r: PSVECTOR; m: PMATRIX): PMATRIX; stdcall; external;
function RotMatrixX(r: longint; m: PMATRIX): PMATRIX; stdcall; external;
function RotMatrixY(r: longint; m: PMATRIX): PMATRIX; stdcall; external;
function RotMatrixZ(r: longint; m: PMATRIX): PMATRIX; stdcall; external;
function RotMatrixC(r: PSVECTOR; m: PMATRIX): PMATRIX; stdcall; external;
function TransMatrix(m: PMATRIX; v: PVECTOR): PMATRIX; stdcall; external;
function ScaleMatrix(m: PMATRIX; v: PVECTOR): PMATRIX; stdcall; external;
function ScaleMatrixL(m: PMATRIX; v: PVECTOR): PMATRIX; stdcall; external;
function TransposeMatrix(m0, m1: PMATRIX): PMATRIX; stdcall; external;
function CompMatrix(m0, m1, m2: PMATRIX): PMATRIX; stdcall; external;
function CompMatrixLV(m0, m1, m2: PMATRIX): PMATRIX; stdcall; external;

procedure MatrixNormal(m, n: PMATRIX); stdcall; external;
procedure MatrixNormal_0(m, n: PMATRIX); stdcall; external;
procedure MatrixNormal_1(m, n: PMATRIX); stdcall; external;
procedure MatrixNormal_2(m, n: PMATRIX); stdcall; external;

procedure SetRotMatrix(m: PMATRIX); stdcall; external;
procedure SetLightMatrix(m: PMATRIX); stdcall; external;
procedure SetColorMatrix(m: PMATRIX); stdcall; external;
procedure SetTransMatrix(m: PMATRIX); stdcall; external;
procedure PushMatrix; stdcall; external;
procedure PopMatrix; stdcall; external;
procedure ReadRotMatrix(m: PMATRIX); stdcall; external;
procedure ReadLightMatrix(m: PMATRIX); stdcall; external;
procedure ReadColorMatrix(m: PMATRIX); stdcall; external;
procedure SetRGBcd(v: PCVECTOR); stdcall; external;
procedure SetBackColor(rbk, gbk, bbk: longint); stdcall; external;
procedure SetFarColor(rfc, gfc, bfc: longint); stdcall; external;
procedure SetGeomOffset(ofx, ofy: longint); stdcall; external;
procedure SetGeomScreen(h: longint); stdcall; external;
procedure ReadSZfifo3(sz0, sz1, sz2: plongint); stdcall; external;
procedure ReadSZfifo4(szx, sz0, sz1, sz2: plongint); stdcall; external;
procedure ReadSXSYfifo(sxy0, sxy1, sxy2: plongint); stdcall; external;
procedure ReadRGBfifo(v0, v1, v2: PCVECTOR); stdcall; external;
procedure ReadGeomOffset(ofx, ofy: plongint); stdcall; external;
function ReadGeomScreen: longint; stdcall; external;

procedure TransRot_32(v0, v1: PVECTOR; flag: plongint); stdcall; external;
function TransRotPers(v0: PSVECTOR; sxy: plongint; p: plongint; flag: plongint): longint; stdcall; external;
function TransRotPers3(v0, v1, v2: PSVECTOR; sxy0, sxy1, sxy2: plongint; p: plongint; flag: plongint): longint; stdcall; external;

//procedure pers_map(abuf: longint; vertex: pointer; int tex[4][2], u_short *dtext); stdcall; external;
procedure PhongLine(istart_x, iend_x: longint; p, q: longint; pixx: pointer; fs, ft, i4, det: longint); stdcall; external;

function RotTransPers(v0: PSVECTOR; sxy, p, flag: plongint): longint; stdcall; external;
function RotTransPers3(v0, v1, v2: PSVECTOR; sxy0, sxy1, sxy2: plongint; p: plongint; flag: plongint): longint; stdcall; external;
procedure RotTrans(v0: PSVECTOR; v1: PVECTOR; flag: plongint); stdcall; external;
procedure RotTransSV(v0, v1: PSVECTOR; flag: plongint); stdcall; external;
procedure LocalLight(v0: PSVECTOR; v1: PVECTOR); stdcall; external;
procedure LightColor(v0, v1: PVECTOR); stdcall; external;
procedure DpqColorLight(v0: PVECTOR; v1: PCVECTOR; p: longint; v2: PCVECTOR); stdcall; external;
procedure DpqColor(v0: PCVECTOR; p: longint; v1: PCVECTOR); stdcall; external;
procedure DpqColor3(v0, v1, v2: PCVECTOR; p: longint; v3, v4, v5: PCVECTOR); stdcall; external;
procedure Intpl(v0:PVECTOR; p: longint; v1: PCVECTOR); stdcall; external;
function Square12(v0, v1: PVECTOR):PVECTOR; stdcall; external;
function Square0(v0, v1: PVECTOR):PVECTOR; stdcall; external;
function SquareSL12(v0: PSVECTOR; v1: PVECTOR): PVECTOR; stdcall; external;
function SquareSL0(v0: PSVECTOR; v1: PVECTOR): PVECTOR; stdcall; external;
function SquareSS12(v0, v1: PSVECTOR): PSVECTOR; stdcall; external;
function SquareSS0(v0, v1: PSVECTOR): PSVECTOR; stdcall; external;
procedure NormalColor(v0: PSVECTOR; v1: PCVECTOR); stdcall; external;
procedure NormalColor3(v0, v1, v2: PSVECTOR; v3, v4, v5:PCVECTOR); stdcall; external;
procedure NormalColorDpq(v0: PSVECTOR; v1: PCVECTOR; p: longint; v2: PCVECTOR); stdcall; external;
procedure NormalColorDpq3(v0, v1, v2: PSVECTOR; v3: PCVECTOR; p: longint; v4, v5, v6: PCVECTOR); stdcall; external;
procedure NormalColorCol(v0: PSVECTOR; v1, v2: PCVECTOR); stdcall; external;
procedure NormalColorCol3(v0, v1, v2: PSVECTOR; v3, v4, v5, v6: PCVECTOR); stdcall; external;
procedure ColorDpq(v0: PVECTOR; v1: PCVECTOR; p: longint; v2: PCVECTOR); stdcall; external;
procedure ColorCol(v0: PVECTOR; v1, v2: PCVECTOR); stdcall; external;
function NormalClip(sxy0, sxy1, sxy2: longint): longint; stdcall; external;
function AverageZ3(sz0, sz1, sz2: longint): longint; stdcall; external;
function AverageZ4(sz0, sz1, sz2, sz3: longint): longint; stdcall; external;
procedure OuterProduct12(v0, v1, v2: PVECTOR); stdcall; external;
procedure OuterProduct0(v0, v1, v2: PVECTOR); stdcall; external;
function Lzc(data: longint): longint; stdcall; external;

function RotTransPers4(v0, v1, v2, v3: PSVECTOR; sxy0, sxy1, sxy2, sxy3: plongint; p, flag: plongint): longint; stdcall; external;
procedure RotTransPersN(v0: PSVECTOR; v1: PDVECTOR; sz: pword; p, flag: pword; n: longint); stdcall; external;
procedure RotTransPers3N(v0: PSVECTOR; v1: PDVECTOR; sz: pword; flag: pword; n: longint); stdcall; external;
procedure RotMeshH(Yheight: pword; Vo: PDVECTOR; sz: pword; flag: pword; Xoffset, Zoffset: smallint; m, n: smallint; base: PDVECTOR); stdcall; external;
function RotAverage3(v0, v1, v2: PSVECTOR; sxy0, sxy1, sxy2: plongint; p, flag: plongint): longint; stdcall; external;
function RotAverage4(v0, v1, v2, v3: PSVECTOR; sxy0, sxy1, sxy2, sxy3: plongint; p, flag: plongint): longint; stdcall; external;
function RotNclip3(v0, v1, v2: PSVECTOR; sxy0, sxy1, sxy2: plongint; p, otz, flag: plongint): longint; stdcall; external;
function RotNclip4(v0, v1, v2, v3: PSVECTOR; sxy0, sxy1, sxy2, sxy3: plongint; p, otz, flag: plongint): longint; stdcall; external;
function RotAverageNclip3(v0, v1, v2: PSVECTOR; sxy0, sxy1, sxy2: plongint; p, otz, flag: plongint): longint; stdcall; external;
function RotAverageNclip4(v0, v1, v2, v3: PSVECTOR; sxy0, sxy1, sxy2, sxy3: plongint; p, otz, flag: plongint): longint; stdcall; external;
function RotColorDpq(v0, v1: PSVECTOR; v2: PCVECTOR; sxy: plongint; v3: PCVECTOR; flag: plongint): longint; stdcall; external;
function RotColorDpq3(v0, v1, v2, v3, v4, v5: PSVECTOR; v6: PCVECTOR; sxy0, sxy1, sxy2: plongint; v7, v8, v9: PCVECTOR; flag: plongint): longint; stdcall; external;
function RotAverageNclipColorDpq3(v0, v1, v2, v3, v4, v5: PSVECTOR; v6: PCVECTOR; sxy0, sxy1, sxy2: plongint; v7, v8, v9: PCVECTOR; otz, flag: plongint): longint; stdcall; external;
function RotAverageNclipColorCol3(v0, v1, v2, v3, v4, v5: PSVECTOR; v6: PCVECTOR; sxy0, sxy1, sxy2: plongint; v7, v8, v9: PCVECTOR; otz, flag: plongint): longint; stdcall; external;
function RotColorMatDpq(v0, v1: PSVECTOR; v2: PCVECTOR; sxy: plongint; v3: PCVECTOR; matc, flag: longint): longint; stdcall; external;
procedure ColorMatDpq(v0: PSVECTOR; v1: PCVECTOR; p: longint; v2: PCVECTOR; matc: longint); stdcall; external;
procedure ColorMatCol(v0: PSVECTOR; v1, v2: PCVECTOR; matc: longint); stdcall; external;
procedure LoadAverage12(v0, v1: PVECTOR; p0, p1: longint; v2: PVECTOR); stdcall; external;
procedure LoadAverageShort12(v0, v1: PSVECTOR; p0, p1: longint; v2: PSVECTOR); stdcall; external;
procedure LoadAverage0(v0, v1: PVECTOR; p0, p1: longint; v2: PVECTOR); stdcall; external;
procedure LoadAverageShort0(v0, v1: PSVECTOR; p0, p1: longint; v2: PSVECTOR); stdcall; external;
procedure LoadAverageByte(v0, v1: pbyte; p0, p1: longint; v2: pbyte); stdcall; external;
procedure LoadAverageCol(v0, v1: pbyte; p0, p1: longint; v2: pbyte); stdcall; external;
function VectorNormal(v0, v1: PVECTOR): longint; stdcall; external;
function VectorNormalS(v0: PVECTOR; v1: PSVECTOR): longint; stdcall; external;
function VectorNormalSS(v0, v1: PSVECTOR): longint; stdcall; external;
function SquareRoot0(a: longint): longint; stdcall; external;
function SquareRoot12(a: longint): longint; stdcall; external;
procedure InvSquareRoot(a: longint; b, c: plongint); stdcall; external;
procedure gteMIMefunc(otp, dfp: PSVECTOR; n, p: plongint); stdcall; external;
procedure SetFogFar(a, h: longint); stdcall; external;
procedure SetFogNear(a, h: longint); stdcall; external;
procedure SetFogNearFar(a, b, h: plongint); stdcall; external;
procedure SubPol4(p: PPOL4; sp: PSPOL; ndiv: longint); stdcall; external;
procedure SubPol3(p: PPOL3; sp: PSPOL; ndiv: longint); stdcall; external;

function rcos(a: longint): longint; stdcall; external;
function rsin(a: longint): longint; stdcall; external;
function ccos(a: longint): longint; stdcall; external;
function csin(a: longint): longint; stdcall; external;
function cln(a: longint): longint; stdcall; external;
function csqrt(a: longint): longint; stdcall; external;
function catan(a: longint): longint; stdcall; external;

function ratan2(y, x: longint): longint; stdcall; external;


procedure RotPMD_F3(pa: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;
procedure RotPMD_G3(pa: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;
procedure RotPMD_FT3(pa: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;
procedure RotPMD_GT3(pa: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;
procedure RotPMD_F4(pa: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;
procedure RotPMD_G4(pa: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;
procedure RotPMD_FT4(pa: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;
procedure RotPMD_GT4(pa: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;

procedure RotPMD_SV_F3(pa, va: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;
procedure RotPMD_SV_G3(pa, va: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;
procedure RotPMD_SV_FT3(pa, va: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;
procedure RotPMD_SV_GT3(pa, va: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;
procedure RotPMD_SV_F4(pa, va: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;
procedure RotPMD_SV_G4(pa, va: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;
procedure RotPMD_SV_FT4(pa, va: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;
procedure RotPMD_SV_GT4(pa, va: plongint; ot: pdword; otlen, id, backc: longint); stdcall; external;


procedure InitClip(evbfad: PEVECTOR; hw, vw, h, _near, _far: longint); stdcall; external;
function Clip3F(v0, v1, v2: PSVECTOR; evmx: PPEVECTOR): longint; stdcall; external;
function Clip3FP(v0, v1, v2: PSVECTOR; evmx: PPEVECTOR): longint; stdcall; external;
function Clip4F(v0, v1, v2, v3: PSVECTOR; evmx: PPEVECTOR): longint; stdcall; external;
function Clip4FP(v0, v1, v2, v3: PSVECTOR; evmx: PPEVECTOR): longint; stdcall; external;
function Clip3FT(v0, v1, v2: PSVECTOR; uv0, uv1, uv2: psmallint; evmx: PPEVECTOR): longint; stdcall; external;
function Clip3FTP(v0, v1, v2: PSVECTOR; uv0, uv1, uv2: psmallint; evmx: PPEVECTOR): longint; stdcall; external;
function Clip4FT(v0, v1, v2, v3: PSVECTOR; uv0, uv1, uv2, uv3: psmallint; evmx: PPEVECTOR): longint; stdcall; external;
function Clip4FTP(v0, v1, v2, v3: PSVECTOR; uv0, uv1, uv2, uv3: psmallint; evmx: PPEVECTOR): longint; stdcall; external;
function Clip3G(v0, v1, v2: PSVECTOR; rgb0, rgb1, rgb2: PCVECTOR; evmx: PPEVECTOR): longint; stdcall; external;
function Clip3GP(v0, v1, v2: PSVECTOR; rgb0, rgb1, rgb2: PCVECTOR; evmx: PPEVECTOR): longint; stdcall; external;
function Clip4G(v0, v1, v2, v3: PSVECTOR; rgb0, rgb1, rgb2, rgb3: PCVECTOR; evmx: PPEVECTOR): longint; stdcall; external;
function Clip4GP(v0, v1, v2, v3: PSVECTOR; rgb0, rgb1, rgb2, rgb3: PCVECTOR; evmx: PPEVECTOR): longint; stdcall; external;
function Clip3GT(v0, v1, v2: PSVECTOR; uv0, uv1, uv2: psmallint; rgb0, rgb1, rgb2: PCVECTOR; evmx: PPEVECTOR): longint; stdcall; external;
function Clip3GTP(v0, v1, v2: PSVECTOR; uv0, uv1, uv2: psmallint; rgb0, rgb1, rgb2: PCVECTOR; evmx: PPEVECTOR): longint; stdcall; external;
function Clip4GT(v0, v1, v2, v3: PSVECTOR; uv0, uv1, uv2, uv3: psmallint; rgb0, rgb1, rgb2, rgb3: PCVECTOR; evmx: PPEVECTOR): longint; stdcall; external;
function Clip4GTP(v0, v1, v2, v3: PSVECTOR; uv0, uv1, uv2, uv3: psmallint; rgb0, rgb1, rgb2, rgb3: PCVECTOR; evmx: PPEVECTOR): longint; stdcall; external;

procedure RotTransPers_nom(v0: PSVECTOR); stdcall; external;
procedure RotTransPers3_nom(v0, v1, v2: PSVECTOR); stdcall; external;
procedure RotTransPers4_nom(v0, v1, v2, v3: PSVECTOR); stdcall; external;
procedure RotTrans_nom(v0: PSVECTOR); stdcall; external;
procedure RotAverage3_nom(v0, v1, v2: PSVECTOR); stdcall; external;
procedure RotNclip3_nom(v0, v1, v2: PSVECTOR); stdcall; external;
procedure RotAverageNclip3_nom(v0, v1, v2: PSVECTOR); stdcall; external;
procedure RotAverageNclipColorDpq3_nom(v0, v1, v2, v3, v4, v5: PSVECTOR; v6: PCVECTOR); stdcall; external;
procedure RotAverageNclipColorCol3_nom(v0, v1, v2, v3, v4, v5: PSVECTOR; v6: PCVECTOR); stdcall; external;
procedure RotColorDpq_nom(v0, v1: PSVECTOR; v2: PCVECTOR); stdcall; external;
function RotColorDpq3_nom(v0, v1, v2, v3, v4, v5: PSVECTOR; v6: PCVECTOR): longint; stdcall; external;
procedure NormalColor_nom(v0: PSVECTOR); stdcall; external;
procedure NormalColor3_nom(v0, v1, v2: PSVECTOR); stdcall; external;
procedure NormalColorDpq_nom(v0: PSVECTOR; v1: PCVECTOR; p: longint); stdcall; external;
procedure NormalColorDpq3_nom(v0, v1, v2: PSVECTOR; v3: PCVECTOR; p: longint); stdcall; external;
procedure NormalColorCol_nom(v0: PSVECTOR; v1: PCVECTOR); stdcall; external;
procedure NormalColorCol3_nom(v0, v1, v2: PSVECTOR; v3: PCVECTOR); stdcall; external;


procedure RotSMD_F3(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;	
procedure RotSMD_G3(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;	
procedure RotSMD_FT3(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint);	 stdcall; external;	
procedure RotSMD_GT3(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotSMD_F4(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotSMD_G4(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotSMD_FT4(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotSMD_GT4(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		

procedure RotSMD_SV_F3(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint);	 stdcall; external;	
procedure RotSMD_SV_G3(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotSMD_SV_FT3(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotSMD_SV_GT3(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotSMD_SV_F4(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotSMD_SV_G4(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotSMD_SV_FT4(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotSMD_SV_GT4(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		



procedure RotRMD_F3(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;	
procedure RotRMD_G3(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;	
procedure RotRMD_FT3(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotRMD_GT3(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotRMD_F4(pa: plongint;  ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotRMD_G4(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotRMD_FT4(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotRMD_GT4(pa: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		

procedure RotRMD_SV_F3(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotRMD_SV_G3(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotRMD_SV_FT3(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotRMD_SV_GT3(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotRMD_SV_F4(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotRMD_SV_G4(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotRMD_SV_FT4(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		
procedure RotRMD_SV_GT4(pa: plongint; va: plongint; ot: pdword; otlen, id, sclip, hclip, vclip, nclipmode: longint); stdcall; external;		

function p2otz(p, projection: longint): longint; stdcall; external;
function otz2p(otz, projection: longint): longint; stdcall; external;



implementation
begin
end.