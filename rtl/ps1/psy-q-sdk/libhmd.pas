// libhmd.h: Hierarchical Modeling Data Library
unit libhmd;
interface
uses libgte, libgs;
const
	GsUNIT_TERM	= $ffffffff;	// Primitive terminator
	GsUNIT_DIV1	= 1 shl 24;		//  2 x  2 divide
	GsUNIT_DIV2	= 2 shl 24;		//  4 x  4 divide
	GsUNIT_DIV3	= 3 shl 24;		//  8 x  8 divide
	GsUNIT_DIV4	= 4 shl 24;		// 16 x 16 divide
	GsUNIT_DIV5	= 5 shl 24;		// 32 x 32 divide

type
	PGsCOORDUNIT = ^GsCOORDUNIT;
	GsCOORDUNIT = packed record
		flg : dword;
		matrix : MATRIX;
		workm : MATRIX;
		rot : SVECTOR;
		super : PGsCOORDUNIT;
	end;

	PGsVIEWUNIT = ^GsVIEWUNIT;
	GsVIEWUNIT = packed record
		view : MATRIX;
		super : PGsCOORDUNIT;
	end;

	PGsRVIEWUNIT = ^GsRVIEWUNIT;
	GsRVIEWUNIT = packed record
		vpx, vpy, vpz : longint;
		vrx, vry, vrz : longint;
		rz : longint;
		super : PGsRVIEWUNIT;
	end;

	PGsUNIT = ^GsUNIT;
	GsUNIT = packed record
		coord : PGsCOORDUNIT;	// local dmatrix
		primtop : pdword;
	end;

	PGsTYPEUNIT = ^GsTYPEUNIT;
	GsTYPEUNIT = packed record
		_type : dword;
		ptr : pointer;
	end;

	PGsARGUNIT = ^GsARGUNIT;
	GsARGUNIT = packed record
		primp : pdword;
		tagp : PGsOT;
		shift : longint;
		offset : longint;
		out_packetp : PPACKET;
	end;

	PGsWORKUNIT = ^GsWORKUNIT;
	GsWORKUNIT = packed record
		vec : DVECTOR;
		otz : smallint;
		p : smallint;
	end;

	GsARGUNIT_NORMAL = packed record
		primp : pdword;
		tagp : PGsOT;
		shift : longint;
		offset : longint;
		out_packetp : PPACKET;
		primtop : pdword;
		vertop : PSVECTOR;
		nortop : PSVECTOR;
	end;

	GsARGUNIT_SHARED = packed record
		primp : pdword;
		tagp : PGsOT;
		shift : longint;
		offset : longint;
		out_packetp : PPACKET;
		primtop : pdword;
		vertop : PSVECTOR;
		vertop2 : PGsWORKUNIT;
		nortop : PSVECTOR;
		nortop2 : PSVECTOR;
	end;

	GsARGUNIT_IMAGE = packed record
		primp : pdword;
		tagp : PGsOT;
		shift : longint;
		offset : longint;
		out_packetp : PPACKET;
		imagetop : pdword;
		cluttop : pdword;
	end;

	GsARGUNIT_GND = packed record
		primp : pdword;
		tagp : PGsOT;
		shift : longint;
		offset : longint;
		out_packetp : PPACKET;
		polytop : pdword;
		boxtop : pdword;
		pointtop : pdword;
		nortop : PSVECTOR;
	end;

	GsARGUNIT_GNDT = packed record
		primp : pdword;
		tagp : PGsOT;
		shift : longint;
		offset : longint;
		out_packetp : PPACKET;
		polytop : pdword;
		boxtop : pdword;
		pointtop : pdword;
		nortop : PSVECTOR;
		uvtop : pdword;
	end;

	GsARGUNIT_CAMERA = packed record
		primp : pdword;
		tagp : PGsOT;
		shift : longint;
		offset : longint;
		out_packetp : PPACKET;
		cparam : pdword;
		coord : PGsCOORDUNIT;
		rcoord : PGsCOORDUNIT;
	end;

	GsARGUNIT_LIGHT = packed record
		primp : pdword;
		tagp : PGsOT;
		shift : longint;
		offset : longint;
		out_packetp : PPACKET;
		lparam : pdword;
		coord : PGsCOORDUNIT;
		rcoord : PGsCOORDUNIT;
	end;

	GsARGUNIT_JntMIMe = packed record
		primp : pdword;
		tagp : PGsOT;
		shift : longint;
		offset : longint;
		out_packetp : PPACKET;
		coord_sect : pdword;
		mimepr : plongint;
		mimenum : dword;
		mimeid, reserved : smallint;
		mime_diff_sect : pdword;
	end;


	GsARGUNIT_RstJntMIMe = packed record
		primp : pdword;
		tagp : PGsOT;
		shift : longint;
		offset : longint;
		out_packetp : PPACKET;
		coord_sect : pdword;
		mimeid, reserved : smallint;
		mime_diff_sect : pdword;
	end;

	GsARGUNIT_VNMIMe = packed record
		primp : pdword;
		tagp : PGsOT;
		shift : longint;
		offset : longint;
		out_packetp : PPACKET;
		mimepr : plongint;
		mimenum : dword;
		mimeid, reserved : smallint;
		mime_diff_sect : pdword;
		orgs_vn_sect : PSVECTOR;
		vert_sect : PSVECTOR;
		norm_sect : PSVECTOR;
	end;

	GsARGUNIT_RstVNMIMe = packed record
		primp : pdword;
		tagp : PGsOT;
		shift : longint;
		offset : longint;
		out_packetp : PPACKET;
		mimeid, reserved : smallint;
		mime_diff_sect : pdword;
		orgs_vn_sect : PSVECTOR;
		vert_sect : PSVECTOR;
		norm_sect : PSVECTOR;
	end;

	PGsARGUNIT_ANIM = ^GsARGUNIT_ANIM;
	GsARGUNIT_ANIM = packed record
		primp : pdword;
		tagp : PGsOT;
		shift : longint;
		offset : longint;
		out_packetp : PPACKET;
		header_size : longint;
		htop : pdword;
		ctop : pdword;
		ptop : pdword;
	end;

	GsSEH = packed record
		idx : smallint;
		sid : byte;
		pad : byte;
	end;

	PGsSEQ = ^GsSEQ;
	GsSEQ = packed record
		rewrite_idx : dword;
		size, num : word;
		ii : word;
		aframe : word;
		sid : byte;
		speed : shortint;
		srcii : word;
		rframe : smallint;
		tframe : word;
		ci, ti : word;
		start : word;
		start_sid : byte;
		traveling : byte;
	end;
	PPGsSEQ = ^PGsSEQ;



// GsTYPEUNIT code macro
const
	GsUF3		= $00000008;	// flat triangle
	GsUFT3		= $00000009;	// texture flat triangle
	GsUG3		= $0000000c;	// gour triangle
	GsUGT3		= $0000000d;	// texture gour triangle
	GsUF4		= $00000010;	// flat quad
	GsUFT4		= $00000011;	// texture flat quad
	GsUG4		= $00000014;	// gour quad
	GsUGT4		= $00000015;	// texture gour quad

	GsUFF3		= $00020008;	// fog flat triangle
	GsUFFT3		= $00020009;	// fog texture flat triangle
	GsUFG3		= $0002000c;	// fog gour triangle
	GsUFGT3		= $0002000d;	// fog texture gour triangle
	GsUFF4		= $00020010;	// fog flat quad
	GsUFFT4		= $00020011;	// fog texture flat quad
	GsUFG4		= $00020014;	// fog gour quad
	GsUFGT4		= $00020015;	// fog texture gour quad

	GsUCF3		= $0000000a;	// colored flat triangle
	GsUCFT3		= $0000000b;	// colored texture flat triangle
	GsUCG3		= $0000000e;	// colored gour triangle
	GsUCGT3		= $0000000f;	// colored texture gour triangle
	GsUCF4		= $00000012;	// colored flat quad
	GsUCFT4		= $00000013;	// colored texture flat quad
	GsUCG4		= $00000016;	// colored gour quad
	GsUCGT4		= $00000017;	// colored texture gour quad

	GsUNF3		= $00040048;	// nonLight flat triangle
	GsUNFT3		= $00040049;	// nonLight texture flat triangle
	GsUNG3		= $0004004c;	// nonLight gouraud triangle
	GsUNGT3		= $0004004d;	// nonLight texture gouraud triangle
	GsUNF4		= $00040050;	// nonLight flat quad
	GsUNFT4		= $00040051;	// nonLight texture flat quad
	GsUNG4		= $00040054;	// nonLight gouraud quad
	GsUNGT4		= $00040055;	// nonLight texture gouraud quad

	GsUDF3		= $00010008;	// div flat triangle
	GsUDFT3		= $00010009;	// div texture flat triangle
	GsUDG3		= $0001000c;	// div gour triangle
	GsUDGT3		= $0001000d;	// div texture gour triangle
	GsUDF4		= $00010010;	// div flat quad
	GsUDFT4		= $00010011;	// div texture flat quad
	GsUDG4		= $00010014;	// div gour quad
	GsUDGT4		= $00010015;	// div texture gour quad

	GsUDFF3		= $00030008;	// div fog flat triangle
	GsUDFFT3	= $00030009;	// div fog texture flat triangle
	GsUDFG3		= $0003000c;	// div fog gour triangle
	GsUDFGT3	= $0003000d;	// div fog texture gour triangle
	GsUDFF4		= $00030010;	// div fog flat quad
	GsUDFFT4	= $00030011;	// div fog texture flat quad
	GsUDFG4		= $00030014;	// div fog gour quad
	GsUDFGT4	= $00030015;	// div fog texture gour quad

	GsUDNF3		= $00050048;	// div nonLight flat triangle
	GsUDNFT3	= $00050049;	// div nonLight texture flat triangle
	GsUDNG3		= $0005004c;	// div nonLight gouraud triangle
	GsUDNGT3	= $0005004d;	// div nonLight tex gouraud triangle
	GsUDNF4		= $00050050;	// div nonLight flat quad
	GsUDNFT4	= $00050051;	// div nonLight texture flat quad
	GsUDNG4		= $00050054;	// div nonLight gouraud quad
	GsUDNGT4	= $00050055;	// div nonLight tex gouraud quad

	GsUSCAL		= $01000000;	// shared calculate vertex and normal
	GsUSG3		= $0100000c;	// shared gour triangle
	GsUSGT3		= $0100000d;	// shared texture gour triangle
	GsUSG4		= $01000014;	// shared gour quad
	GsUSGT4		= $01000015;	// shared texture gour quad

	GsUSTGT3	= $0100020d;	// shared tile texture gour triangle
	GsUSTGT4	= $01000215;	// shared tile texture gour quad

	GsUSFG3		= $0102000c;	// shared fog gour triangle
	GsUSFGT3	= $0102000d;	// shared fog texture gour triangle
	GsUSFG4		= $01020014;	// shared fog gour quad
	GsUSFGT4	= $01020015;	// shared fog texture gour quad

	GsUSNF3		= $01040048;	// shared nonLight flat tri
	GsUSNFT3	= $01040049;	// shared nonLight texture flat tri
	GsUSNG3		= $0104004c;	// shared nonLight gour tri
	GsUSNGT3	= $0104004d;	// shared nonLight texture gour tri
	GsUSNF4		= $01040050;	// shared nonLight flat quad
	GsUSNFT4	= $01040051;	// shared nonLight texture flat quad
	GsUSNG4		= $01040054;	// shared nonLight gour quad
	GsUSNGT4	= $01040055;	// shared nonLight texture gour quad

	GsUMF3		= $00000018;	// mesh flat tri
	GsUMFT3		= $00000019;	// mesh texture flat tri
	GsUMG3		= $0000001c;	// mesh gour triangle
	GsUMGT3		= $0000001d;	// mesh texture gour triangle
	GsUMNF3		= $00040058;	// mesh nonLight flat tri
	GsUMNFT3	= $00040059;	// mesh nonLight tex flat tri
	GsUMNG3		= $0004005c;	// mesh nonLight gour triangle
	GsUMNGT3	= $0004005d;	// mesh nonLight tex gour tri

	GsUTFT3		= $00000209;	// tile texture flat triangle
	GsUTGT3		= $0000020d;	// tile texture gour triangle
	GsUTFT4		= $00000211;	// tile texture flat quad
	GsUTGT4		= $00000215;	// tile texture gour quad

	GsUPNF3		= $00040148;	// preset nonLight flat triangle
	GsUPNFT3	= $00040149;	// preset nonLight tex flat triangle
	GsUPNG3		= $0004014c;	// preset nonLight gouraud triangle
	GsUPNGT3	= $0004014d;	// preset nonLight tex gour triangle
	GsUPNF4		= $00040150;	// preset nonLight flat quad
	GsUPNFT4	= $00040151;	// preset nonLight tex flat quad
	GsUPNG4		= $00040154;	// preset nonLight gouraud quad
	GsUPNGT4	= $00040155;	// preset nonLight tex gour quad

	GsUSTPF3	= $00200008;	// semi-trans flat triangle
	GsUSTPFT3	= $00200009;	// semi-trans texture flat triangle
	GsUSTPG3	= $0020000c;	// semi-trans gour triangle
	GsUSTPGT3	= $0020000d;	// semi-trans texture gour triangle
	GsUSTPF4	= $00200010;	// semi-trans flat quad
	GsUSTPFT4	= $00200011;	// semi-trans texture flat quad
	GsUSTPG4	= $00200014;	// semi-trans gour quad
	GsUSTPGT4	= $00200015;	// semi-trans texture gour quad
	GsUSTPSG3	= $0120000c;	// semi-trans shared gour tri
	GsUSTPSGT3	= $0120000d;	// semi-trans shared tex gour tri
	GsUSTPSG4	= $01200014;	// semi-trans shared gour quad
	GsUSTPSGT4	= $01200015;	// semi-trans shared tex gour quad

	GsUSTPNF3	= $00240048;	// semi-trans nonLight flat tri
	GsUSTPNFT3	= $00240049;	// semi-trans nonLight tex flat tri
	GsUSTPNG3	= $0024004c;	// semi-trans nonLight gour tri
	GsUSTPNGT3	= $0024004d;	// semi-trans nonLight tex gour tri
	GsUSTPNF4	= $00240050;	// semi-trans nonLight flat quad
	GsUSTPNFT4	= $00240051;	// semi-trans nonLight tex flat quad
	GsUSTPNG4	= $00240054;	// semi-trans nonLight gour quad
	GsUSTPNGT4	= $00240055;	// semi-trans nonLight tex gour quad

	GsUSTPSNF3	= $01240048;	// stp shared nonLight flat tri
	GsUSTPSNFT3	= $01240049;	// stp shared nonLight tex flat tri
	GsUSTPSNG3	= $0124004c;	// stp shared nonLight gour tri
	GsUSTPSNGT3	= $0124004d;	// stp shared nonLight tex gour tri
	GsUSTPSNF4	= $01240050;	// stp shared nonLight flat quad
	GsUSTPSNFT4	= $01240051;	// stp shared nonLight tex flat quad
	GsUSTPSNG4	= $01240054;	// stp shared nonLight gour quad
	GsUSTPSNGT4	= $01240055;	// stp shared nonLight tex gour quad

	GsUADF3		= $00080008;	// active-div flat triangle
	GsUADFT3	= $00080009;	// active-div texture flat triangle
	GsUADG3		= $0008000c;	// active-div gour triangle
	GsUADGT3	= $0008000d;	// active-div texture gour triangle
	GsUADF4		= $00080010;	// active-div flat quad
	GsUADFT4	= $00080011;	// active-div texture flat quad
	GsUADG4		= $00080014;	// active-div gour quad
	GsUADGT4	= $00080015;	// active-div texture gour quad

	GsUADFF3	= $000a0008;	// active-div fog flat tri
	GsUADFFT3	= $000a0009;	// active-div fog texture flat tri
	GsUADFG3	= $000a000c;	// active-div fog gour tri
	GsUADFGT3	= $000a000d;	// active-div fog texture gour tri
	GsUADFF4	= $000a0010;	// active-div fog flat quad
	GsUADFFT4	= $000a0011;	// active-div fog texture flat quad
	GsUADFG4	= $000a0014;	// active-div fog gour quad
	GsUADFGT4	= $000a0015;	// active-div fog texture gour quad

	GsUADNF3	= $000c0048;	// active-div nonLight flat tri
	GsUADNFT3	= $000c0049;	// active-div nonLight tex flat tri
	GsUADNG3	= $000c004c;	// active-div nonLight gour tri
	GsUADNGT3	= $000c004d;	// active-div nonLight tex gour tri
	GsUADNF4	= $000c0050;	// active-div nonLight flat quad
	GsUADNFT4	= $000c0051;	// active-div nonLight tex flat quad
	GsUADNG4	= $000c0054;	// active-div nonLight gour quad
	GsUADNGT4	= $000c0055;	// active-div nonLight tex gour quad

	GsUBF3		= $00100008;	// back-f flat tri
	GsUBFT3		= $00100009;	// back-f tex flat tri
	GsUBG3		= $0010000c;	// back-f gour tri
	GsUBGT3		= $0010000d;	// back-f tex gour tri
	GsUBF4		= $00100010;	// back-f flat quad
	GsUBFT4		= $00100011;	// back-f tex flat quad
	GsUBG4		= $00100014;	// back-f gour quad
	GsUBGT4		= $00100015;	// back-f tex gour quad

	GsUBCF3		= $0010000a;	// back-f colored flat tri
	GsUBCFT3	= $0010000b;	// back-f colored tex flat tri
	GsUBCG3		= $0010000e;	// back-f colored gour tri
	GsUBCGT3	= $0010000f;	// back-f colored tex gour tri
	GsUBCF4		= $00100012;	// back-f colored flat quad
	GsUBCFT4	= $00100013;	// back-f colored tex flat quad
	GsUBCG4		= $00100016;	// back-f colored gour quad
	GsUBCGT4	= $00100017;	// back-f colored tex gour quad

	GsUBSTPF3	= $00300008;	// back-f semi-trans flat tri
	GsUBSTPFT3	= $00300009;	// back-f semi-trans tex flat tri
	GsUBSTPG3	= $0030000c;	// back-f semi-trans gour tri
	GsUBSTPGT3	= $0030000d;	// back-f semi-trans tex gour tri
	GsUBSTPF4	= $00300010;	// back-f semi-trans flat quad
	GsUBSTPFT4	= $00300011;	// back-f semi-trans tex flat quad
	GsUBSTPG4	= $00300014;	// back-f semi-trans gour quad
	GsUBSTPGT4	= $00300015;	// back-f semi-trans tex gour quad

	GsUBNF3		= $00140048;	// back-f noLgt flat tri
	GsUBNFT3	= $00140049;	// back-f noLgt tex flat tri
	GsUBNG3		= $0014004c;	// back-f noLgt gouraud tri
	GsUBNGT3	= $0014004d;	// back-f noLgt tex gouraud tri
	GsUBNF4		= $00140050;	// back-f noLgt flat quad
	GsUBNFT4	= $00140051;	// back-f noLgt tex flat quad
	GsUBNG4		= $00140054;	// back-f noLgt gouraud quad
	GsUBNGT4	= $00140055;	// back-f noLgt tex gouraud quad

	GsUBSTPNF3	= $00340048;	// back-f stp noLgt flat tri
	GsUBSTPNFT3	= $00340049;	// back-f stp noLgt tex flat tri
	GsUBSTPNG3	= $0034004c;	// back-f stp noLgt gour tri
	GsUBSTPNGT3	= $0034004d;	// back-f stp noLgt tex gour tri
	GsUBSTPNF4	= $00340050;	// back-f stp noLgt flat quad
	GsUBSTPNFT4	= $00340051;	// back-f stp noLgt tex flat quad
	GsUBSTPNG4	= $00340054;	// back-f stp noLgt gour quad
	GsUBSTPNGT4	= $00340055;	// back-f stp noLgt tex gour quad

	GsUBSNF3	= $01140048;	// back-f shrd noLgt flat tri
	GsUBSNFT3	= $01140049;	// back-f shrd noLgt tex flat tri
	GsUBSNG3	= $0114004c;	// back-f shrd noLgt gour tri
	GsUBSNGT3	= $0114004d;	// back-f shrd noLgt tex gour tri
	GsUBSNF4	= $01140050;	// back-f shrd noLgt flat quad
	GsUBSNFT4	= $01140051;	// back-f shrd noLgt tex flat quad
	GsUBSNG4	= $01140054;	// back-f shrd noLgt gour quad
	GsUBSNGT4	= $01140055;	// back-f shrd noLgt tex gour quad

	GsUBSTPSNF3		= $01340048;	// back-f stp shrd noLgt flat tri
	GsUBSTPSNFT3	= $01340049;	// back-f stp shrd noLgt tex flat tri
	GsUBSTPSNG3		= $0134004c;	// back-f stp shrd noLgt gour tri
	GsUBSTPSNGT3	= $0134004d;	// back-f stp shrd noLgt tex gour tri
	GsUBSTPSNF4		= $01340050;	// back-f stp shrd noLgt flat quad
	GsUBSTPSNFT4	= $01340051;	// back-f stp shrd noLgt tex flat quad
	GsUBSTPSNG4		= $01340054;	// back-f stp shrd noLgt gour quad
	GsUBSTPSNGT4	= $01340055;	// back-f stp shrd noLgt tex gour quad

	GsUIMG0		= $02000000;	// image data with no-clut
	GsUIMG1		= $02000001;	// image data with clut

	GsUGNDF		= $05000000;	// ground flat
	GsUGNDFT	= $05000001;	// ground flat texture

	GsUSCAL2	= $06000100;	// envmap shared calculate
	GsUE1G3		= $0600100c;	// envmap 1D gour tri
	GsUE1G4		= $06001014;	// envmap 1D gour quad
	GsUE1SG3	= $0600110c;	// envmap 1D shared gour tri
	GsUE1SG4	= $06001114;	// envmap 1D shared gour quad
	GsUE2LG3	= $0600200c;	// envmap 2D reflect gour tri
	GsUE2LG4	= $06002014;	// envmap 2D reflect gour quad
	GsUE2RG3	= $0600300c;	// envmap 2D refract gour tri
	GsUE2RG4	= $06003014;	// envmap 2D refract gour quad
	GsUE2RLG3	= $0600400c;	// envmap 2D both gour tri
	GsUE2RLG4	= $06004014;	// envmap 2D both gour quad
	GsUE2OLG3	= $0600500c;	// envmap 2D org+reflect gour tri
	GsUE2OLG4	= $06005014;	// envmap 2D org+reflect gour quad

	GsVtxMIMe		 = $04010020;	// Vertex-MIMe
	GsNrmMIMe		 = $04010021;	// Normal-MIMe
	GsRstVtxMIMe	 = $04010028;	// Reset-Vertex-MIMe
	GsRstNrmMIMe	 = $04010029;	// Reset-Normal-MIMe
	GsJntAxesMIMe	 = $04010010;	// Joint-Axes-MIMe
	GsRstJntAxesMIMe = $04010018;	// Reset-Joint-Axes-MIMe
	GsJntRPYMIMe	 = $04010011;	// Joint-RPY-MIMe
	GsRstJntRPYMIMe	 = $04010019;	// Reset-Joint-RPY-MIMe





	function GsU_00000008(arg: PGsARGUNIT): pdword; external;
	function GsU_00000009(arg: PGsARGUNIT): pdword; external;
	function GsU_0000000c(arg: PGsARGUNIT): pdword; external;
	function GsU_0000000d(arg: PGsARGUNIT): pdword; external;
	function GsU_00000010(arg: PGsARGUNIT): pdword; external;
	function GsU_00000020(arg: PGsARGUNIT): pdword; external;
	function GsU_00000011(arg: PGsARGUNIT): pdword; external;
	function GsU_00000014(arg: PGsARGUNIT): pdword; external;
	function GsU_00000015(arg: PGsARGUNIT): pdword; external;
	function GsU_00020008(arg: PGsARGUNIT): pdword; external;
	function GsU_00020009(arg: PGsARGUNIT): pdword; external;
	function GsU_0002000c(arg: PGsARGUNIT): pdword; external;
	function GsU_0002000d(arg: PGsARGUNIT): pdword; external;
	function GsU_00020010(arg: PGsARGUNIT): pdword; external;
	function GsU_00020011(arg: PGsARGUNIT): pdword; external;
	function GsU_00020014(arg: PGsARGUNIT): pdword; external;
	function GsU_00020015(arg: PGsARGUNIT): pdword; external;
	function GsU_0000000a(arg: PGsARGUNIT): pdword; external;
	function GsU_0000000b(arg: PGsARGUNIT): pdword; external;
	function GsU_0000000e(arg: PGsARGUNIT): pdword; external;
	function GsU_0000000f(arg: PGsARGUNIT): pdword; external;
	function GsU_00000012(arg: PGsARGUNIT): pdword; external;
	function GsU_00000013(arg: PGsARGUNIT): pdword; external;
	function GsU_00000016(arg: PGsARGUNIT): pdword; external;
	function GsU_00000017(arg: PGsARGUNIT): pdword; external;
	function GsU_00030008(arg: PGsARGUNIT): pdword; external;
	function GsU_00030009(arg: PGsARGUNIT): pdword; external;
	function GsU_0003000c(arg: PGsARGUNIT): pdword; external;
	function GsU_0003000d(arg: PGsARGUNIT): pdword; external;
	function GsU_00030010(arg: PGsARGUNIT): pdword; external;
	function GsU_00030011(arg: PGsARGUNIT): pdword; external;
	function GsU_00030014(arg: PGsARGUNIT): pdword; external;
	function GsU_00030015(arg: PGsARGUNIT): pdword; external;
	function GsU_00040048(arg: PGsARGUNIT): pdword; external;
	function GsU_00040049(arg: PGsARGUNIT): pdword; external;
	function GsU_0004004c(arg: PGsARGUNIT): pdword; external;
	function GsU_0004004d(arg: PGsARGUNIT): pdword; external;
	function GsU_00040050(arg: PGsARGUNIT): pdword; external;
	function GsU_00040051(arg: PGsARGUNIT): pdword; external;
	function GsU_00040054(arg: PGsARGUNIT): pdword; external;
	function GsU_00040055(arg: PGsARGUNIT): pdword; external;
	function GsU_00010008(arg: PGsARGUNIT): pdword; external;
	function GsU_00010009(arg: PGsARGUNIT): pdword; external;
	function GsU_0001000c(arg: PGsARGUNIT): pdword; external;
	function GsU_0001000d(arg: PGsARGUNIT): pdword; external;
	function GsU_00010010(arg: PGsARGUNIT): pdword; external;
	function GsU_00010011(arg: PGsARGUNIT): pdword; external;
	function GsU_00010014(arg: PGsARGUNIT): pdword; external;
	function GsU_00010015(arg: PGsARGUNIT): pdword; external;
	function GsU_00050048(arg: PGsARGUNIT): pdword; external;
	function GsU_00050049(arg: PGsARGUNIT): pdword; external;
	function GsU_0005004c(arg: PGsARGUNIT): pdword; external;
	function GsU_0005004d(arg: PGsARGUNIT): pdword; external;
	function GsU_00050050(arg: PGsARGUNIT): pdword; external;
	function GsU_00050051(arg: PGsARGUNIT): pdword; external;
	function GsU_00050054(arg: PGsARGUNIT): pdword; external;
	function GsU_00050055(arg: PGsARGUNIT): pdword; external;
	function GsU_00040058(arg: PGsARGUNIT): pdword; external;
	function GsU_00040059(arg: PGsARGUNIT): pdword; external;
	function GsU_0004005c(arg: PGsARGUNIT): pdword; external;
	function GsU_0004005d(arg: PGsARGUNIT): pdword; external;
	function GsU_01000000(arg: PGsARGUNIT): pdword; external;
	function GsU_0100000c(arg: PGsARGUNIT): pdword; external;
	function GsU_0100000d(arg: PGsARGUNIT): pdword; external;
	function GsU_01000014(arg: PGsARGUNIT): pdword; external;
	function GsU_01000015(arg: PGsARGUNIT): pdword; external;
	function GsU_0102000c(arg: PGsARGUNIT): pdword; external;
	function GsU_0102000d(arg: PGsARGUNIT): pdword; external;
	function GsU_01020014(arg: PGsARGUNIT): pdword; external;
	function GsU_01020015(arg: PGsARGUNIT): pdword; external;
	function GsU_01040048(arg: PGsARGUNIT): pdword; external;
	function GsU_01040049(arg: PGsARGUNIT): pdword; external;
	function GsU_0104004c(arg: PGsARGUNIT): pdword; external;
	function GsU_0104004d(arg: PGsARGUNIT): pdword; external;
	function GsU_01040050(arg: PGsARGUNIT): pdword; external;
	function GsU_01040051(arg: PGsARGUNIT): pdword; external;
	function GsU_01040054(arg: PGsARGUNIT): pdword; external;
	function GsU_01040055(arg: PGsARGUNIT): pdword; external;
	function GsU_00000018(arg: PGsARGUNIT): pdword; external;
	function GsU_00000019(arg: PGsARGUNIT): pdword; external;
	function GsU_0000001c(arg: PGsARGUNIT): pdword; external;
	function GsU_0000001d(arg: PGsARGUNIT): pdword; external;
	function GsU_00000209(arg: PGsARGUNIT): pdword; external;
	function GsU_0000020d(arg: PGsARGUNIT): pdword; external;
	function GsU_00000211(arg: PGsARGUNIT): pdword; external;
	function GsU_00000215(arg: PGsARGUNIT): pdword; external;
	function GsU_02000000(arg: PGsARGUNIT): pdword; external;
	function GsU_02000001(arg: PGsARGUNIT): pdword; external;
	function GsU_00040148(arg: PGsARGUNIT): pdword; external;
	function GsU_00040149(arg: PGsARGUNIT): pdword; external;
	function GsU_0004014c(arg: PGsARGUNIT): pdword; external;
	function GsU_0004014d(arg: PGsARGUNIT): pdword; external;
	function GsU_00040150(arg: PGsARGUNIT): pdword; external;
	function GsU_00040151(arg: PGsARGUNIT): pdword; external;
	function GsU_00040154(arg: PGsARGUNIT): pdword; external;
	function GsU_00040155(arg: PGsARGUNIT): pdword; external;
	function GsU_00200008(arg: PGsARGUNIT): pdword; external;
	function GsU_00200009(arg: PGsARGUNIT): pdword; external;
	function GsU_0020000c(arg: PGsARGUNIT): pdword; external;
	function GsU_0020000d(arg: PGsARGUNIT): pdword; external;
	function GsU_00200010(arg: PGsARGUNIT): pdword; external;
	function GsU_00200011(arg: PGsARGUNIT): pdword; external;
	function GsU_00200014(arg: PGsARGUNIT): pdword; external;
	function GsU_00200015(arg: PGsARGUNIT): pdword; external;
	function GsU_0120000c(arg: PGsARGUNIT): pdword; external;
	function GsU_0120000d(arg: PGsARGUNIT): pdword; external;
	function GsU_01200014(arg: PGsARGUNIT): pdword; external;
	function GsU_01200015(arg: PGsARGUNIT): pdword; external;
	function GsU_00240048(arg: PGsARGUNIT): pdword; external;
	function GsU_00240049(arg: PGsARGUNIT): pdword; external;
	function GsU_0024004c(arg: PGsARGUNIT): pdword; external;
	function GsU_0024004d(arg: PGsARGUNIT): pdword; external;
	function GsU_00240050(arg: PGsARGUNIT): pdword; external;
	function GsU_00240051(arg: PGsARGUNIT): pdword; external;
	function GsU_00240054(arg: PGsARGUNIT): pdword; external;
	function GsU_00240055(arg: PGsARGUNIT): pdword; external;
	function GsU_01240048(arg: PGsARGUNIT): pdword; external;
	function GsU_01240049(arg: PGsARGUNIT): pdword; external;
	function GsU_0124004c(arg: PGsARGUNIT): pdword; external;
	function GsU_0124004d(arg: PGsARGUNIT): pdword; external;
	function GsU_01240050(arg: PGsARGUNIT): pdword; external;
	function GsU_01240051(arg: PGsARGUNIT): pdword; external;
	function GsU_01240054(arg: PGsARGUNIT): pdword; external;
	function GsU_01240055(arg: PGsARGUNIT): pdword; external;
	function GsU_00080008(arg: PGsARGUNIT): pdword; external;
	function GsU_00080009(arg: PGsARGUNIT): pdword; external;
	function GsU_0008000c(arg: PGsARGUNIT): pdword; external;
	function GsU_0008000d(arg: PGsARGUNIT): pdword; external;
	function GsU_00080010(arg: PGsARGUNIT): pdword; external;
	function GsU_00080011(arg: PGsARGUNIT): pdword; external;
	function GsU_00080014(arg: PGsARGUNIT): pdword; external;
	function GsU_00080015(arg: PGsARGUNIT): pdword; external;
	function GsU_000a0008(arg: PGsARGUNIT): pdword; external;
	function GsU_000a0009(arg: PGsARGUNIT): pdword; external;
	function GsU_000a000c(arg: PGsARGUNIT): pdword; external;
	function GsU_000a000d(arg: PGsARGUNIT): pdword; external;
	function GsU_000a0010(arg: PGsARGUNIT): pdword; external;
	function GsU_000a0011(arg: PGsARGUNIT): pdword; external;
	function GsU_000a0014(arg: PGsARGUNIT): pdword; external;
	function GsU_000a0015(arg: PGsARGUNIT): pdword; external;
	function GsU_000c0048(arg: PGsARGUNIT): pdword; external;
	function GsU_000c0049(arg: PGsARGUNIT): pdword; external;
	function GsU_000c004c(arg: PGsARGUNIT): pdword; external;
	function GsU_000c004d(arg: PGsARGUNIT): pdword; external;
	function GsU_000c0050(arg: PGsARGUNIT): pdword; external;
	function GsU_000c0051(arg: PGsARGUNIT): pdword; external;
	function GsU_000c0054(arg: PGsARGUNIT): pdword; external;
	function GsU_000c0055(arg: PGsARGUNIT): pdword; external;
	function GsU_0100020d(arg: PGsARGUNIT): pdword; external;
	function GsU_01000215(arg: PGsARGUNIT): pdword; external;

	function GsU_00100008(arg: PGsARGUNIT): pdword; external;
	function GsU_00100009(arg: PGsARGUNIT): pdword; external;
	function GsU_0010000a(arg: PGsARGUNIT): pdword; external;
	function GsU_0010000b(arg: PGsARGUNIT): pdword; external;
	function GsU_0010000c(arg: PGsARGUNIT): pdword; external;
	function GsU_0010000d(arg: PGsARGUNIT): pdword; external;
	function GsU_0010000e(arg: PGsARGUNIT): pdword; external;
	function GsU_0010000f(arg: PGsARGUNIT): pdword; external;
	function GsU_00100010(arg: PGsARGUNIT): pdword; external;
	function GsU_00100011(arg: PGsARGUNIT): pdword; external;
	function GsU_00100012(arg: PGsARGUNIT): pdword; external;
	function GsU_00100013(arg: PGsARGUNIT): pdword; external;
	function GsU_00100014(arg: PGsARGUNIT): pdword; external;
	function GsU_00100015(arg: PGsARGUNIT): pdword; external;
	function GsU_00100016(arg: PGsARGUNIT): pdword; external;
	function GsU_00100017(arg: PGsARGUNIT): pdword; external;
	function GsU_00300008(arg: PGsARGUNIT): pdword; external;
	function GsU_00300009(arg: PGsARGUNIT): pdword; external;
	function GsU_0030000c(arg: PGsARGUNIT): pdword; external;
	function GsU_0030000d(arg: PGsARGUNIT): pdword; external;
	function GsU_00300010(arg: PGsARGUNIT): pdword; external;
	function GsU_00300011(arg: PGsARGUNIT): pdword; external;
	function GsU_00300014(arg: PGsARGUNIT): pdword; external;
	function GsU_00300015(arg: PGsARGUNIT): pdword; external;
	function GsU_00140048(arg: PGsARGUNIT): pdword; external;
	function GsU_00140049(arg: PGsARGUNIT): pdword; external;
	function GsU_0014004c(arg: PGsARGUNIT): pdword; external;
	function GsU_0014004d(arg: PGsARGUNIT): pdword; external;
	function GsU_00140050(arg: PGsARGUNIT): pdword; external;
	function GsU_00140051(arg: PGsARGUNIT): pdword; external;
	function GsU_00140054(arg: PGsARGUNIT): pdword; external;
	function GsU_00140055(arg: PGsARGUNIT): pdword; external;
	function GsU_00340048(arg: PGsARGUNIT): pdword; external;
	function GsU_00340049(arg: PGsARGUNIT): pdword; external;
	function GsU_0034004c(arg: PGsARGUNIT): pdword; external;
	function GsU_0034004d(arg: PGsARGUNIT): pdword; external;
	function GsU_00340050(arg: PGsARGUNIT): pdword; external;
	function GsU_00340051(arg: PGsARGUNIT): pdword; external;
	function GsU_00340054(arg: PGsARGUNIT): pdword; external;
	function GsU_00340055(arg: PGsARGUNIT): pdword; external;
	function GsU_01140048(arg: PGsARGUNIT): pdword; external;
	function GsU_01140049(arg: PGsARGUNIT): pdword; external;
	function GsU_0114004c(arg: PGsARGUNIT): pdword; external;
	function GsU_0114004d(arg: PGsARGUNIT): pdword; external;
	function GsU_01140050(arg: PGsARGUNIT): pdword; external;
	function GsU_01140051(arg: PGsARGUNIT): pdword; external;
	function GsU_01140054(arg: PGsARGUNIT): pdword; external;
	function GsU_01140055(arg: PGsARGUNIT): pdword; external;
	function GsU_01340048(arg: PGsARGUNIT): pdword; external;
	function GsU_01340049(arg: PGsARGUNIT): pdword; external;
	function GsU_0134004c(arg: PGsARGUNIT): pdword; external;
	function GsU_0134004d(arg: PGsARGUNIT): pdword; external;
	function GsU_01340050(arg: PGsARGUNIT): pdword; external;
	function GsU_01340051(arg: PGsARGUNIT): pdword; external;
	function GsU_01340054(arg: PGsARGUNIT): pdword; external;
	function GsU_01340055(arg: PGsARGUNIT): pdword; external;

	function GsU_0020000a(arg: PGsARGUNIT): pdword; external;
	function GsU_0020000b(arg: PGsARGUNIT): pdword; external;
	function GsU_0020000e(arg: PGsARGUNIT): pdword; external;
	function GsU_0020000f(arg: PGsARGUNIT): pdword; external;
	function GsU_00200012(arg: PGsARGUNIT): pdword; external;
	function GsU_00200013(arg: PGsARGUNIT): pdword; external;
	function GsU_00200016(arg: PGsARGUNIT): pdword; external;
	function GsU_00200017(arg: PGsARGUNIT): pdword; external;
	function GsU_0030000a(arg: PGsARGUNIT): pdword; external;
	function GsU_0030000b(arg: PGsARGUNIT): pdword; external;
	function GsU_0030000e(arg: PGsARGUNIT): pdword; external;
	function GsU_0030000f(arg: PGsARGUNIT): pdword; external;
	function GsU_00300012(arg: PGsARGUNIT): pdword; external;
	function GsU_00300013(arg: PGsARGUNIT): pdword; external;
	function GsU_00300016(arg: PGsARGUNIT): pdword; external;
	function GsU_00300017(arg: PGsARGUNIT): pdword; external;

	function GsU_0100000e(arg: PGsARGUNIT): pdword; external;
	function GsU_0100000f(arg: PGsARGUNIT): pdword; external;
	function GsU_01000016(arg: PGsARGUNIT): pdword; external;
	function GsU_01000017(arg: PGsARGUNIT): pdword; external;
	function GsU_0120000e(arg: PGsARGUNIT): pdword; external;
	function GsU_0120000f(arg: PGsARGUNIT): pdword; external;
	function GsU_01200016(arg: PGsARGUNIT): pdword; external;
	function GsU_01200017(arg: PGsARGUNIT): pdword; external;

	function GsU_0002000a(arg: PGsARGUNIT): pdword; external;
	function GsU_0002000b(arg: PGsARGUNIT): pdword; external;
	function GsU_0002000e(arg: PGsARGUNIT): pdword; external;
	function GsU_0002000f(arg: PGsARGUNIT): pdword; external;
	function GsU_00020012(arg: PGsARGUNIT): pdword; external;
	function GsU_00020013(arg: PGsARGUNIT): pdword; external;
	function GsU_00020016(arg: PGsARGUNIT): pdword; external;
	function GsU_00020017(arg: PGsARGUNIT): pdword; external;
	function GsU_0102000e(arg: PGsARGUNIT): pdword; external;
	function GsU_0102000f(arg: PGsARGUNIT): pdword; external;
	function GsU_01020016(arg: PGsARGUNIT): pdword; external;
	function GsU_01020017(arg: PGsARGUNIT): pdword; external;

	// tiled texture
	function GsU_0000020b(arg: PGsARGUNIT): pdword; external;
	function GsU_0000020f(arg: PGsARGUNIT): pdword; external;
	function GsU_00000213(arg: PGsARGUNIT): pdword; external;
	function GsU_00000217(arg: PGsARGUNIT): pdword; external;
	function GsU_00020209(arg: PGsARGUNIT): pdword; external;
	function GsU_0002020b(arg: PGsARGUNIT): pdword; external;
	function GsU_0002020d(arg: PGsARGUNIT): pdword; external;
	function GsU_0002020f(arg: PGsARGUNIT): pdword; external;
	function GsU_00020211(arg: PGsARGUNIT): pdword; external;
	function GsU_00020213(arg: PGsARGUNIT): pdword; external;
	function GsU_00020215(arg: PGsARGUNIT): pdword; external;
	function GsU_00020217(arg: PGsARGUNIT): pdword; external;
	function GsU_00040249(arg: PGsARGUNIT): pdword; external;
	function GsU_0004024d(arg: PGsARGUNIT): pdword; external;
	function GsU_00040251(arg: PGsARGUNIT): pdword; external;
	function GsU_00040255(arg: PGsARGUNIT): pdword; external;
	function GsU_00100209(arg: PGsARGUNIT): pdword; external;
	function GsU_0010020b(arg: PGsARGUNIT): pdword; external;
	function GsU_0010020d(arg: PGsARGUNIT): pdword; external;
	function GsU_0010020f(arg: PGsARGUNIT): pdword; external;
	function GsU_00100211(arg: PGsARGUNIT): pdword; external;
	function GsU_00100213(arg: PGsARGUNIT): pdword; external;
	function GsU_00100215(arg: PGsARGUNIT): pdword; external;
	function GsU_00100217(arg: PGsARGUNIT): pdword; external;
	function GsU_00200209(arg: PGsARGUNIT): pdword; external;
	function GsU_0020020d(arg: PGsARGUNIT): pdword; external;
	function GsU_00200211(arg: PGsARGUNIT): pdword; external;
	function GsU_00200215(arg: PGsARGUNIT): pdword; external;
	function GsU_00240249(arg: PGsARGUNIT): pdword; external;
	function GsU_0024024d(arg: PGsARGUNIT): pdword; external;
	function GsU_00240251(arg: PGsARGUNIT): pdword; external;
	function GsU_00240255(arg: PGsARGUNIT): pdword; external;
	function GsU_00300209(arg: PGsARGUNIT): pdword; external;
	function GsU_0030020d(arg: PGsARGUNIT): pdword; external;
	function GsU_00300211(arg: PGsARGUNIT): pdword; external;
	function GsU_00300215(arg: PGsARGUNIT): pdword; external;
	function GsU_0100020f(arg: PGsARGUNIT): pdword; external;
	function GsU_01000217(arg: PGsARGUNIT): pdword; external;
	function GsU_0102020d(arg: PGsARGUNIT): pdword; external;
	function GsU_0102020f(arg: PGsARGUNIT): pdword; external;
	function GsU_01020215(arg: PGsARGUNIT): pdword; external;
	function GsU_01020217(arg: PGsARGUNIT): pdword; external;
	function GsU_01040249(arg: PGsARGUNIT): pdword; external;
	function GsU_0104024d(arg: PGsARGUNIT): pdword; external;
	function GsU_01040251(arg: PGsARGUNIT): pdword; external;
	function GsU_01040255(arg: PGsARGUNIT): pdword; external;
	function GsU_0120020d(arg: PGsARGUNIT): pdword; external;
	function GsU_01200215(arg: PGsARGUNIT): pdword; external;
	function GsU_01240249(arg: PGsARGUNIT): pdword; external;
	function GsU_0124024d(arg: PGsARGUNIT): pdword; external;
	function GsU_01240251(arg: PGsARGUNIT): pdword; external;
	function GsU_01240255(arg: PGsARGUNIT): pdword; external;
	function GsU_00140249(arg: PGsARGUNIT): pdword; external;
	function GsU_0014024d(arg: PGsARGUNIT): pdword; external;
	function GsU_00140251(arg: PGsARGUNIT): pdword; external;
	function GsU_00140255(arg: PGsARGUNIT): pdword; external;
	function GsU_00340249(arg: PGsARGUNIT): pdword; external;
	function GsU_0034024d(arg: PGsARGUNIT): pdword; external;
	function GsU_00340251(arg: PGsARGUNIT): pdword; external;
	function GsU_00340255(arg: PGsARGUNIT): pdword; external;
	function GsU_01140249(arg: PGsARGUNIT): pdword; external;
	function GsU_0114024d(arg: PGsARGUNIT): pdword; external;
	function GsU_01140251(arg: PGsARGUNIT): pdword; external;
	function GsU_01140255(arg: PGsARGUNIT): pdword; external;
	function GsU_01340249(arg: PGsARGUNIT): pdword; external;
	function GsU_0134024d(arg: PGsARGUNIT): pdword; external;
	function GsU_01340251(arg: PGsARGUNIT): pdword; external;
	function GsU_01340255(arg: PGsARGUNIT): pdword; external;
	function GsU_0020020b(arg: PGsARGUNIT): pdword; external;
	function GsU_0020020f(arg: PGsARGUNIT): pdword; external;
	function GsU_00200213(arg: PGsARGUNIT): pdword; external;
	function GsU_00200217(arg: PGsARGUNIT): pdword; external;
	function GsU_0030020b(arg: PGsARGUNIT): pdword; external;
	function GsU_0030020f(arg: PGsARGUNIT): pdword; external;
	function GsU_00300213(arg: PGsARGUNIT): pdword; external;
	function GsU_00300217(arg: PGsARGUNIT): pdword; external;
	function GsU_0120020f(arg: PGsARGUNIT): pdword; external;
	function GsU_01200217(arg: PGsARGUNIT): pdword; external;

	function GsU_00000000(arg: PGsARGUNIT): pdword; external;

	function GsU_05000000(arg: PGsARGUNIT): pdword; external;
	function GsU_05000001(arg: PGsARGUNIT): pdword; external;

	// camera
	function GsU_07000100(arg: PGsARGUNIT): pdword; external;
	function GsU_07010100(arg: PGsARGUNIT): pdword; external;
	function GsU_07020100(arg: PGsARGUNIT): pdword; external;
	function GsU_07030100(arg: PGsARGUNIT): pdword; external;

	// light
	function GsU_07000200(arg: PGsARGUNIT): pdword; external;
	function GsU_07010200(arg: PGsARGUNIT): pdword; external;
	function GsU_07020200(arg: PGsARGUNIT): pdword; external;
	function GsU_07030200(arg: PGsARGUNIT): pdword; external;

	// update driver
	function GsU_03000000(sp: PGsARGUNIT_ANIM): pdword; external;

	// interpolation driver
	function GsU_03000001(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000002(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000003(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000009(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300000a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300000b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000010(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000011(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000012(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000013(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000019(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300001a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300001b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000020(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000021(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000022(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000023(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000029(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300002a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300002b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000030(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000031(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000032(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000033(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000039(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300003a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300003b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000100(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000111(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000112(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000119(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300011a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000901(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000902(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000909(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300090a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000910(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000911(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000912(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03000919(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300091a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001010(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001011(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001012(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001013(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001019(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300101a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300101b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001020(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001021(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001022(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001023(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001029(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300102a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300102b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001030(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001031(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001032(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001033(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001039(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300103a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300103b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001111(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001112(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001119(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300111a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001910(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001911(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001912(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03001919(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300191a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002010(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002011(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002012(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002013(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002019(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300201a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300201b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002020(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002021(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002022(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002023(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002029(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300202a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300202b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002030(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002031(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002032(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002033(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002039(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300203a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300203b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002111(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002112(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002119(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300211a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002910(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002911(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002912(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03002919(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300291a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003010(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003011(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003012(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003013(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003019(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300301a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300301b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003020(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003021(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003022(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003023(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003029(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300302a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300302b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003030(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003031(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003032(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003033(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003039(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300303a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300303b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003111(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003112(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003119(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300311a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003910(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003911(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003912(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03003919(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300391a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004010(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004011(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004012(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004013(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004019(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300401a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300401b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004020(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004021(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004022(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004023(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004029(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300402a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300402b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004030(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004031(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004032(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004033(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004039(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300403a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300403b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004111(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004112(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004119(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300411a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004910(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004911(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004912(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03004919(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300491a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005010(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005011(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005012(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005013(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005019(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300501a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300501b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005020(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005021(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005022(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005023(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005029(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300502a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300502b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005030(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005031(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005032(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005033(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005039(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300503a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300503b(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005111(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005112(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005119(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300511a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005910(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005911(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005912(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03005919(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_0300591a(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010110(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010111(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010112(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010121(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010122(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010141(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010142(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010171(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010172(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010182(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010210(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010211(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010212(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010221(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010222(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010241(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010242(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010271(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010272(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010310(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010311(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010312(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010321(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010322(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010341(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010342(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010371(sp: PGsARGUNIT_ANIM): longint; external;
	function GsU_03010372(sp: PGsARGUNIT_ANIM): longint; external;

	// envmap driver
	function GsU_06000100(arg: PGsARGUNIT): longint; external;
	function GsU_0600100c(arg: PGsARGUNIT): longint; external;
	function GsU_06001014(arg: PGsARGUNIT): longint; external;
	function GsU_0600110c(arg: PGsARGUNIT): longint; external;
	function GsU_06001114(arg: PGsARGUNIT): longint; external;
	function GsU_0600200c(arg: PGsARGUNIT): longint; external;
	function GsU_06002014(arg: PGsARGUNIT): longint; external;
	function GsU_0600300c(arg: PGsARGUNIT): longint; external;
	function GsU_06003014(arg: PGsARGUNIT): longint; external;
	function GsU_0600400c(arg: PGsARGUNIT): longint; external;
	function GsU_06004014(arg: PGsARGUNIT): longint; external;
	function GsU_0600500c(arg: PGsARGUNIT): longint; external;
	function GsU_06005014(arg: PGsARGUNIT): longint; external;

	// MIMe driver
	function GsU_04010020(arg: PGsARGUNIT): longint; external;
	function GsU_04010021(arg: PGsARGUNIT): longint; external;
	function GsU_04010028(arg: PGsARGUNIT): longint; external;
	function GsU_04010029(arg: PGsARGUNIT): longint; external;
	function GsU_04010010(arg: PGsARGUNIT): longint; external;
	function GsU_04010018(arg: PGsARGUNIT): longint; external;
	function GsU_04010011(arg: PGsARGUNIT): longint; external;
	function GsU_04010019(arg: PGsARGUNIT): longint; external;


	function GsMapCoordUnit(base, p: pdword): PGsCOORDUNIT; external;
	function GsGetHeadpUnit: pdword; external;
	function GsScanUnit(p: pdword; ut: PGsTYPEUNIT; ot: PGsOT; scratch: pdword): longint; external;
	procedure GsMapUnit(p: pdword); external;
	procedure GsSortUnit(objp: PGsUNIT; otp: PGsOT; scrathc: pdword); external;
	procedure GsGetLwUnit(coord: PGsCOORDUNIT; m: PMATRIX); external;
	procedure GsGetLsUnit(coord: PGsCOORDUNIT; m: PMATRIX); external;
	procedure GsGetLwsUnit(coord: PGsCOORDUNIT; lw, ls: PMATRIX); external;
	function GsSetViewUnit(pv: PGsVIEWUNIT): longint; external;
	function GsSetRefViewUnit(pv: PGsRVIEWUNIT): longint; external;
	function GsSetRefViewLUnit(pv: PGsRVIEWUNIT): longint; external;
	function GsScanAnim(p: pdword; ut: PGsTYPEUNIT): pdword; external;
	function GsLinkAnim(seq: PPGsSEQ; p: dword): longint; external;

	// for MIMe
	procedure GsInitRstVtxMIMe(primtop, hp: pdword); external;
	procedure GsInitRstNrmMIMe(primtop, hp: pdword); external;

implementation

begin
end.