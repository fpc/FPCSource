{$MODE OBJFPC}
uses libstd, libcd, libcomb, libds, libetc, libgpu, libgte;


procedure GsInitGraph(x, y, intmode, dith, varmmode: word); stdcall; external;

const
	MODE_NTSC = 0;
	MODE_PAL = 1;



const 
	OTSIZE = 4096;

	vertices : array [0..7] of SVECTOR = (
	  (vx: -128; vy: -128; vz: -128; pad: 0 ),
	  (vx:  128; vy: -128; vz: -128; pad: 0 ),
	  (vx:  128; vy: -128; vz:  128; pad: 0 ),
	  (vx: -128; vy: -128; vz:  128; pad: 0 ),
	  (vx: -128; vy:  128; vz: -128; pad: 0 ),
	  (vx:  128; vy:  128; vz: -128; pad: 0 ),
	  (vx:  128; vy:  128; vz:  128; pad: 0 ),
	  (vx: -128; vy:  128; vz:  128; pad: 0 ));

	faces : array [0..35] of smallint = (
	  0, 3, 2, // top
	  0, 2, 1, // top
	  4, 0, 1, // front
	  4, 1, 5, // front
	  7, 4, 5, // bottom
	  7, 5, 6, // bottom
	  5, 1, 2, // right
	  5, 2, 6, // right
	  2, 3, 7, // back
	  2, 7, 6, // back
	  0, 4, 7, // left
	  0, 7, 3  // left 
	  );

type

	DoubleBuff = packed record
  		draw : DRAWENV;
  		disp : DISPENV;
 	end;

var
	screen : array [0..1] of DoubleBuff;           // Struct to hold the display & draw buffers
	currbuff : byte;            // Holds the current buffer number (0 or 1)

	ot : array[0..1, 0..OTSIZE] of dword;

	i : longint;
	ii : longint;
	otz : longint;
	counter : dword;

	poly : array [0..11] of POLY_G3;


procedure setRGB0(var c: DRAWENV; r, g, b: byte);
begin
	c.r0:=r;
	c.g0:=g;
	c.b0:=b;
end;


procedure ScreenInit(width, height: dword);
begin
		
  	ResetGraph(0);
	InitGeom();

	SetGraphDebug(0);

	
	SetVideoMode(MODE_PAL);
	GsInitGraph(width, height, 0, 0, 0);
	

	SetDefDispEnv(@screen[0].disp, 0, 0, width, height);
	SetDefDispEnv(@screen[1].disp, 0, height, width, height);

	SetDefDrawEnv(@screen[0].draw, 0, height, width, height);
	SetDefDrawEnv(@screen[1].draw, 0, 0, width, height);

	screen[0].disp.screen.x:= 0;
	screen[0].disp.screen.y:= 0;
	screen[1].disp.screen.x:= 0;
	screen[1].disp.screen.y:= 0;

	screen[0].disp.screen.h:= 256;
	screen[0].disp.screen.w:= 0;
	screen[1].disp.screen.h:= 256;
	screen[1].disp.screen.w:= 0;


	screen[0].draw.isbg:= 1;
	screen[1].draw.isbg:= 1;

	// Set the background clear color
	setRGB0(screen[0].draw, 0, 0, 0);
	setRGB0(screen[1].draw, 0, 0, 0);

	
	// Initialize and setup the GTE geometry offsets
	SetGeomOffset(width div 2, height div 2);
	SetGeomScreen(100);
	
	SetDispMask(1);

	// Set the current initial buffer
	currbuff:= 0;
	PutDispEnv(@screen[currbuff].disp);
	PutDrawEnv(@screen[currbuff].draw);

end;


procedure DisplayFrame;
begin
	
	// Set the current display & draw buffers
	PutDispEnv(@screen[currbuff].disp);
	PutDrawEnv(@screen[currbuff].draw);

	DrawOTag(@ot[currbuff, OTSIZE - 1]);

	FntFlush(-1);

	if currbuff = 0 then currbuff:=1 else currbuff:=0;

	// Sync and wait for vertical blank
	DrawSync(0);
	VSync(0);

end;


var
	rotation : SVECTOR;
    translation : VECTOR;
    transform : MATRIX;

    p : pointer;
    flg : pointer;
    nclip : longint;

    ch: pchar;
    d: dword;


begin


	ScreenInit(320, 256);


	FntLoad(960, 256);
	SetDumpFnt(FntOpen(0, 100, 200, 200, 0, 512));

	rotation.vx:= 0;
	rotation.vy:= 0;
	rotation.vz:= 0;

	translation.vx:= 0;
	translation.vy:= 0;
	translation.vz:= 500;

	counter:= 1;

//	srand(1234);
//randomize;


	repeat

		ClearOTagR(@ot[currbuff], OTSIZE);
		


		rotation.vx +=  6;
	  	rotation.vy +=  8;
	  	rotation.vz += 12;

	    RotMatrix(@rotation, @transform);
	    TransMatrix(@transform, @translation);
			
		SetRotMatrix(@transform);
    	SetTransMatrix(@transform);

	    for i:= 0 to 11 do begin
			setPolyG3(@poly[i]);

			  poly[i].r0:= 255; poly[i].g0:=0;   poly[i].b0:= 0;
			  poly[i].r1:= 0;   poly[i].g1:=255; poly[i].b1:= 0;
			  poly[i].r2:= 0;   poly[i].g2:=0;   poly[i].b2:= 255;
	{
			  otz:= 0;
			  otz:= otz + RotTransPers(@vertices[faces[i * 3 + 0]], @poly[i].x0, @p, @flg);
			  otz:= otz + RotTransPers(@vertices[faces[i * 3 + 1]], @poly[i].x1, @p, @flg);
			  otz:= otz + RotTransPers(@vertices[faces[i * 3 + 2]], @poly[i].x2, @p, @flg);
			  otz:= otz div 3;
	}
				nclip:= RotAverageNclip3(@vertices[faces[i * 3 + 0]], @vertices[faces[i * 3 + 1]], @vertices[faces[i * 3 + 2]], @poly[i].x0, @poly[i].x1, @poly[i].x2, @p, @otz, @flg);
				if nclip <= 0 then continue;

				if (otz > 0) and (otz < OTSIZE) then addPrim(@ot[currbuff, otz], @poly[i]);
		end;


		counter:= counter + 1;

		FntPrint('Hello from FPC %d', counter);
		

		DisplayFrame;
		
	until false;

end.	