(*
  Easy GL2D
  Relminator 2011 
  Richard Eric M. Lope BSN RN
  Http://Rel.Phatcode.Net
  A very small, simple, yet very fast DS 2D rendering lib using the DS' 3D core.
  --
  Translated in Object Pascal by Francesco Lombardi - 2012
  http://itaprogaming.free.fr
*)
program dual_screen;

{$mode objfpc}{$H+}

uses
  nds9, ctypes, gl2d;

const
  HALF_WIDTH  = (SCREEN_WIDTH  div 2);
  HALF_HEIGHT = (SCREEN_HEIGHT div 2);
  BRAD_PI     = (1 shl 14);


// Simple box, triangle, and putpixel demo
procedure simple(frame: cint);
var
  red, green, blue: cint;
  i: integer;
  x, y: integer;
begin
	// set up GL2D for 2d mode
	glBegin2D();

		// Do some funky color cycling
		red   := abs(sinLerp(frame * 220) * 31) shr 12;
		green := abs(sinLerp(frame * 140) * 31) shr 12;
		blue  := abs(sinLerp(frame *  40) * 31) shr 12;
		
		// fill the whole screen with a gradient box
		glBoxFilledGradient( 0, 0, 255, 191,
							 RGB15( red,  green,  blue ),
							 RGB15(  blue, 31 - red,  green ),
							 RGB15( green,  blue, 31 - red ),
							 RGB15(  31 - green, red, blue )
						   );
		
		// draw a black box
		glBoxFilled( 200, 10,
					       250, 180,
					       RGB15(0,0,0)
				       );
		
		// draw a border around the black box
		glBox( 200, 10,
			     250, 180,
			     RGB15(0,31,0)
		     );
	
		// draw a triangle
		glTriangleFilled( 20, 100,
						          200, 30,
						          60, 40,
						          RGB15(31,0,31)
						        );
	
		// draw a gradient triangle
		glTriangleFilledGradient( 20, 100,
								  200, 30,
								  60, 40,
								  RGB15(blue,red,green),
								  RGB15(green,blue, red),
								  RGB15(red,green,blue)
								);

		// translucent mode
		// Poly ID 1
		glPolyFmt(POLY_ALPHA(16) or POLY_CULL_NONE or POLY_ID(1));
		glBoxFilledGradient( 10, 50, 230, 150,
							 RGB15( green,  0,  0 ),
							 RGB15(  0, red,  0 ),
							 RGB15( 31,  0, blue ),
							 RGB15(  0, red, 31 )
						   );

		// translucent mode
		// Poly ID 2
		glPolyFmt(POLY_ALPHA(16) or POLY_CULL_NONE or POLY_ID(2));
		glTriangleFilledGradient( 70, 10,
								  20, 130,
								  230, 180,
								  RGB15(red,green,blue),
								  RGB15(blue,red,green),
								  RGB15(green,blue, red)
								);
		i := 0;
		// restore to normal(solid) rendering
		glPolyFmt(POLY_ALPHA(31) or POLY_CULL_NONE or POLY_ID(3));
		// draw a circle using putpixel
{
    for i := 0 to 128 do
    begin
			x := sar(cosLerp(i * 256) * 80, 12);
			y := sar(sinLerp(i * 256) * 70, 12);
			glPutPixel( HALF_WIDTH  + x, 
                  HALF_HEIGHT + y, 
                  RGB15(red, green, blue) 
                );
    end;
    }

		while i < BRAD_PI * 2 do
		begin
			x := SarLongint(cosLerp(i) * 80, 12);
			y := SarLongint(sinLerp(i) * 70, 12);
			glPutPixel( HALF_WIDTH + x, HALF_HEIGHT + y, RGB15(red, green, blue) );
			inc(i, 256);
		end;

	glEnd2D();
end;


// oldskool lines demo
procedure lines(frame: cint);
var
  red, green, blue: cint;
  i: cint;
  px, py, px2, py2: cint;
begin
	// Do some funky color cycling
	red   := abs(sinLerp(frame * 220) * 31) shr 12 ;
	green := abs(sinLerp(frame * 140) * 31) shr 12 ;
	blue  := abs(sinLerp(frame *  40) * 31) shr 12 ;
	
	// set up GL2D for 2d mode
	glBegin2D();
		// draw a bunch (4096/32) of colored lines
		// using some funky trig equations

		i := frame;
		while i < ((1 shl 12) + frame) do
		begin
			px  := SarLongint(sinLerp(frame * 130) * 130, 12) * cosLerp( (i * 100));
			py  := SarLongint(sinLerp(frame * 280) *  70, 12) * sinLerp( (i * 200));
			px2 := SarLongint(sinLerp(frame * 330) * 100, 12) * cosLerp(((i * 300 + BRAD_PI)));
		 	py2 := SarLongint(sinLerp(frame * 140) *  80, 12) * sinLerp(((i * 400 + BRAD_PI)));
			glLine( HALF_WIDTH + SarLongint(px, 12), HALF_HEIGHT + SarLongint(py, 12),
					    HALF_WIDTH + SarLongint(px2, 12), HALF_HEIGHT + SarLongint(py2, 12),
					    RGB15(red, green, blue)
				    );
			glLine( HALF_WIDTH + SarLongint(py2, 12), HALF_HEIGHT + SarLongint(px, 12),
					    HALF_WIDTH + SarLongint(py, 12), HALF_HEIGHT + SarLongint(px2, 12),
					    RGB15(green, blue, red)
				    );
			inc(i, 32);
		end;


	glEnd2D();
end;


// Some radially displaced pixels
procedure pixels(frame: cint);
var
  radius, red, green, blue: cint;
  i, angle: cint;
  x, y, a2,x2,y2: cint;
begin
	// Elastic radius
	radius := 40 + (abs(sinLerp(frame * 20) * 80) shr 12);
	
	// Do some funky color cycling
	red := abs(sinLerp(frame * 220) * 31) shr 12 ;
	green := abs(sinLerp(frame * 140) * 31) shr 12 ;
	blue := abs(sinLerp(frame * 40) * 31) shr 12 ;

	// speed opf animation
	i := (frame * 140) and 32767;
	
	// duh!
	angle := 0;
	
	// set up GL2D for 2d mode
	glBegin2D();
		// Draw a full revolution of some radially dispalced pixels
		for angle := 0 to 512 do
		begin
			a2 := (angle * 64) + i;
			x := cosLerp(angle * 64 * 2) * radius;
			y := sinLerp(x div 32 + a2) * radius;
			x := cosLerp((y div 64) + (angle * 64)) * (radius + 20);
			y := sinLerp(x div 64 + a2) * radius;
			x2 := -y;
			y2 := x;
			
			glPutPixel( HALF_WIDTH  + SarLongint(x, 12), 
						      HALF_HEIGHT + SarLongint(y, 12), 
						      RGB15(red, green, blue)
					      );
			glPutPixel( HALF_WIDTH  + SarLongint(x2, 12), 
						      HALF_HEIGHT + SarLongint(y2, 12), 
						      RGB15(green, blue, red)
					      );
		end;
	glEnd2D();
end;

//-------------------------------------------------------
// set up a 2D layer construced of bitmap sprites
// this holds the image when rendering to the top screen
//-------------------------------------------------------
procedure initSubSprites();
var
  x, y, id: cint;
begin
	oamInit(oamSub, SpriteMapping_Bmp_2D_256, false);
 
	//set up a 4x3 grid of 64x64 sprites to cover the screen
	for y := 0 to 2 do
	  for x := 0 to 3 do
    begin
		  oamSub.oamMemory[id].attribute[0] := ATTR0_BMP or ATTR0_SQUARE or (64 * y);
		  oamSub.oamMemory[id].attribute[1] := ATTR1_SIZE_64 or (64 * x);
		  oamSub.oamMemory[id].attribute[2] := ATTR2_ALPHA(1) or (8 * 32 * y) or (8 * x);
		  inc(id);
    end;
 
	swiWaitForVBlank();
 
	oamUpdate(oamSub);
end;

var
  frame, demonum, key: cint;

begin	
	
	// Set it to my favorite mode
	videoSetMode(MODE_5_3D);
	videoSetModeSub(MODE_5_2D);
 
	// init oam to capture 3D scene
	initSubSprites();
 
	// sub background holds the top image when 3D directed to bottom
	bgInitSub(3, BgType_Bmp16, BgSize_B16_256x256, 0, 0);
 
	
	// Initialize GL in 3d mode
	glScreen2D();
	
	

	
	// our ever present frame counter	
	frame := 0;
	
	demonum := 0;	// what demo are we currently viewing
	
	
	while true do
	begin
	
	
		// increment frame counter 
		inc(frame);
		
		// get input
		scanKeys();
		key := keysDown();
		
		
		// process input
		if ((key and KEY_DOWN) or (key and KEY_RIGHT) ) <> 0 then
			demonum := (demonum + 1) mod 3;
		
		if ((key and KEY_UP) or (key and KEY_LEFT)) <> 0 then
		begin
			dec(demonum);
			if (demonum < 0) then demonum := 2;
		end;
	
		// wait for capture unit to be ready
		while (REG_DISPCAPCNT^ and DCAP_ENABLE) <> 0 do;

	
		// Alternate rendering every other frame
		// Limits your FPS to 30
		if ((frame and 1) = 0) then
		begin
			lcdMainOnBottom();
			vramSetBankC(VRAM_C_LCD);
			vramSetBankD(VRAM_D_SUB_SPRITE);
			REG_DISPCAPCNT^ := DCAP_BANK(2) or DCAP_ENABLE or DCAP_SIZE(3);
		end else
		begin
			lcdMainOnTop();
			vramSetBankD(VRAM_D_LCD);
			vramSetBankC(VRAM_C_SUB_BG);
			REG_DISPCAPCNT^ := DCAP_BANK(3) or DCAP_ENABLE or DCAP_SIZE(3);
		end;
 
		// figure out what demo should be viewed
		case demonum of
			0: begin			// Alternately draw every other frame
				if ((frame and 1) = 0) then
					pixels( frame )
				else
					lines( frame );
			end;
			1: begin
				if ((frame and 1) = 0) then
					lines( frame )
				else
					pixels( frame );
			end;
			2: begin
				if ((frame and 1) = 0) then
					simple( frame )
				else
					lines( frame );
			end;
		  else begin
				if ((frame and 1) = 0) then
					pixels( frame )
				else
					lines( frame );
		  end;
		end;
		
		glFlush(0);
		swiWaitForVBlank();
		
		
	end;

end.

