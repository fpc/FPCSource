program EnvMapping;
{$L build/teapot.bin.o}
{$L build/cafe.bin.o}

{$mode objfpc}

uses
  ctypes, nds9;

{$include inc/teapot.bin.inc}
{$include inc/cafe.bin.inc}


var
	prev_pen: array [0..1] of cint = ($7FFFFFFF, $7FFFFFFF);
	
procedure get_pen_delta(dx, dy: pcint);
var
  keys: u32;
  touchXY: touchPosition;
begin
  keys := keysHeld();

	if (keys and KEY_TOUCH) <> 0 then
	begin
		touchRead(touchXY);

		if (prev_pen[0] <> $7FFFFFFF) then
		begin
			dx^ := (prev_pen[0] - touchXY.rawx);
			dy^ := (prev_pen[1] - touchXY.rawy);
		end;

		prev_pen[0] := touchXY.rawx;
		prev_pen[1] := touchXY.rawy;
	end else
	begin
	  prev_pen[0] := $7FFFFFFF;
		prev_pen[1] := $7FFFFFFF;
		dx^ := 0;
		dy^ := 0;
	end;
end;


var
	rotateX: integer = 0;
	rotateY: integer = 0;
	tex_scale: GLvector;  
  keys: u32;
  cafe_texid: cint;
	pen_delta: array [0..1] of cint;

begin
	//set mode 0, enable BG0 and set it to 3D
	videoSetMode(MODE_0_3D);

	// intialize gl
	glInit();
	
	// enable antialiasing
	glEnable(GL_ANTIALIAS);
	
	// setup the rear plane
	glClearColor(0,0,0,31); // BG must be opaque for AA to work
	glClearPolyID(63); // BG must have a unique polygon ID for AA to work
	glClearDepth($7FFF);

	//this should work the same as the normal gl call
	glViewport(0,0,255,191);

	vramSetBankA(VRAM_A_TEXTURE);
	glEnable(GL_TEXTURE_2D);
	
	glGenTextures(1, @cafe_texid);
	glBindTexture(0, cafe_texid);
	glTexImage2D(0, 0, GL_RGB, TEXTURE_SIZE_128 , TEXTURE_SIZE_128, 0, GL_TEXTURE_WRAP_S or GL_TEXTURE_WRAP_T or TEXGEN_NORMAL, pcuint8(@cafe_bin));

	
	//any floating point gl call is being converted to fixed prior to being implemented
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(70, 256.0 / 192.0, 0.1, 40);
	
	
	while true do
	begin
		//TEXGEN_NORMAL helpfully pops our normals into this matrix and uses the result as texcoords
		glMatrixMode(GL_TEXTURE);
		glLoadIdentity();
		
		tex_scale.x := (64 shl 16);
		tex_scale.y := (-64 shl 16); 
		tex_scale.z := (1 shl 16);
		glScalev( @tex_scale );		//scale normals up from (-1,1) range into texcoords
		glRotateXi(rotateX);		//rotate texture-matrix to match the camera
		glRotateYi(rotateY);

		glMatrixMode(GL_POSITION);
		glLoadIdentity();
		glTranslatef32(0, 0, floattof32(-3));
		glRotateXi(rotateX);
		glRotateYi(rotateY);

		glMaterialf(GL_EMISSION, RGB15(31,31,31));

		glPolyFmt(POLY_ALPHA(31) or POLY_CULL_BACK );

		scanKeys();
		keys := keysHeld();

		if ( keys and KEY_UP ) <> 0 then rotateX := rotateX + (3 shl 3);
		if ( keys and KEY_DOWN ) <> 0 then rotateX := rotateX - (3 shl 3);
		if ( keys and KEY_LEFT ) <> 0 then rotateY := rotateY + (3 shl 3);
		if ( keys and KEY_RIGHT ) <> 0 then rotateY := rotateY - (3 shl 3);
		
		
		get_pen_delta( @pen_delta[0], @pen_delta[1] );
		rotateY := rotateY - pen_delta[0];
		rotateX := rotateX - pen_delta[1];


		glBindTexture( 0, cafe_texid );
		glCallList(@teapot_bin);

		glFlush(0);
  end;
end.
