program ToonShading;
//NB: This would look better if the object had a bit of texturing too (eyes, nose etc)

{$L build/statue.bin.o}
{$mode objfpc}

uses
  ctypes, nds9;


{$include inc/statue.bin.inc}

var
	prev_pen: array [0..1] of cint = ( $7FFFFFFF, $7FFFFFFF );


procedure get_pen_delta(var dx, dy: cint);
var
	keys: cuint32;
  touchXY: touchPosition;
begin
	keys := keysHeld();

	if( keys and KEY_TOUCH ) <> 0 then
	begin
		touchXY := touchReadXY();

		if ( prev_pen[0] <> $7FFFFFFF ) then
		begin
			dx := (prev_pen[0] - touchXY.rawx);
			dy := (prev_pen[1] - touchXY.rawy);
		end;

		prev_pen[0] := touchXY.rawx;
		prev_pen[1] := touchXY.rawy;
	end else
	begin
		prev_pen[0] := $7FFFFFFF;
    prev_pen[1] := $7FFFFFFF;
		dx := 0;
		dy := 0;
	end;
end;

var
	rotateX: integer = 0;
	rotateY: integer = 0;
	keys: cuint32;
	pen_delta: array [0..1] of cint;

begin
	//set mode 0, enable BG0 and set it to 3D
	videoSetMode(MODE_0_3D);

	// initialize gl
	glInit();
	
	// enable antialiasing
	glEnable(GL_ANTIALIAS);
	
	// setup the rear plane
	glClearColor(0,0,0,31); // BG must be opaque for AA to work
	glClearPolyID(63); // BG must have a unique polygon ID for AA to work
	glClearDepth($7FFF);

	//this should work the same as the normal gl call
	glViewport(0,0,255,191);

	//toon-table entry 0 is for completely unlit pixels, going up to entry 31 for completely lit
	//We block-fill it in two halves, we get cartoony 2-tone lighting
	glSetToonTableRange( 0, 15, RGB15(8,8,8) );
	glSetToonTableRange( 16, 31, RGB15(24,24,24) );
	
	//any floating point gl call is being converted to fixed prior to being implemented
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(70, 256.0 / 192.0, 0.1, 40);
	
	//NB: When toon-shading, the hw ignores lights 2 and 3
	//Also note that the hw uses the RED component of the lit vertex to index the toon-table
	glLight(0, RGB15(16,16,16) , 0,		floattov10(-1.0),		0);
	glLight(1, RGB15(16,16,16),   floattov10(-1.0),	0,		0);
	
	gluLookAt(	0.0, 0.0, -3.0,		//camera possition 
				0.0, 0.0, 0.0,		//look at
				0.0, 1.0, 0.0);		//up
	
	while true do
	begin

		glMatrixMode(GL_MODELVIEW);
		glPushMatrix();
			glRotateXi(rotateX);
			glRotateYi(rotateY);


			glMaterialf(GL_AMBIENT, RGB15(8,8,8));
			glMaterialf(GL_DIFFUSE, RGB15(24,24,24));
			glMaterialf(GL_SPECULAR, RGB15(0,0,0));
			glMaterialf(GL_EMISSION, RGB15(0,0,0));

			glPolyFmt(POLY_ALPHA(31) or POLY_CULL_BACK or POLY_FORMAT_LIGHT0 or POLY_FORMAT_LIGHT1 or POLY_TOON_HIGHLIGHT);


			scanKeys();
			keys := keysHeld();

			if( keys and KEY_UP ) <> 0 then rotateX := rotateX +1;
			if( keys and KEY_DOWN ) <> 0 then rotateX := rotateX -1;
			if( keys and KEY_LEFT ) <> 0 then rotateY := rotateY +1;
			if( keys and KEY_RIGHT ) <> 0 then rotateY := rotateY -1;

			get_pen_delta( pen_delta[0], pen_delta[1] );
			rotateY := rotateY - pen_delta[0];
			rotateX := rotateY - pen_delta[1];


			glCallList(@statue_bin);
			glPopMatrix(1);

		glFlush(0);

		swiWaitForVBlank();
	end;
end.
