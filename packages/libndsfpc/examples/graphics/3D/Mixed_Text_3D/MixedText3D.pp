program MixedText3D;

uses
  ctypes, nds9, math;


var
  rtri: cfloat;				// Angle For The Triangle ( NEW )
  rquad: cfloat;				// Angle For The Quad ( NEW )

	console: PrintConsole;

function fmodf(a,b:cfloat):cfloat;
begin
  fmodf := b*Frac(a/b);
end;

procedure DrawGLScene();
begin
	//ds does this automagically*open>///glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);	// Clear Screen And Depth Buffer

	glLoadIdentity();								// Reset The Current Modelview Matrix
	glTranslatef(-1.5, 0.0,-6.0);					// Move Left 1.5 Units And Into The Screen 6.0
	glRotatef(rtri, 0.0, 1.0, 0.0);					// Rotate The Triangle On The Y axis ( NEW )
	glColor3f(1, 1, 1);								// set the vertex color
	glBegin(GL_TRIANGLES);							// Start Drawing A Triangle
		glColor3f( 1.0, 0.0, 0.0);					// Set Top Point Of Triangle To Red
		glVertex3f( 0.0, 1.0, 0.0);				// First Point Of The Triangle
		glColor3f( 0.0, 1.0, 0.0);					// Set Left Point Of Triangle To Green
		glVertex3f(-1.0,-1.0, 0.0);				// Second Point Of The Triangle
		glColor3f( 0.0, 0.0, 1.0);					// Set Right Point Of Triangle To Blue
		glVertex3f( 1.0,-1.0, 0.0);				// Third Point Of The Triangle
	glEnd();										// Done Drawing The Triangle
	glLoadIdentity();								// Reset The Current Modelview Matrix


	glTranslatef( 1.5, 0.0,-6.0);					// Move Right 1.5 Units And Into The Screen 6.0
	glRotatef(rquad, 1.0, 0.0, 0.0);				// Rotate The Quad On The X axis ( NEW )
	glColor3f( 0.5, 0.5, 1.0);						// Set The Color To Blue One Time Only
	glBegin(GL_QUADS);								// Draw A Quad
		glVertex3f(-1.0, 1.0, 0.0);				// Top Left
		glVertex3f( 1.0, 1.0, 0.0);				// Top Right
		glVertex3f( 1.0,-1.0, 0.0);				// Bottom Right
		glVertex3f(-1.0,-1.0, 0.0);				// Bottom Left
	glEnd();										// Done Drawing The Quad
end;

begin
	// initialize the geometry engine
	glInit();	

	// Setup the Main screen for 3D 
	videoSetMode(MODE_0_3D);
	
	//map some vram to background for printing
	vramSetBankC(VRAM_C_MAIN_BG_0x06000000);
 
	consoleInit(nil, 1, BgType_Text4bpp, BgSize_T_256x256, 31,0, true, true);

	//put bg 0 at a lower priority than the text background
	bgSetPriority(0, 1);
  

 
	// enable antialiasing
	glEnable(GL_ANTIALIAS);
 
	// setup the rear plane
	glClearColor(0,0,0,31); // BG must be opaque for AA to work
	glClearPolyID(63); // BG must have a unique polygon ID for AA to work
	glClearDepth($7FFF);
 
	// Set our viewport to be the same size as the screen
	glViewport(0,0,255,191);
 
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(70, 256.0 / 192.0, 0.1, 100);
 
	//ds specific, several attributes can be set here	
	glPolyFmt(POLY_ALPHA(31) or POLY_CULL_NONE);
 
	// Set the current matrix to be the model matrix
	glMatrixMode(GL_MODELVIEW);
 
	iprintf('      Hello DS World'#10);
	iprintf('     www.devkitpro.org'#10);
	iprintf('   www.drunkencoders.com'#10);

	while true do
	begin
		DrawGLScene();
 
		// flush to screen	
		glFlush(0);
 
		// wait for the screen to refresh
		swiWaitForVBlank();

		printf(#$1b'[15;5H rtri  = %f     '#10, rtri);
		printf(#$1b'[16;5H rquad = %f     '#10, rquad);
		rtri := rtri + 0.9;										// Increase The Rotation Variable For The Triangle ( NEW )
		rquad := rquad - 0.75;									// Decrease The Rotation Variable For The Quad ( NEW )

		rtri  := fmodf( rtri , 360 );
		rquad := fmodf( rquad, 360 );

	end;
 
end.
