program picking;
{$L build/cone.bin.o}
{$L build/cylinder.bin.o}
{$L build/sphere.bin.o}

uses
  ctypes, nds9;
  
{$include inc/cone.bin.inc}
{$include inc/cylinder.bin.inc}
{$include inc/sphere.bin.inc}

type
  TClickable = ( clNothing, clCone, clCylinder, clSphere);

var
  clicked: TClickable; // what is being clicked
  closeW: integer; // closest distace to camera
  polyCount: integer; // keeps track of the number of polygons drawn

// run before starting to draw an object while picking
procedure startCheck();
begin
	while PosTestBusy() do; // wait for the position test to finish
	while GFX_BUSY do; // wait for all the polygons from the last object to be drawn
	PosTest_Asynch(0,0,0); // start a position test at the current translated position
	polyCount := GFX_POLYGON_RAM_USAGE^; // save the polygon count
end;

// run afer drawing an object while picking
procedure endCheck(obj: TClickable);
begin
	while GFX_BUSY do; // wait for all the polygons to get drawn
	while PosTestBusy() do; // wait for the position test to finish
	if (GFX_POLYGON_RAM_USAGE^ > polyCount) then // if a polygon was drawn
	begin
		if PosTestWresult() <= closeW then
		begin
			// this is currently the closest object under the cursor!
			closeW := PosTestWresult();
			clicked := obj;
		end;
	end;
end;


var
	rotateX: cuint32 = 0;
	rotateY: cuint32 = 0;
  touchXY: touchPosition;
  viewport: array [0..3] of cint32 = (0,0,255,191); // used later for gluPickMatrix()
	keys: u16;

begin
	// initialize gl
	glInit();

	//set mode 0, enable BG0 and set it to 3D
	videoSetMode(MODE_0_3D);
	
	lcdMainOnBottom(); // we are going to be touching the 3D display
	
	// enable edge outlining, this will be used to show which object is selected
	glEnable(GL_OUTLINE);
	
	//set the first outline color to white
	glSetOutlineColor(0,RGB15(31,31,31));

	// setup the rear plane
	glClearColor(0,0,0,0); // set BG to black and clear
	glClearPolyID(0); // the BG and polygons will have the same ID unless a polygon is highlighted
	glClearDepth($7FFF);
	
	// setup the camera
	gluLookAt( 0.0, 0.0, 1.0,		//camera possition
				     0.0, 0.0, 0.0,		//look at
				     0.0, 1.0, 0.0);	//up
	
	glLight(0, RGB15(31,31,31) , 0, floattov10(-1.0), 0); // setup the light
	
	while true do
	begin
		// handle key input
		scanKeys();
		keys := keysHeld();
		if ((keys and KEY_UP)) = 0 then rotateX := rotateX +3;
		if((keys and KEY_DOWN)) = 0 then rotateX := rotateX -3;
		if((keys and KEY_LEFT)) = 0 then rotateY := rotateY +3;
		if((keys and KEY_RIGHT)) = 0 then rotateY := rotateY -3;

		// get touchscreen position
		touchXY := touchReadXY();

		glViewPort(0,0,255,191); // set the viewport to fullscreen

		// setup the projection matrix for regular drawing
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		gluPerspective(60, 256.0 / 192.0, 0.1, 20); 
		
		glMatrixMode(GL_MODELVIEW); // use the modelview matrix while drawing
		
		glPushMatrix(); // save the state of the current matrix(the modelview matrix)
			glTranslate3f32(0,0,floattof32(-6));
			glRotateXi(rotateX); // add X rotation to the modelview matrix
			glRotateYi(rotateY); // add Y rotation to the modelview matrix
			
			glPushMatrix(); // save the state of the modelview matrix while making the first pass
				// draw the scene for displaying
				
				glTranslate3f32(floattof32(2.9),floattof32(0),floattof32(0)); // translate the modelview matrix to the drawing location
				if (clicked = clCone) then
					glPolyFmt(POLY_ALPHA(31) or POLY_CULL_BACK or POLY_FORMAT_LIGHT0 or POLY_ID(1)) // set a poly ID for outlining
				else
					glPolyFmt(POLY_ALPHA(31) or POLY_CULL_BACK or POLY_FORMAT_LIGHT0 or POLY_ID(0)); // set a poly ID for no outlining (same as BG)

				glCallList((@cone_bin)); // draw a green cone from a predefined packed command list

				
				glTranslate3f32(floattof32(-3),floattof32(1.8),floattof32(2)); // translate the modelview matrix to the drawing location
				if (clicked = clCylinder) then
					glPolyFmt(POLY_ALPHA(31) or POLY_CULL_BACK or POLY_FORMAT_LIGHT0 or POLY_ID(1)) // set a poly ID for outlining
				else
					glPolyFmt(POLY_ALPHA(31) or POLY_CULL_BACK or POLY_FORMAT_LIGHT0 or POLY_ID(0)); // set a poly ID for no outlining (same as BG)

        glCallList((@cylinder_bin)); // draw a blue cylinder from a predefined packed command list


				glTranslate3f32(floattof32(0.5),floattof32(-2.6),floattof32(-4)); // translate the modelview matrix to the drawing location
				if(clicked = clSphere) then
					glPolyFmt(POLY_ALPHA(31) or POLY_CULL_BACK or POLY_FORMAT_LIGHT0 or POLY_ID(1)) // set a poly ID for outlining
				else
					glPolyFmt(POLY_ALPHA(31) or POLY_CULL_BACK or POLY_FORMAT_LIGHT0 or POLY_ID(0)); // set a poly ID for no outlining (same as BG)

				glCallList((@sphere_bin)); // draw a red sphere from a predefined packed command list

			glPopMatrix(1); // restores the modelview matrix to where it was just rotated
			
			// draw the scene again for picking
				clicked := clNothing; //reset what was clicked on
				closeW := $7FFFFFFF; //reset the distance

				//set the viewport to just off-screen, this hides all rendering that will be done during picking
				glViewport(0,192,0,192);
				
				// setup the projection matrix for picking
				glMatrixMode(GL_PROJECTION);
				glLoadIdentity();
				gluPickMatrix((touchXY.px),(191-touchXY.py),4,4,viewport); // render only what is below the cursor
				gluPerspective(60, 256.0 / 192.0, 0.1, 20); // this must be the same as the original perspective matrix
				
				glMatrixMode(GL_MODELVIEW); // switch back to modifying the modelview matrix for drawing
				
				glTranslate3f32(floattof32(2.9),floattof32(0),floattof32(0)); // translate the modelview matrix to the drawing location
				startCheck();
				glCallList((@cone_bin)); // draw a cone from a predefined packed command list
				endCheck(clCone);

				glTranslate3f32(floattof32(-3),floattof32(1.8),floattof32(2)); // translate the modelview matrix to the drawing location
				startCheck();
				glCallList((@cylinder_bin)); // draw a cylinder from a predefined packed command list
				endCheck(clCylinder);

				glTranslate3f32(floattof32(0.5),floattof32(-2.6),floattof32(-4)); // translate the modelview matrix to the drawing location
				startCheck();
				glCallList((@sphere_bin)); // draw a sphere from a predefined packed command list
				endCheck(clSphere);

		glPopMatrix(1); // restores the modelview matrix to its original state

		glFlush(0); // wait for everything to be drawn before starting on the next frame
	end;

end.
