program BothScreens3D;

uses
  ctypes, nds9;

procedure renderCube(angle: cint);
begin 
  glPushMatrix();
  glTranslatef(0, 0, -4);
  glRotatef32i(degreesToAngle(angle), inttof32(1), inttof32(1), inttof32(1)); 
  
  glBegin(GL_QUADS);
    glColor3b(255,0,0);   glVertex3f(-1.0,  1.0,  1.0);					
    glColor3b(0,255,0);   glVertex3f( 1.0,  1.0,  1.0);					
    glColor3b(0,0,255);   glVertex3f( 1.0, -1.0,  1.0);					
    glColor3b(255,255,0); glVertex3f(-1.0, -1.0,  1.0);					
    
    glColor3b(255,0,0);   glVertex3f(-1.0,  1.0, -1.0);					
    glColor3b(0,255,0);   glVertex3f( 1.0,  1.0, -1.0);					
    glColor3b(0,0,255);   glVertex3f( 1.0, -1.0, -1.0);					
    glColor3b(255,255,0); glVertex3f(-1.0, -1.0, -1.0);					
    
    glColor3b(255,0,0);   glVertex3f(-1.0,  1.0,  1.0);					
    glColor3b(0,255,0);   glVertex3f( 1.0,  1.0,  1.0);					
    glColor3b(0,0,255);   glVertex3f( 1.0,  1.0, -1.0);					
    glColor3b(255,255,0); glVertex3f(-1.0,  1.0, -1.0);					
    
    glColor3b(255,0,0);   glVertex3f(-1.0, -1.0,  1.0);					
    glColor3b(0,255,0);   glVertex3f( 1.0, -1.0,  1.0);					
    glColor3b(0,0,255);   glVertex3f( 1.0, -1.0, -1.0);					
    glColor3b(255,255,0); glVertex3f(-1.0, -1.0, -1.0);					
    
    glColor3b(255,0,0);   glVertex3f( 1.0, 1.0,  -1.0);					
    glColor3b(0,255,0);   glVertex3f( 1.0, 1.0,   1.0);					
    glColor3b(0,0,255);   glVertex3f( 1.0,-1.0,   1.0);					
    glColor3b(255,255,0); glVertex3f( 1.0,-1.0,  -1.0);					
    
    glColor3b(255,0,0);   glVertex3f(-1.0, 1.0,  -1.0);					
    glColor3b(0,255,0);   glVertex3f(-1.0, 1.0,   1.0);					
    glColor3b(0,0,255);   glVertex3f(-1.0,-1.0,   1.0);					
    glColor3b(255,255,0); glVertex3f(-1.0,-1.0,  -1.0);					
  
  glEnd();
  
  glPopMatrix(1);
end;
 

procedure renderPyramid(angle: cint);
begin
  glPushMatrix();
  glTranslatef(0, 0, -4);
  glRotatef32i(degreesToAngle(angle), inttof32(1),inttof32(1),inttof32(1)); 
	
  glBegin(GL_QUADS);
    glColor3b(255,0,0);		glVertex3f(-1.0, -1.0,  1.0);					
    glColor3b(0,255,0);		glVertex3f( 1.0, -1.0,  1.0);					
    glColor3b(0,0,255);		glVertex3f( 1.0, -1.0, -1.0);					
    glColor3b(255,255,0);	glVertex3f(-1.0, -1.0, -1.0);					
  glEnd();

  glBegin(GL_TRIANGLES);
    glColor3b(255,0,0);		glVertex3f( 0.0,  1.0,  0.0);					
    glColor3b(0,255,0);		glVertex3f(-1.0, -1.0,  1.0);					
    glColor3b(0,0,255);		glVertex3f( 1.0, -1.0,  1.0);					
    
    glColor3b(255,0,0);		glVertex3f( 0.0,  1.0,  0.0);					
    glColor3b(0,255,0);		glVertex3f(-1.0, -1.0, -1.0);					
    glColor3b(0,0,255);		glVertex3f( 1.0, -1.0, -1.0);					
    
    glColor3b(255,0,0);		glVertex3f( 0.0,  1.0,  0.0);					
    glColor3b(0,255,0);		glVertex3f(-1.0, -1.0,  1.0);					
    glColor3b(0,0,255);		glVertex3f(-1.0, -1.0, -1.0);					
    
    glColor3b(255,0,0);		glVertex3f( 0.0,  1.0,  0.0);					
    glColor3b(0,255,0);		glVertex3f( 1.0, -1.0,  1.0);					
    glColor3b(0,0,255);		glVertex3f( 1.0, -1.0, -1.0);					
  glEnd();
 
  glPopMatrix(1);
end;
 
var
  angle: cshort = 0;
  
procedure renderScene(top: boolean);
begin
  if (top) then
    renderCube(angle)
  else
    renderPyramid(angle);

	inc(angle);
end;
 
 
//-------------------------------------------------------
// set up a 2D layer construced of bitmap sprites
// this holds the image when rendering to the top screen
//-------------------------------------------------------
procedure initSubSprites();
var
  x: integer;
  y: integer;
  offset: pcuint16;
begin
  oamInit(oamSub, SpriteMapping_Bmp_2D_256, false);
  
  x := 0;
  y := 0;

  //set up a 4x3 grid of 64x64 sprites to cover the screen
  for y := 0 to 2 do
    for x := 0 to 3 do
    begin
      offset := pcuint16(@SPRITE_GFX_SUB[(x * 64) + (y * 64 * 256)]);
      oamSet(oamSub, x + y * 4, x * 64, y * 64, 0, 15, SpriteSize_64x64, 
			SpriteColorFormat_Bmp, offset, -1, false,false,false,false,false);
    end;
 
  swiWaitForVBlank();
 
  oamUpdate(oamSub);
end;

var
	top: boolean = true;

begin 
  videoSetMode(MODE_0_3D);
  videoSetModeSub(MODE_5_2D);
 
  glInit();
 
  // sub sprites hold the bottom image when 3D directed to top
  initSubSprites();
  
  // sub background holds the top image when 3D directed to bottom
  bgInitSub(3, BgType_Bmp16, BgSize_B16_256x256, 0, 0);
 
//-------------------------------------------------------
//	 Setup gl
//-------------------------------------------------------
  glEnable(GL_ANTIALIAS);
  
  glClearColor(0,0,0,31); 
  glClearPolyID(63);
  glClearDepth($7FFF);
  
  glViewport(0,0,255,191);
  
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(70, 256.0 / 192.0, 0.1, 100);
  
  glPolyFmt(POLY_ALPHA(31) or POLY_CULL_NONE);
 
//-------------------------------------------------------
//	 main loop
//-------------------------------------------------------

  while true do 
  begin
    // wait for capture unit to be ready
    while (REG_DISPCAPCNT^ and DCAP_ENABLE) <> 0 do;
    
    //-------------------------------------------------------
    //	 Switch render targets
    //-------------------------------------------------------
    top := not top;
 
    if (top) then
    begin
      lcdMainOnBottom();
      vramSetBankC(VRAM_C_LCD);
      vramSetBankD(VRAM_D_SUB_SPRITE);
      REG_DISPCAPCNT^ := DCAP_BANK(2) or DCAP_ENABLE or DCAP_SIZE(3);
    end	else
    begin
      lcdMainOnTop();
      vramSetBankD(VRAM_D_LCD);
      vramSetBankC(VRAM_C_SUB_BG);
      REG_DISPCAPCNT^ := DCAP_BANK(3) or DCAP_ENABLE or DCAP_SIZE(3);
    end;
 
    //-------------------------------------------------------
    //	 Render the scene
    //-------------------------------------------------------
    glMatrixMode(GL_MODELVIEW);
    
    renderScene(top);
    
    glFlush(0);
  end; 
end.
