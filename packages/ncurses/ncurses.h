
/* These are defined only in curses.h, and are used for conditional compiles */
#define NCURSES_VERSION_MAJOR 3
#define NCURSES_VERSION_MINOR 0
#define NCURSES_VERSION_PATCH 980228

/* This is defined in more than one ncurses header, for identification */
#undef  NCURSES_VERSION
#define NCURSES_VERSION "3.0"

/*
#ifdef NCURSES_NOMACROS
#define NCURSES_ATTR_T attr_t
#endif

#ifndef NCURSES_ATTR_T
#define NCURSES_ATTR_T int
#endif

#ifndef NCURSES_CONST
#define NCURSES_CONST 
#endif
*/

typedef unsigned long chtype;


#define CXX_BUILTIN_BOOL 1
/*
#define CXX_TYPE_OF_BOOL char
*/

/* colors */
extern int COLORS;
extern int COLOR_PAIRS;

#define COLOR_BLACK	0
#define COLOR_RED	1
#define COLOR_GREEN	2
#define COLOR_YELLOW	3
#define COLOR_BLUE	4
#define COLOR_MAGENTA	5
#define COLOR_CYAN	6
#define COLOR_WHITE	7

/* line graphics */

/*
extern	chtype acs_map[];
*/

/* VT100 symbols begin here */

/*
#define ACS_ULCORNER	(acs_map['l'])	
#define ACS_LLCORNER	(acs_map['m'])	
#define ACS_URCORNER	(acs_map['k'])	
#define ACS_LRCORNER	(acs_map['j'])	
#define ACS_LTEE	(acs_map['t'])	
#define ACS_RTEE	(acs_map['u'])	
#define ACS_BTEE	(acs_map['v'])	
#define ACS_TTEE	(acs_map['w'])	
#define ACS_HLINE	(acs_map['q'])	
#define ACS_VLINE	(acs_map['x'])	
#define ACS_PLUS	(acs_map['n'])	
#define ACS_S1		(acs_map['o'])	
#define ACS_S9		(acs_map['s'])	
#define ACS_DIAMOND	(acs_map['`'])	
#define ACS_CKBOARD	(acs_map['a'])	
#define ACS_DEGREE	(acs_map['f'])	
#define ACS_PLMINUS	(acs_map['g'])	
#define ACS_BULLET	(acs_map['~'])	

#define ACS_LARROW	(acs_map[','])	
#define ACS_RARROW	(acs_map['+'])	
#define ACS_DARROW	(acs_map['.'])	
#define ACS_UARROW	(acs_map['-'])	
#define ACS_BOARD	(acs_map['h'])	
#define ACS_LANTERN	(acs_map['i'])	
#define ACS_BLOCK	(acs_map['0'])	
#define ACS_S3		(acs_map['p'])	
#define ACS_S7		(acs_map['r'])	
#define ACS_LEQUAL	(acs_map['y'])	
#define ACS_GEQUAL	(acs_map['z'])	
#define ACS_PI		(acs_map['{'])	
#define ACS_NEQUAL	(acs_map['|'])	
#define ACS_STERLING	(acs_map['}'])	
*/

/*
 * Line drawing ACS names are of the form ACS_trbl, where t is the top, r
 * is the right, b is the bottom, and l is the left.  t, r, b, and l might
 * be B (blank), S (single), D (double), or T (thick).  The subset defined
 * here only uses B and S.
 */
/*
#define ACS_BSSB	ACS_ULCORNER
#define ACS_SSBB	ACS_LLCORNER
#define ACS_BBSS	ACS_URCORNER
#define ACS_SBBS	ACS_LRCORNER
#define ACS_SBSS	ACS_RTEE
#define ACS_SSSB	ACS_LTEE
#define ACS_SSBS	ACS_BTEE
#define ACS_BSSS	ACS_TTEE
#define ACS_BSBS	ACS_HLINE
#define ACS_SBSB	ACS_VLINE
#define ACS_SSSS	ACS_PLUS
*/

#define ERR     (-1)
#define OK      (0)


#define _SUBWIN         0x01	
#define _ENDLINE        0x02	
#define _FULLWIN        0x04	
#define _SCROLLWIN      0x08	
#define _ISPAD	        0x10	
#define _HASMOVED       0x20	
#define _WRAPPED        0x40	

/*
 * this value is used in the firstchar and lastchar fields to mark
 * unchanged lines
 */
#define _NOCHANGE       -1

/*
 * this value is used in the oldindex field to mark lines created by insertions
 * and scrolls.
 */
#define _NEWINDEX	-1

/*
typedef struct screen  SCREEN;
typedef struct _win_st WINDOW;
*/

typedef	chtype	attr_t;		

struct ldat
{
	chtype  *text;		
	short   firstchar;	
	short   lastchar;	
	short   oldindex;	
};

struct _win_st
{
	short   _cury, _curx;	
	short   _maxy, _maxx;	
	short   _begy, _begx;	
	short   _flags;		
	attr_t  _attrs;		
	chtype  _bkgd;		
	bool	_notimeout;	
	bool    _clear;		
	bool    _leaveok;	
	bool    _scroll;	
	bool    _idlok;		
	bool    _idcok;		
	bool	_immed;		
	bool	_sync;		
	bool    _use_keypad;    
	int	_delay;		
	struct ldat *_line;	
	short	_regtop;	
	short	_regbottom;	
	int	_parx;		
	int	_pary;		
	WINDOW	*_parent;	
	struct pdat
	{
	    short _pad_y,      _pad_x;
	    short _pad_top,    _pad_left;
	    short _pad_bottom, _pad_right;
	} _pad;

	short   _yoffset;	
};

extern WINDOW   *stdscr;
extern WINDOW   *curscr;
extern WINDOW   *newscr;

extern int	LINES;
extern int	COLS;
extern int	TABSIZE;

/*
 * This global was an undocumented feature under AIX curses.
 */
extern int ESCDELAY;	


extern int define_key (char *, int);
extern int keyok (int, bool);
extern int resizeterm (int, int);
extern int use_default_colors (void);
extern int wresize (WINDOW *, int, int);

/*
extern char ttytype[];		
*/

extern int baudrate(void);				
extern int beep(void);					
extern bool can_change_color(void);			
extern int cbreak(void);				
extern int clearok(WINDOW *,bool);			
extern int color_content(short,short*,short*,short*);	
extern int copywin(const WINDOW*,WINDOW*,int,int,int,int,int,int,int);	
extern int curs_set(int);				
extern int def_prog_mode(void);				
extern int def_shell_mode(void);			
extern int delay_output(int);				
extern void delscreen(SCREEN *);			
extern int delwin(WINDOW *);				
extern WINDOW *derwin(WINDOW *,int,int,int,int);	
extern int doupdate(void);				
extern WINDOW *dupwin(WINDOW *);			
extern int echo(void);					
extern int endwin(void);				
extern char erasechar(void);				
extern void filter(void);				
extern int flash(void);					
extern int flushinp(void);				
extern WINDOW *getwin(FILE *);				
extern int halfdelay(int);				
extern bool has_colors(void);				
extern int has_ic(void);				
extern int has_il(void);				
extern void idcok(WINDOW *, bool);			
extern int idlok(WINDOW *, bool);			
extern void immedok(WINDOW *, bool);			
extern WINDOW *initscr(void);				
extern int init_color(short,short,short,short);		
extern int init_pair(short,short,short);		
extern int intrflush(WINDOW *,bool);			
extern int isendwin(void);				
extern int is_linetouched(WINDOW *,int);		
extern int is_wintouched(WINDOW *);			
extern const char *keyname(int);			
extern int keypad(WINDOW *,bool);			
extern char killchar(void);				
extern int leaveok(WINDOW *,bool);			
extern char *longname(void);				
extern int meta(WINDOW *,bool);				
extern int mvcur(int,int,int,int);			
extern int mvderwin(WINDOW *, int, int);		
/*
extern int mvprintw(int,int,const char *,...)		
		GCC_PRINTFLIKE(3,4);
extern int mvscanw(int,int,const char *,...)		
		GCC_SCANFLIKE(3,4);
*/
extern int mvwin(WINDOW *,int,int);			
/*
extern int mvwprintw(WINDOW*,int,int,const char *,...)	
		GCC_PRINTFLIKE(4,5);
extern int mvwscanw(WINDOW *,int,int,const char *,...)	
		GCC_SCANFLIKE(4,5);
*/
extern int napms(int);					
extern WINDOW *newpad(int,int);				
extern SCREEN *newterm(const char *,FILE *,FILE *);	
extern WINDOW *newwin(int,int,int,int);			
extern int nl(void);					
extern int nocbreak(void);				
extern int nodelay(WINDOW *,bool);			
extern int noecho(void);				
extern int nonl(void);					
extern int noqiflush(void);				
extern int noraw(void);					
extern int notimeout(WINDOW *,bool);			
extern int overlay(const WINDOW*,WINDOW *);		
extern int overwrite(const WINDOW*,WINDOW *);		
extern int pair_content(short,short*,short*);		
extern int pechochar(WINDOW *, const chtype);		
extern int pnoutrefresh(WINDOW*,int,int,int,int,int,int);
extern int prefresh(WINDOW *,int,int,int,int,int,int);	
/*
extern int printw(const char *,...)			
		GCC_PRINTFLIKE(1,2);
*/
extern int putp(const char *);				
extern int putwin(WINDOW *, FILE *);			
extern int qiflush(void);				
extern int raw(void);					
extern int resetty(void);				
extern int reset_prog_mode(void);			
extern int reset_shell_mode(void);			
extern int ripoffline(int, int (*init)(WINDOW *, int));	
extern int savetty(void);				
/*
extern int scanw(const char *,...)			
		GCC_SCANFLIKE(1,2);
*/
extern int scr_dump(const char *);			
extern int scr_init(const char *);			
extern int scrollok(WINDOW *,bool);			
extern int scr_restore(const char *);			
extern int scr_set(const char *);			
extern SCREEN *set_term(SCREEN *);			
extern int slk_attroff(const attr_t);			
extern int slk_attron(const attr_t);			
extern int slk_attrset(const attr_t);			
extern attr_t slk_attr(void);                           
extern int slk_clear(void);				
extern int slk_init(int);				
extern char *slk_label(int);				
extern int slk_noutrefresh(void);			
extern int slk_refresh(void);				
extern int slk_restore(void);				
extern int slk_set(int,const char *,int);		
extern int slk_touch(void);				
extern int start_color(void);				
extern WINDOW *subpad(WINDOW *, int, int, int, int);	
extern WINDOW *subwin(WINDOW *,int,int,int,int);	
extern int syncok(WINDOW *, bool);			
extern chtype termattrs(void);				
extern char *termname(void);				
extern int tigetflag(const char *);			
extern int tigetnum(const char *);			
extern char *tigetstr(const char *);			
extern int typeahead(int);				
extern int ungetch(int);				
extern void use_env(bool);				
extern int vidattr(chtype);				
extern int vidputs(chtype, int (*)(int));		
extern int vwprintw(WINDOW *,const char *,va_list);	
extern int vwscanw(WINDOW *,const char *,va_list);	
extern int waddch(WINDOW *, const chtype);		
extern int waddchnstr(WINDOW *,const chtype *const,int); 
extern int waddnstr(WINDOW *,const char *const,int);	
extern int wattr_on(WINDOW *, const attr_t);		
extern int wattr_off(WINDOW *, const attr_t);		
extern int wbkgd(WINDOW *,const chtype);		
extern void wbkgdset(WINDOW *,chtype);			
extern int wborder(WINDOW *,chtype,chtype,chtype,chtype,chtype,chtype,chtype,chtype);	
extern int wchgat(WINDOW *, int, attr_t, short, const void *);
extern int wclear(WINDOW *);				
extern int wclrtobot(WINDOW *);				
extern int wclrtoeol(WINDOW *);				
extern void wcursyncup(WINDOW *);			
extern int wdelch(WINDOW *);				
extern int wechochar(WINDOW *, const chtype);		
extern int werase(WINDOW *);				
extern int wgetch(WINDOW *);				
extern int wgetnstr(WINDOW *,char *,int);		
extern int whline(WINDOW *, chtype, int);		
extern int winchnstr(WINDOW *, chtype *, int);		
extern int winnstr(WINDOW *, char *, int);		
extern int winsch(WINDOW *, chtype);			
extern int winsdelln(WINDOW *,int);			
extern int winsnstr(WINDOW *, const char *,int);	
extern int wmove(WINDOW *,int,int);			
extern int wnoutrefresh(WINDOW *);			
/*
extern int wprintw(WINDOW *,const char *,...)		
		GCC_PRINTFLIKE(2,3);
*/
extern int wredrawln(WINDOW *,int,int);			
extern int wrefresh(WINDOW *);				
/*
extern int wscanw(WINDOW *,const char *,...)		
		GCC_SCANFLIKE(2,3);
*/
extern int wscrl(WINDOW *,int);				
extern int wsetscrreg(WINDOW *,int,int);		
extern void wsyncdown(WINDOW *);			
extern void wsyncup(WINDOW *);				
extern int wtimeout(WINDOW *,int);			
extern int wtouchln(WINDOW *,int,int,int);		
extern int wvline(WINDOW *,chtype,int);			



#define NCURSES_BITS(mask,shift) ((mask) << ((shift) + 8))

#define A_NORMAL	0L
/*
#define A_ATTRIBUTES	NCURSES_BITS(~(1UL - 1UL),0)
*/
#define A_CHARTEXT	(NCURSES_BITS(1UL,0) - 1UL)
#define A_COLOR		NCURSES_BITS(((1UL) << 8) - 1UL,0)
#define A_STANDOUT	NCURSES_BITS(1UL,8)
#define A_UNDERLINE	NCURSES_BITS(1UL,9)
#define A_REVERSE	NCURSES_BITS(1UL,10)
#define A_BLINK		NCURSES_BITS(1UL,11)
#define A_DIM		NCURSES_BITS(1UL,12)
#define A_BOLD		NCURSES_BITS(1UL,13)
#define A_ALTCHARSET	NCURSES_BITS(1UL,14)
#define A_INVIS		NCURSES_BITS(1UL,15)
#define A_PROTECT	NCURSES_BITS(1UL,16)
#define A_HORIZONTAL	NCURSES_BITS(1UL,17)
#define A_LEFT		NCURSES_BITS(1UL,18)
#define A_LOW		NCURSES_BITS(1UL,19)
#define A_RIGHT		NCURSES_BITS(1UL,20)
#define A_TOP		NCURSES_BITS(1UL,21)
#define A_VERTICAL	NCURSES_BITS(1UL,22)

#define COLOR_PAIR(n)	NCURSES_BITS(n, 0)
#define PAIR_NUMBER(a)	(((a) & A_COLOR) >> 8)

/*
 * pseudo functions
 */
#define wgetstr(w, s)		wgetnstr(w, s, -1)
#define getnstr(s, n)		wgetnstr(stdscr, s, n)

#define setterm(term)		setupterm(term, 1, (int *)0)

#define fixterm()		reset_prog_mode()
#define resetterm()		reset_shell_mode()
#define saveterm()		def_prog_mode()
#define crmode()		cbreak()
#define nocrmode()		nocbreak()

/*
#define getyx(win,y,x)   	(y = (win)?(win)->_cury:ERR, x = (win)?(win)->_curx:ERR)
#define getbegyx(win,y,x)	(y = (win)?(win)->_begy:ERR, x = (win)?(win)->_begx:ERR)
#define getmaxyx(win,y,x)	(y = (win)?((win)->_maxy + 1):ERR, x = (win)?((win)->_maxx + 1):ERR)
#define getparyx(win,y,x)	(y = (win)?(win)->_pary:ERR, x = (win)?(win)->_parx:ERR)
*/
#define getsyx(y,x)		getyx(stdscr, y, x)
/*
#define setsyx(y,x)		(stdscr->_cury = y, stdscr->_curx = x)
*/


#define getattrs(win)		((win)?(win)->_attrs:A_NORMAL)
#define getcurx(win)		((win)?(win)->_curx:ERR)
#define getcury(win)		((win)?(win)->_cury:ERR)
#define getbegx(win)		((win)?(win)->_begx:ERR)
#define getbegy(win)		((win)?(win)->_begy:ERR)
#define getmaxx(win)		((win)?((win)->_maxx + 1):ERR)
#define getmaxy(win)		((win)?((win)->_maxy + 1):ERR)
#define getparx(win)		((win)?(win)->_parx:ERR)
#define getpary(win)		((win)?(win)->_pary:ERR)


/*
#define winch(win)       	((win)?(win)->_line[(win)->_cury].text[(win)->_curx]:0)
*/
#define wstandout(win)      	(wattr_set(win,A_STANDOUT))
#define wstandend(win)      	(wattr_set(win,A_NORMAL))
/*
#define wattr_set(win,at)    	((win)?((win)->_attrs = (at)):0)
*/

#define wattron(win,at)		wattr_on(win, at)
#define wattroff(win,at)	wattr_off(win, at)
#define wattrset(win,at)    	wattr_set(win, at)

#define scroll(win)		wscrl(win,1)

#define touchwin(win)		wtouchln((win), 0, getmaxy(win), 1)
#define touchline(win, s, c)	wtouchln((win), s, c, 1)
#define untouchwin(win)		wtouchln((win), 0, getmaxy(win), 0)

#define box(win, v, h)		wborder(win, v, v, h, h, 0, 0, 0, 0)
#define border(ls, rs, ts, bs, tl, tr, bl, br)	wborder(stdscr, ls, rs, ts, bs, tl, tr, bl, br)
#define hline(ch, n)		whline(stdscr, ch, n)
#define vline(ch, n)		wvline(stdscr, ch, n)

#define winstr(w, s)		winnstr(w, s, -1)
#define winchstr(w, s)		winchnstr(w, s, -1)
#define winsstr(w, s)		winsnstr(w, s, -1)

#define redrawwin(w)		wredrawln(w, 0, w->_maxy+1)
#define waddstr(win,str)	waddnstr(win,st,-1)
#define waddchstr(win,st)	waddchnstr(win,st,-1)

/*
 * pseudo functions for standard screen
 */

#define addch(ch)      		waddch(stdscr,ch)
#define addchnstr(st,n)	waddchnstr(stdscr,st,n)
#define addchstr(st)		waddchstr(stdscr,st)
#define addnstr(st,n)		waddnstr(stdscr,st,n)
#define addstr(st)    		waddnstr(stdscr,st,-1)
#define attroff(at)    		wattroff(stdscr,at)
#define attron(at)     		wattron(stdscr,at)
#define attrset(at)    		wattrset(stdscr,at)
#define bkgd(ch)		wbkgd(stdscr,ch)
#define bkgdset(ch)		wbkgdset(stdscr,ch)
#define clear()        		wclear(stdscr)
#define clrtobot()     		wclrtobot(stdscr)
#define clrtoeol()     		wclrtoeol(stdscr)
#define delch()        		wdelch(stdscr)
#define deleteln()     		winsdelln(stdscr,-1)
#define echochar(c)		wechochar(stdscr,c)
#define erase()        		werase(stdscr)
#define getch()        		wgetch(stdscr)
#define getstr(st)    		wgetstr(stdscr,st)
#define inch()       		winch(stdscr)
#define inchnstr(s,n)		winchnstr(stdscr,s,n)
#define inchstr(s)		winchstr(stdscr,s)
#define innstr(s,n)		winnstr(stdscr,s,n)
#define insch(c)       		winsch(stdscr,c)
#define insdelln(n)		winsdelln(stdscr,n)
#define insertln()     		winsdelln(stdscr,1)
#define insnstr(s,n)		winsnstr(stdscr,s,n)
#define insstr(s)		winsstr(stdscr,s)
#define instr(s)		winstr(stdscr,s)
#define move(y,x)     		wmove(stdscr,y,x)
#define refresh()      		wrefresh(stdscr)
#define scrl(n)			wscrl(stdscr,n)
#define setscrreg(t,b) 		wsetscrreg(stdscr,t,b)
#define standend()     		wstandend(stdscr)
#define standout()     		wstandout(stdscr)
#define timeout(delay)		wtimeout(stdscr,delay)
#define wdeleteln(win)     	winsdelln(win,-1)
#define winsertln(win)     	winsdelln(win,1)

/*
 * mv functions
 */

/*
#define mvwaddch(win,y,x,ch)    	(wmove(win,y,x) == ERR ? ERR : waddch(win,ch))
#define mvwaddchnstr(win,y,x,st,n)	(wmove(win,y,x) == ERR ? ERR : waddchnstr(win,st,n))
#define mvwaddchstr(win,y,x,st)  	(wmove(win,y,x) == ERR ? ERR : waddchnstr(win,st,-1))
#define mvwaddnstr(win,y,x,st,n)	(wmove(win,y,x) == ERR ? ERR : waddnstr(win,st,n))
#define mvwaddstr(win,y,x,st)  	        (wmove(win,y,x) == ERR ? ERR : waddnstr(win,st,-1))
#define mvwdelch(win,y,x)       	(wmove(win,y,x) == ERR ? ERR : wdelch(win))
#define mvwgetch(win,y,x)       	(wmove(win,y,x) == ERR ? ERR : wgetch(win))
#define mvwgetnstr(win,y,x,st,n)    	(wmove(win,y,x) == ERR ? ERR : wgetnstr(win,st,n))
#define mvwgetstr(win,y,x,st)      	(wmove(win,y,x) == ERR ? ERR : wgetstr(win,st))
#define mvwhline(win,y,x,c,n)     	(wmove(win,y,x) == ERR ? ERR : whline(win,c,n))
#define mvwinch(win,y,x)        	(wmove(win,y,x) == ERR ? (chtype)ERR : winch(win))
#define mvwinchnstr(win,y,x,s,n)	(wmove(win,y,x) == ERR ? ERR : winchnstr(win,s,n))
#define mvwinchstr(win,y,x,s)		(wmove(win,y,x) == ERR ? ERR : winchstr(win,s))
#define mvwinnstr(win,y,x,s,n)		(wmove(win,y,x) == ERR ? ERR : winnstr(win,s,n))
#define mvwinsch(win,y,x,c)     	(wmove(win,y,x) == ERR ? ERR : winsch(win,c))
#define mvwinsnstr(win,y,x,s,n)		(wmove(win,y,x) == ERR ? ERR : winsnstr(win,s,n))
#define mvwinsstr(win,y,x,s)		(wmove(win,y,x) == ERR ? ERR : winsstr(win,s))
#define mvwinstr(win,y,x,s)		(wmove(win,y,x) == ERR ? ERR : winstr(win,s))
#define mvwvline(win,y,x,c,n)     	(wmove(win,y,x) == ERR ? ERR : wvline(win,c,n))
*/

#define mvaddch(y,x,ch)         	mvwaddch(stdscr,y,x,ch)
#define mvaddchnstr(y,x,st,n)		mvwaddchnstr(stdscr,y,x,st,n)
#define mvaddchstr(y,x,st)		mvwaddchstr(stdscr,y,x,st)
#define mvaddnstr(y,x,st,n)		mvwaddnstr(stdscr,y,x,st,n)
#define mvaddstr(y,x,st)       	mvwaddstr(stdscr,y,x,st)
#define mvdelch(y,x)            	mvwdelch(stdscr,y,x)
#define mvgetch(y,x)            	mvwgetch(stdscr,y,x)
#define mvgetnstr(y,x,st,n)		mvwgetnstr(stdscr,y,x,st,n)
#define mvgetstr(y,x,st)           	mvwgetstr(stdscr,y,x,st)
#define mvhline(y,x,c,n)		mvwhline(stdscr,y,x,c,n)
#define mvinch(y,x)             	mvwinch(stdscr,y,x)
#define mvinchnstr(y,x,s,n)		mvwinchnstr(stdscr,y,x,s,n)
#define mvinchstr(y,x,s)		mvwinchstr(stdscr,y,x,s)
#define mvinnstr(y,x,s,n)		mvwinnstr(stdscr,y,x,s,n)
#define mvinsch(y,x,c)          	mvwinsch(stdscr,y,x,c)
#define mvinsnstr(y,x,s,n)		mvwinsnstr(stdscr,y,x,s,n)
#define mvinsstr(y,x,s)			mvwinsstr(stdscr,y,x,s)
#define mvinstr(y,x,s)			mvwinstr(stdscr,y,x,s)
#define mvvline(y,x,c,n)		mvwvline(stdscr,y,x,c,n)

#define add_wch(c)			wadd_wch(stsdscr,c)
#define addnwstr(wstr,n)		waddnwstr(stdscr,wstr,n)
#define addwstr(wstr,n)			waddnwstr(stdscr,wstr,-1)
#define attr_get()			wattr_get(stdscr)
#define attr_off(a)			wattr_off(stdscr,a)
#define attr_on(a)			wattr_on(stdscr,a)
#define attr_set(a)			wattr_set(stdscr,a)
#define box_set(w,v,h)			wborder_set(w,v,v,h,h,0,0,0,9)
#define chgat(n,a,c,o)			wchgat(stdscr,n,a,c,o)
#define echo_wchar(c)			wecho_wchar(stdscr,c)
#define getbkgd(win)			((win)->_bkgd)
#define get_wch(c)			wget_wch(stdscr,c)
#define get_wstr(t)			wgetn_wstr(stdscr,t,-1)
#define getn_wstr(t,n)			wgetn_wstr(stdscr,t,n)
#define hline_set(c,n)			whline_set(stdscr,c,n)
#define in_wch(c)			win_wch(stdscr,c)
#define in_wchnstr(c,n)			win_wchnstr(stdscr,c,n)
#define in_wchstr(c)			win_wchnstr(stdscr,c,-1)
#define innwstr(c,n)			winnwstr(stdscr,c,n)
#define ins_nwstr(t,n)			wins_nwstr(stdscr,t,n)
#define ins_wch(c)			wins_wch(stdscr,c)
#define ins_wstr(t)			wins_nwstr(stdscr,t,-1)
#define inwstr(c)			winnwstr(stdscr,c,-1)

#define mvadd_wch(y,x,c)		mvwadd_wch(stdscr,y,x,c)
#define mvaddnwstr(y,x,wstr,n)		mvwaddnwstr(stdscr,y,x,wstr,n)
#define mvaddwstr(y,x,wstr,n)		mvwaddnwstr(stdscr,y,x,wstr,-1)
#define mvchgat(y,x,n,a,c,o)		mvwchgat(stdscr,y,x,n,a,c,o)
#define mvget_wch(y,x,c)		mvwget_wch(stdscr,y,x,c)
#define mvget_wstr(y,x,t)		mvwgetn_wstr(stdscr,y,x,t,-1)
#define mvgetn_wstr(y,x,t,n)		mvwgetn_wstr(stdscr,y,x,t,n)
#define mvhline_set(y,x,c,n)		mvwhline_set(stdscr,y,x,c,n)
#define mvin_wch(y,x,c)			mvwin_wch(stdscr,y,x,c)
#define mvin_wchnstr(y,x,c,n)		mvwin_wchnstr(stdscr,y,x,c,n)
#define mvin_wchstr(y,x,c)		mvwin_wchnstr(stdscr,y,x,c,-1)
#define mvinnwstr(y,x,c,n)		mvwinnwstr(stdscr,y,x,c,n)
#define mvins_nwstr(y,x,t,n)		mvwins_nwstr(stdscr,y,x,t,n)
#define mvins_wch(y,x,c)		mvwins_wch(stdscr,y,x,c)
#define mvins_wstr(y,x,t)		mvwins_nwstr(stdscr,y,x,t,-1)
#define mvinwstr(y,x,c)			mvwinnwstr(stdscr,y,x,c,-1)
#define mvvline_set(y,x,c,n)		mvwvline_set(stdscr,y,x,c,n)

/*
#define mvwadd_wch(y,x,win,c)		(wmove(win,y,x) == ERR ? ERR : wadd_wch(stsdscr,c))
#define mvwaddnwstr(y,x,win,wstr,n)	(wmove(win,y,x) == ERR ? ERR : waddnwstr(stdscr,wstr,n))
#define mvwaddwstr(y,x,win,wstr,n)	(wmove(win,y,x) == ERR ? ERR : waddnwstr(stdscr,wstr,-1))
#define mvwchgat(win,y,x,n,a,c,o)	(wmove(win,y,x) == ERR ? ERR : wchgat(win,n,a,c,o))
#define mvwget_wch(win,y,x,c)		(wmove(win,y,x) == ERR ? ERR : wget_wch(win,n))
#define mvwget_wstr(win,y,x,t)		(wmove(win,y,x) == ERR ? ERR : wgetn_wstr(win,t,-1))
#define mvwgetn_wstr(win,y,x,t,n)	(wmove(win,y,x) == ERR ? ERR : wgetn_wstr(win,t,n))
#define mvwhline_set(win,y,x,c,n)	(wmove(win,y,x) == ERR ? ERR : whline_set(win,c,n))
#define mvwin_wch(win,y,x,c)		(wmove(win,y,x) == ERR ? ERR : win_wch(win,c))
#define mvwin_wchnstr(win,y,x,c,n)	(wmove(win,y,x) == ERR ? ERR : win_wchnstr(stdscr,c,n))
#define mvwin_wchstr(win,y,x,c)		(wmove(win,y,x) == ERR ? ERR : win_wchnstr(stdscr,c,-1))
#define mvwinnwstr(win,y,x,c,n)		(wmove(win,y,x) == ERR ? ERR : winnwstr(stdscr,c,n))
#define mvwins_nwstr(win,y,x,t,n)	(wmove(win,y,x) == ERR ? ERR : wins_nwstr(stdscr,t,n))
#define mvwins_wch(win,y,x,c)		(wmove(win,y,x) == ERR ? ERR : wins_wch(c))
#define mvwins_wstr(win,y,x,t)		(wmove(win,y,x) == ERR ? ERR : wins_nwstr(stdscr,t,-1))
#define mvwinwstr(win,y,x,c)		(wmove(win,y,x) == ERR ? ERR : winnwstr(stdscr,c,-1))
#define mvwvline_set(win,y,x,c,n)	(wmove(win,y,x) == ERR ? ERR : wvline_set(win,c,n))
*/

#define slk_attr_off(a)			slk_attroff(a)
#define slk_attr_on(a)			slk_attron(a)
#define slk_attr_set(a)			slk_attrset(a)
#define vid_attr(a)			vidattr(a)
#define vline_set(c,n)			vhline_set(stdscr,c,n)
#define waddwstr(win,wstr,n)		waddnwstr(win,wstr,-1)
#define wattr_get(win)			((win)->_attrs)
#define wget_wstr(w,t)			wgetn_wstr(w,t,-1)
#define win_wchstr(w,c)			win_wchnstr(w,c,-1)
#define wins_wstr(w,t)			wins_nwstr(w,t,-1)
#define winwstr(w,c)			winnwstr(w,c,-1)


/*
 * Pseudo-character tokens outside ASCII range.  The curses wgetch() function
 * will return any given one of these only if the corresponding k- capability
 * is defined in your terminal's terminfo entry.
 */
#define KEY_CODE_YES	0400		
#define KEY_MIN		0401		
#define KEY_BREAK       0401            
#define KEY_DOWN        0402            
#define KEY_UP          0403		
#define KEY_LEFT        0404		
#define KEY_RIGHT       0405            
#define KEY_HOME        0406            
#define KEY_BACKSPACE   0407            
#define KEY_F0          0410            
#define KEY_F(n)        (KEY_F0+(n))    
#define KEY_DL          0510            
#define KEY_IL          0511            
#define KEY_DC          0512            
#define KEY_IC          0513            
#define KEY_EIC         0514            
#define KEY_CLEAR       0515            
#define KEY_EOS         0516            
#define KEY_EOL         0517            
#define KEY_SF          0520            
#define KEY_SR          0521            
#define KEY_NPAGE       0522            
#define KEY_PPAGE       0523            
#define KEY_STAB        0524            
#define KEY_CTAB        0525            
#define KEY_CATAB       0526            
#define KEY_ENTER       0527            
#define KEY_SRESET      0530            
#define KEY_RESET       0531            
#define KEY_PRINT       0532            
#define KEY_LL          0533            

#define KEY_A1		0534		
#define KEY_A3		0535		
#define KEY_B2		0536		
#define KEY_C1		0537		
#define KEY_C3		0540		
#define KEY_BTAB	0541		
#define KEY_BEG		0542		
#define KEY_CANCEL	0543		
#define KEY_CLOSE	0544		
#define KEY_COMMAND	0545		
#define KEY_COPY	0546		
#define KEY_CREATE	0547		
#define KEY_END		0550		
#define KEY_EXIT	0551		
#define KEY_FIND	0552		
#define KEY_HELP	0553		
#define KEY_MARK	0554		
#define KEY_MESSAGE	0555		
#define KEY_MOVE	0556		
#define KEY_NEXT	0557		
#define KEY_OPEN	0560		
#define KEY_OPTIONS	0561		
#define KEY_PREVIOUS	0562		
#define KEY_REDO	0563		
#define KEY_REFERENCE	0564		
#define KEY_REFRESH	0565		
#define KEY_REPLACE	0566		
#define KEY_RESTART	0567		
#define KEY_RESUME	0570		
#define KEY_SAVE	0571		
#define KEY_SBEG	0572		
#define KEY_SCANCEL	0573		
#define KEY_SCOMMAND	0574		
#define KEY_SCOPY	0575		
#define KEY_SCREATE	0576		
#define KEY_SDC		0577		
#define KEY_SDL		0600		
#define KEY_SELECT	0601		
#define KEY_SEND	0602		
#define KEY_SEOL	0603		
#define KEY_SEXIT	0604		
#define KEY_SFIND	0605		
#define KEY_SHELP	0606		
#define KEY_SHOME	0607		
#define KEY_SIC		0610		
#define KEY_SLEFT	0611		
#define KEY_SMESSAGE	0612		
#define KEY_SMOVE	0613		
#define KEY_SNEXT	0614		
#define KEY_SOPTIONS	0615		
#define KEY_SPREVIOUS	0616		
#define KEY_SPRINT	0617		
#define KEY_SREDO	0620		
#define KEY_SREPLACE	0621		
#define KEY_SRIGHT	0622		
#define KEY_SRSUME	0623		
#define KEY_SSAVE	0624		
#define KEY_SSUSPEND	0625		
#define KEY_SUNDO	0626		
#define KEY_SUSPEND	0627		
#define KEY_UNDO	0630		
#define KEY_MOUSE	0631		
#define KEY_RESIZE	0632		
#define KEY_MAX		0777		


extern int mcprint(char *, int);	
extern int has_key(int);		
