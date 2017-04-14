// Comes from https://news.ycombinator.com/item?id=14046446, which references:
// http://viewsourcecode.org/snaptoken/kilo/
// what it's based on: http://antirez.com/news/108, 
// 	hn comments: https://news.ycombinator.com/item?id=12065217
//
// someone else has tweaked it some more: 
//    https://www.reddit.com/r/programming/comments/64uinn/cli_text_editor_written_in_c_from_scratch/
// It has heaps of comments and has a couple of interesting additions which I've incorporated: 
// - it copes with dynamic screen size changes (uses a signal)
// - restores original screen (using separate terminal buffers
//
// VT100 reference: http://vt100.net/docs/vt100-ug/
//
// fun debug technique - redirect stderr messages to another screen:
// 1. find the device name of the other screen by typing "tty". It will o/p eg /dev/pts/1
// 2. on cmd line redirect any error output to this device - eg ./kilo 2>/dev/pts/1
// 3. write debug messages to stderr, eg by using the debug macro below
//
// added debug macro by stealing the idea from "Learning C the hard way", page 92
// couldn't get the macro to compile warniong-free, except by always including a 2nd para.



/*** includes ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>


/*** defines ***/

#define KILO_VERSION "0.0.1"
#define KILO_TAB_STOP 8
#define KILO_QUIT_TIMES 3

#define CTRL_KEY(k) ((k) & 0x1f)

//#define debug(M, ...) fprintf(stderr, "DEBUG %s:%d: " M "\n", __FILE__, __LINE__, ##__VA_ARGS__) // orig from Learn C
// following inspired by http://stackoverflow.com/questions/3576396/variadic-macros-with-0-arguments-in-c99
#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
#define debug(...) fprintf(stderr, "\nDEBUG " __FILE__ ":" TOSTRING(__LINE__) ": " __VA_ARGS__);

enum editorKey {
	BACKSPACE = 127,
	ARROW_LEFT = 1000,
	ARROW_RIGHT,
	ARROW_UP,
	ARROW_DOWN,
	DEL_KEY,
	HOME_KEY,
	END_KEY,
	PAGE_UP,
	PAGE_DOWN
};

enum editorHilight {
	HL_NORMAL = 0,
	HL_COMMENT,
	HL_MLCOMMENT,
	HL_KEYWORD1,
	HL_KEYWORD2,
	HL_STRING,
	HL_NUMBER,
	HL_MATCH
};

// bitfield for flags field in editorSyntax struct
#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

/*** data ***/

struct editorSyntax {
	char *filetype;
	char **filematch;	// array of strings, each string is a pattern to match
	char **keywords;
	char *singleline_comment_start; // differs between languages
	char *multiline_comment_start;
	char *multiline_comment_end;
	int flags;			// bit field - whether to highlight numbers, strings
};

typedef struct erow {
	int idx;	// know my own index; allow each row to examine previous row's open_commetn status
	int size;
	int rsize;
	char *chars;
	char *render;	// how we process tabs & display them as spaces
	unsigned char *hl;
	int hl_open_comment;
} erow;

typedef struct coord {
	int x;
	int y;
} coord;

struct editorConfig {
	coord c;
	int rx;		// correctly position the cursor when tabs are present
	coord offset;
	coord screen;
	int numrows;	// number of rows of actual text being edited
	erow *row;
	int dirty;
	char *filename;
	char statusmsg[80];
	time_t statusmsg_time;
	struct editorSyntax *syntax;
	struct termios orig_termios;
};

struct editorConfig E;

/*** prototypes ***/

void die(const char *s);
void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));
void consoleBufferOpen();

/*** utilities ***/

// search backwards from start towards beg looking for an occurence of find
// return NULL if not found
// (start should always point inside the string starting at beg)
char *revstrstr(char *beg, char *start, char *find) {
	if (start < beg)
		die("invalid pointers to revstrstr");
	
	size_t findlen = strlen(find);
	for (char * p = start; p >= beg; p--) {
		if (strncmp(p, find, findlen) == 0)
			return p;
	}
	return NULL;
}
	
	

/*** filetypes ***/

char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };
char *C_HL_keywords[] = {
	// keywords
	"switch", "if", "while", "for", "break", "continue", "return", "else",
	"struct", "union", "typedef", "static", "enum", "class", "case",
	
	// C types
	"int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
	"void|", NULL
};

struct editorSyntax HLDB[] = {	// HLDB = Highlight Database
	{
		"c",
		C_HL_extensions,
		C_HL_keywords,
		"//", "/*", "*/",
		HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
	},
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/*** terminal ***/

void die(const char *s) {
	write(STDOUT_FILENO, "\x1b[2J", 4);
	write(STDOUT_FILENO, "\x1b[H", 3);
	
	//write(STDOUT_FILENO, "Die: ", 5);
	perror(s);
	exit(1);
}

void disableRawMode() {
	if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) ==-1)
		die("tcsetattr");
}

void enableRawMode() {
	if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die ("tcgetarr");
	atexit(disableRawMode);
	
	struct termios raw = E.orig_termios;
	raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
	raw.c_oflag &= ~(OPOST);
	raw.c_cflag &= ~(CS8);
	raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
	raw.c_cc[VMIN] = 0;		// return each byte, or zero for timeout
	raw.c_cc[VTIME] = 1;		// 100ms timeout *units of tenths of second
	
	consoleBufferOpen();
	
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

int editorReadKey() {
	int nread;
	char c;
	while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
		if (nread == -1 && errno != EAGAIN) die("read");
	}
	
	if (c== '\x1b') {
		char seq[3];
		
		if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
		if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';
		
		if (seq[0] == '[') {
			if(seq[1] >= '0' && seq[1] <= '9') {
				// extended escape - read additional byte
				if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
				if (seq[2] == '~') {
					switch (seq[1]) {
						case '1': return HOME_KEY;
						case '3': return DEL_KEY;
						case '4': return END_KEY;
						case '5': return PAGE_UP;
						case '6': return PAGE_DOWN;
						case '7': return HOME_KEY;
						case '8': return END_KEY;
					}
				}
			} else {
				switch (seq[1]) {
					case 'A': return ARROW_UP;
					case 'B': return ARROW_DOWN;
					case 'C': return ARROW_RIGHT;
					case 'D': return ARROW_LEFT;
					case 'H': return HOME_KEY;
					case 'F': return END_KEY;
				}
			}
		// ESC O sequences
		} else if (seq[0] == 'O') {
			switch (seq[1]) {
				case 'H': return HOME_KEY;
				case 'F': return END_KEY;
			}
		}
		
		return '\x1b';
	} else {
		return c;
	}
}

int getCursorPosition(int *rows, int *cols) {
	char buf[32];
	unsigned int i = 0;
	
	// use this seq to queary the horiz curose position
	if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;
	
	while (i < sizeof(buf)) {
		if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
		if(buf[i] == 'R') break;
		i++;
	}
	buf[i] = 0;
	
	// parse it
	if(buf[0] != '\x1b' || buf[1] != '[') return -1;
	if(sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;
	
	editorReadKey();
	
	return -1;
}

int getWindowSize(int *rows, int *cols) {
	struct winsize ws;
	
	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
		if(write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
		return getCursorPosition(rows, cols);
	} else {
		*cols = ws.ws_col;
		*rows = ws.ws_row;
		return 0;
	}
}

void editorUpdateWindowSize() {
	if (getWindowSize(&E.screen.y, &E.screen.x) == -1) 
		die("getWindowSize");
	E.screen.y -= 2; // one line is for status, another for messages
}

void editorHandleSigwinch() {
	editorUpdateWindowSize();
	if (E.c.x >= E.screen.x) E.c.x = E.screen.x - 1;
	if (E.c.y >= E.screen.y) E.c.y = E.screen.y - 1;
	editorRefreshScreen();
}

void editorHandleSigcont() {
	disableRawMode();
	consoleBufferOpen();
	enableRawMode();
	editorHandleSigwinch();	// in case the screen size changed whilst paused. Also refreshes screen
}

void editorClearScreen() {
	write(STDOUT_FILENO, "\x1b[2J", 4);
	write(STDOUT_FILENO, "\x1b[H", 3);
}

void consoleBufferOpen() {
	// switch to another terminal buffer in order to be able to restore state on exit
	if (write(STDOUT_FILENO, "\x1b[?47h", 6) == -1)
		die("Error changing terminal buffer");
}

void consoleBufferClose() {
	// restore console to the state when opened)
	if (write(STDOUT_FILENO, "\x1b[?91", 5) == -1 ||
	    write(STDOUT_FILENO, "\x1b[?471", 6) == -1)
			die("Error restoring buffer state");
	
	editorClearScreen();
}

/*** syntax highlighting ***/

int is_separator(int c) {
	return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row) {
	row->hl = realloc(row->hl, row->rsize);
	memset(row->hl, HL_NORMAL, row->rsize);
	
	if (E.syntax == NULL) return;
	
	char **keywords = E.syntax->keywords;
	
	char *scs = E.syntax->singleline_comment_start;
	char *mcs = E.syntax->multiline_comment_start;
	char *mce = E.syntax->multiline_comment_end;
	
	int scs_len = scs ? strlen(scs) : 0;
	int mcs_len = scs ? strlen(mcs) : 0;
	int mce_len = scs ? strlen(mce) : 0;
	
	int prev_sep = 1;
	int in_string = 0;
	int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);
	
	int i = 0;
	while (i < row->rsize) {
		char c = row->render[i];
		unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;
		
		if (scs_len && !in_string && !in_comment) {
			if (!strncmp(&row->render[i], scs, scs_len)) {
				memset(&row->hl[i], HL_COMMENT, row->rsize - i);
				break;
			}
		}
		
		if (mcs_len && mce_len && !in_string) {
			if (in_comment) {
				row->hl[i] = HL_MLCOMMENT;
				if (!strncmp(&row->render[i], mce, mce_len)) {
					memset(&row->hl[i], HL_MLCOMMENT, mce_len);
					i += mce_len;
					in_comment = 0;
					prev_sep = 1;
					continue;
				} else {
					i++;
					continue;
				}
			} else if (!strncmp(&row->render[i], mcs, mcs_len)) {
				memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
				i += mcs_len;
				in_comment = 1;
				continue;
			}
		}
		
		if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
			if (in_string) {
				row->hl[i] = HL_STRING;
				if (c == '\\' && i + 1 < row->rsize) {
					row->hl[i + 1] = HL_STRING;
					i += 2;
					continue;
				}
				if (c == in_string) in_string = 0;
				i++;
				prev_sep = 1;
				continue;
			} else {
				if (c == '"' || c == '\'') {
					in_string = c;
					row->hl[i] = HL_STRING;
					i++;
					continue;
				}
			}
		}
		
		if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
			if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
					(c == '.' && prev_hl == HL_NUMBER)) {
				row->hl[i] = HL_NUMBER;
				i++;
				prev_sep = 0;
				continue;
			}
		}
		
		// keywoard highlighting
		if(prev_sep) {		// keywords require a separator before...
			int j;
			for (j=0; keywords[j]; j++) {
				int klen = strlen(keywords[j]);
				int kw2 = keywords[j][klen - 1] == '|';
				if (kw2) klen--;
				
				if (!strncmp(&row->render[i], keywords[j], klen) &&
						is_separator(row->render[i + klen])) { //... and after
					memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
					i += klen;
					break;
				}
			}
			if (keywords[j] != NULL) {
				prev_sep = 0;
				continue;
			}
		}
		
		prev_sep = is_separator(c);
		i++;
	}
	
	// propage syntax change to the next row if there's an open comment state change
	int changed = (row->hl_open_comment != in_comment);
	row->hl_open_comment = in_comment;
	if (changed && row->idx + 1 < E.numrows)
		editorUpdateSyntax(&E.row[row->idx + 1]); // fun recursive call
	
}

int editorSyntaxToColor(int hl) {	// map syntax types to terminal colors
	switch (hl) {
		case HL_COMMENT: 
		case HL_MLCOMMENT: return 36;		// cyan
		case HL_KEYWORD1:  return 33;		// yellow
		case HL_KEYWORD2:  return 32;		// green
		case HL_STRING:    return 35;		// magenta
		case HL_NUMBER:    return 31;		// red
		case HL_MATCH:     return 34;		// blue
		default:           return 37;		// white
	}
}

// select the syntax highlight scheme depending on the filename
void editorSelectSyntaxHighlight() {
	E.syntax = NULL;
	if (E.filename == NULL) return;
	
	for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
		struct editorSyntax *s = &HLDB[j];
		int i = 0;
		while (s->filematch[i]) {
			char *p = strstr(E.filename, s->filematch[i]);
			if (p != NULL) {
				int patlen = strlen(s->filematch[i]);
				// if the pattern started with a . make sure at the end of filename
				// otherwise the match can be anywhere
				if (s->filematch[i][0] != '.' || p[patlen] == '\0') {
					E.syntax = s;	
					
					// update the entire screen to reflect the new highlighting rules
					int filerow;
					for (filerow = 0; filerow < E.numrows; filerow++) {
						editorUpdateSyntax(&E.row[filerow]);
					}
					
					return;
				}
			}
			i++;
		}
	}
}

/*** row operations ***/

int editorRowCxToRx(erow *row, int x) {
	int rx = 0;
	int j;
	for (j = 0; j < x; j++) {
		if (row->chars[j] == '\t')
			rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
		rx++;
	}
	return rx;
}

int editorRowRxToCx(erow *row, int rx) {
	int cur_rx = 0;
	int x;
	for (x = 0; x < row->size; x++) {
		if (row->chars[x] == '\t')
			cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
		cur_rx++;
		
		if (cur_rx > rx) return x;
	}
	return x;
}

void editorUpdateRow(erow *row) {
	int tabs = 0;
	int j;
	for (j = 0; j < row->size; j++) 
		if (row->chars[j] == '\t') tabs++;

	free(row->render);
	row->render = malloc(row->size + tabs*(KILO_TAB_STOP - 1) + 1);
	
	int idx = 0;
	for (j = 0; j < row->size; j++) {
		if (row->chars[j] == '\t') {
			row->render[idx++] = ' ';
			while (idx % KILO_TAB_STOP != 0) row->render[idx++] = ' ';
		} else {
			row->render[idx++] = row->chars[j];
		}
	}	

	row->render[idx] = '\0';
	row->rsize = idx;
	
	editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t len) {
	if (at < 0 || at > E.numrows) return;
	
	E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
	memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));
	for (int j = at + 1; j <= E.numrows; j++) E.row[j].idx++; //fix index no's.
	
	E.row[at].idx = at;
	
	E.row[at].size = len;
	E.row[at].chars = malloc(len + 1);
	memcpy(E.row[at].chars, s, len);
	E.row[at].chars[len] = '\0';
	
	E.row[at].rsize = 0;
	E.row[at].render = NULL;
	E.row[at].hl = NULL;
	E.row[at].hl_open_comment = 0;
	editorUpdateRow(&E.row[at]);
	
	E.numrows++;
	E.dirty++;
}

void editorFreeRow(erow *row) {
	free(row->render);
	free(row->chars);
	free(row->hl);
}

void editorDelRow(int at) {
	if (at < 0 || at >= E.numrows) return;
	editorFreeRow(&E.row[at]);
	memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
	for (int j = at; j < E.numrows - 1; j++)
		E.row[j].idx--; //fix index no's
	
	E.numrows--;
	E.dirty++;
}

void editorRowInsertChar(erow *row, int at, int c) {
	if (at < 0 || at > row->size) at = row->size;
	row->chars = realloc(row->chars, row->size + 2);
	memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
	row->size++;
	row->chars[at] = c;
	editorUpdateRow(row);
	E.dirty++;
}

void editorRowAppendString(erow *row, char *s, size_t len) {
	row->chars = realloc(row->chars, row->size + len + 1);
	memcpy(&row->chars[row->size], s, len);
	row->size += len;
	row->chars[row->size] = '\0';
	editorUpdateRow(row);
	E.dirty++;
}

void editorRowDelChar(erow *row, int at) {
	if (at < 0 || at >= row->size) return;
	memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
	row->size--;
	editorUpdateRow(row);
	E.dirty++;
}

/*** editor operations ***/

void editorInsertChar(int c) {
	if (E.c.y == E.numrows) {
		editorInsertRow(E.numrows, "", 0);
	}
	editorRowInsertChar(&E.row[E.c.y], E.c.x, c);
	E.c.x++;
}

void editorInsertNewLine() {
	if (E.c.x == 0) {
		editorInsertRow(E.c.y, "", 0);
	} else {
		erow *row = &E.row[E.c.y];
		editorInsertRow(E.c.y + 1, &row->chars[E.c.x], row->size - E.c.x);
		row = &E.row[E.c.y];
		row->size = E.c.x;
		row->chars[row->size] = '\0';
		editorUpdateRow(row);
	}
	E.c.y++;
	E.c.x = 0;
}

void editorDelChar() {
	if (E.c.y == E.numrows) return;
	if (E.c.x == 0 && E.c.y == 0) return;
	
	erow *row = &E.row[E.c.y];
	if (E.c.x > 0) {
		editorRowDelChar(row, E.c.x - 1);
		E.c.x--;
	} else {
		E.c.x = E.row[E.c.y - 1].size;
		editorRowAppendString(&E.row[E.c.y - 1], row->chars, row->size);
		editorDelRow(E.c.y);
		E.c.y--;
	}
}

/*** file i/o ***/

char *editorRowsToString(int *buflen) {
	int totlen = 0;
	int j;
	for (j = 0; j < E.numrows; j++)
		totlen += E.row[j].size + 1;
	*buflen = totlen;
	
	char *buf = malloc(totlen);
	char *p = buf;
	for (j = 0; j < E.numrows; j++) {
		memcpy(p, E.row[j].chars, E.row[j].size);
		p += E.row[j].size;
		*p = '\n';
		p++;
	}
	
	return buf;
}

void editorOpen(char *filename) {
	free(E.filename);
	E.filename = strdup(filename);
	
	editorSelectSyntaxHighlight();
	
	FILE *fp = fopen(filename, "r");
	if (!fp) die("fopen");
	
	char *line = NULL;
	size_t linecap = 0;
	ssize_t linelen;
	while ((linelen = getline(&line, &linecap, fp)) != -1) {
		if (linelen > 0 && (line[linelen - 1] == '\n' || line[linelen - 1] == '\r'))
			linelen--;
		editorInsertRow(E.numrows, line, linelen);
	}
	free(line);
	fclose(fp);
	E.dirty = 0;

}

void editorSave() {
	if (E.filename == NULL) {
		E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
		if (E.filename == NULL) {
			editorSetStatusMessage("Save aborted");
			return;
		}
		editorSelectSyntaxHighlight();
	}
	
	int len;
	char *buf = editorRowsToString(&len);
	
	int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
	if (fd != -1) {
		if (ftruncate(fd, len) != -1) {
			if (write(fd, buf, len) == len) {
				close(fd);
				free(buf);
				E.dirty = 0;
				editorSetStatusMessage("%d bytes written to disk", len);
				return;
			}
		}
		close(fd);
	}
	
	free(buf);
	editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
}

/*** find ***/

void editorFindCallback(char *query, int key) {
	static coord last_match = { -1, 0 }; // posn of last match. x =-1 if no match
	static char *saved_hl = NULL;	// use to restore line to state prior to highlighting
	
	// remove any previous search highlighting (if any)
	if (saved_hl) {
		memcpy(E.row[last_match.y].hl, saved_hl, E.row[last_match.y].rsize);
		free(saved_hl);
		saved_hl = NULL;
	}

	if (key== '\r' || key == '\x1b') {		// finish search; restore flag
		last_match.x = -1;
		return;
	}
	
	int direction;			// which direction to search
	if (key == ARROW_RIGHT || key == ARROW_DOWN) {
		direction = 1;
	} else if (key == ARROW_LEFT || key == ARROW_UP) {
		direction = -1;
	} else {
		last_match.x = -1;	// anything else means it's a new search
		direction = 1;
	}
	
	// start the search from the current cursor position, or one past the last search
	if (last_match.x <= -1) {
		last_match.y = E.c.y;
		last_match.x = E.c.x;
	} else {
		last_match.x += direction;	// Must wrap to previous line if < 0. Should add a util to do wrapping
		if (last_match.x < 0) { // can only occur if going backwards
			last_match.y--;			//wot if going past the top of the screen?? 
			last_match.x = E.row[last_match.y].size;
		}
	}
	
	for (int i = 0; i < E.numrows; i++) {
		
		erow *row = &E.row[last_match.y];
		char *match;
		if (direction  > 0) {
			match = strstr(&row->render[last_match.x], query);
		} else {
			match = revstrstr(&row->render[0], &row->render[last_match.x], query);
		}
		if (match) {
			// yay found a match - reposition the cursor
			last_match.x = editorRowRxToCx(row, match - row->render);
			E.c = last_match;
			
			// reposition the viewport, if needed
			int new_offset = (E.c.y - E.offset.y) + 1;
			if ( (new_offset > E.screen.y) || (new_offset < 0) ) {
				E.offset.y = E.c.y - E.numrows/2; // reposition to middle of screen
				if (E.offset.y < 0) E.offset.y = 0;
			}
			
			// highlight the match, having first saved the old highlighting
			saved_hl = malloc(row->rsize);
			memcpy(saved_hl, row->hl, row->rsize);
			memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
			break;
		}

		last_match.y += direction;
		if (last_match.y <= -1) last_match.y = E.numrows - 1;	// wrap from bot to top
		if (last_match.y >= E.numrows) last_match.y = 0;	// wrap from top to bot	
		if (direction > 0) {
			last_match.x = 0; 	// start at the beginning of the next line, or end if going up (TODO)
		} else {
			last_match.x = E.row[last_match.y].size;
		}
	}
}	

void editorFind() {
	coord saved_c = E.c;
	coord saved_offset = E.offset;
	char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)", editorFindCallback);
	
	if (query) {
		free(query);
	} else {
		E.c = saved_c;
		E.offset = saved_offset;
	}
}

/*** append buffer ***/

struct abuf {
	char *b;
	int len;
};

#define ABUF_INIT {NULL, 0}

void abAppend(struct abuf *ab, const char *s, int len) {
	char *new = realloc(ab->b, ab->len + len);
	
	if (new == NULL) return;
	memcpy(&new[ab->len], s, len);
	ab->b = new;
	ab->len += len;
}

void abFree(struct abuf *ab) {
	free(ab->b);
}

/*** output ***/

void editorScroll() {
	E.rx = 0;
	if (E.c.y < E.numrows) {
		E.rx = editorRowCxToRx(&E.row[E.c.y], E.c.x);
	}
	
	if (E.c.y < E.offset.y) {
		E.offset.y = E.c.y;
	}
	if (E.c.y >= E.offset.y + E.screen.y) {
		E.offset.y = E.c.y - E.screen.y + 1;
	}
	if (E.rx < E.offset.x) {
		E.offset.x = E.rx;
	}
	if (E.rx >= E.offset.x + E.screen.x) {
		E.offset.x = E.rx - E.screen.x + 1;
	}
}

void editorDrawRows(struct abuf *ab) {
	int y;
	for (y=0; y < E.screen.y; y++) {
		int filerow = y + E.offset.y;
		
		if (filerow >= E.numrows) {
			if (E.numrows == 0 && y == E.screen.y / 3) {
				char welcome[80];
				int welcomelen = snprintf(welcome, sizeof(welcome),
				  "Kilo editor -- version %s", KILO_VERSION);
				if (welcomelen > E.screen.x) welcomelen = E.screen.x;
				int padding = (E.screen.x - welcomelen) / 2;
				if (padding) {
					abAppend(ab, "~", 1);
					padding--;
				}
				while (padding--) abAppend(ab, " ", 1);
				abAppend(ab, welcome, welcomelen);
			} else {
				abAppend( ab, "~", 1);
			}
		} else {
			int len = E.row[filerow].rsize - E.offset.x;
			if (len < 0) len = 0;
			if (len > E.screen.x) len = E.screen.x;
			char *c = &E.row[filerow].render[E.offset.x];
			unsigned char *hl = &E.row[filerow].hl[E.offset.x];
			int current_color = -1;
			int j;
			for (j = 0; j < len; j++) {
				if (iscntrl(c[j])) {
					char sym = (c[j] <= 26) ? '@' + c[j] : '?';
					abAppend(ab, "\x1b[7m", 4);
					abAppend(ab, &sym, 1);
					abAppend(ab, "\x1b[m", 3);
					if (current_color != -1) {
						char buf[16];
						int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
						abAppend(ab, buf, clen);
					}
				} else if (hl[j] == HL_NORMAL) {
					if (current_color != -1) {
						abAppend(ab, "\x1b[39m", 5);
						current_color = -1;
					}
					abAppend(ab, &c[j], 1);
				} else {
					int color = editorSyntaxToColor(hl[j]);
					if (color != current_color) {
						current_color = color;
						char buf[16];
						int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
						abAppend(ab, buf, clen);
					}
					abAppend(ab, &c[j], 1);
				}
			}
			abAppend(ab, "\x1b[39m", 5);
		}
		
		abAppend(ab, "\x1b[K", 3);
		abAppend(ab, "\r\n", 2);

	}
	
}

void editorDrawStatusBar(struct abuf *ab) {
	abAppend(ab, "\x1b[7m", 4);
	char status[80], rstatus[80];
	int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
		E.filename ? E.filename : "[No Name]", E.numrows,
		E.dirty ? "(modified)" : "");
	int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d",
		E.syntax ? E.syntax->filetype : "no ft", E.c.y + 1, E.numrows);
	if (len > E.screen.x) len = E.screen.x;
	abAppend(ab, status, len);
	while (len < E.screen.x) {
		if (E.screen.x - len == rlen) {
			abAppend(ab, rstatus, rlen);
			break;
		} else {
			abAppend(ab, " ", 1);
			len++;
		}
	}
	abAppend(ab, "\x1b[m", 3);
	abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct abuf *ab) {
	abAppend(ab, "\x1b[K", 3);
	int msglen = strlen(E.statusmsg);
	if (msglen > E.screen.x) msglen = E.screen.x;
	if (msglen && time(NULL) - E.statusmsg_time < 5)
		abAppend(ab, E.statusmsg, msglen);
}

void editorRefreshScreen() {
	editorScroll();
	
	struct abuf ab = ABUF_INIT;
	
	abAppend( &ab, "\x1b[?25l", 6);		// hide cursor
	abAppend( &ab, "\x1b[H", 3);			// go home
	
	editorDrawRows(&ab);
	editorDrawStatusBar(&ab);
	editorDrawMessageBar(&ab);
	
	// put cursor at its current position
	char buf[32];
	snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.c.y - E.offset.y) + 1, 
												(E.rx - E.offset.x) + 1);
	abAppend(&ab, buf, strlen(buf));
	abAppend( &ab, "\x1b[?25h", 6);	// show cursor
	write(STDOUT_FILENO, ab.b, ab.len);
	abFree(&ab);
}

void editorSetStatusMessage(const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
	va_end(ap);
	E.statusmsg_time = time(NULL);
}

/*** input ***/

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
	size_t bufsize = 128;
	char *buf = malloc(bufsize);
	
	size_t buflen = 0;
	buf[0] = '\0';
	
	while (1) {
		editorSetStatusMessage(prompt, buf);
		editorRefreshScreen();
		
		int c = editorReadKey();
		if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
			if (buflen != 0) buf[--buflen] = '\0';
		} else if (c == '\x1b') {
			editorSetStatusMessage("");
			if (callback) callback(buf, c);
			free(buf);
			return NULL;
		} else if (c == '\r') {
			if (buflen != 0) {
				editorSetStatusMessage("");
				if (callback) callback(buf, c);
				return buf;
			}
		} else if (!iscntrl(c) && c < 128) {
			if (buflen == bufsize - 1) {
				bufsize *= 2;
				buf = realloc(buf, bufsize);
			}
			buf[buflen++] = c;
			buf[buflen] = '\0';
		}
		
		if (callback) callback(buf, c);
	}
}

void editorMoveCursor(int key) {
	erow *row = (E.c.y >= E.numrows) ? NULL : &E.row[E.c.y];
	
	switch (key) {
		case ARROW_LEFT:
			if (E.c.x != 0) {
				E.c.x--;
			} else if (E.c.y > 0) {
				E.c.y--;
				E.c.x = E.row[E.c.y].size;
			}
			break;
		case ARROW_RIGHT:
			if (row && E.c.x < row->size) {
				E.c.x++;
			} else if (row && E.c.x == row->size) {
				E.c.y++;
				E.c.x = 0;
			}
			break;
		case ARROW_UP:
			if (E.c.y != 0) {
				E.c.y--;
			}
			break;
		case ARROW_DOWN:
			if (E.c.y < E.numrows) {
				E.c.y++;
			}
			break;
	}
	
	row = (E.c.y >= E.numrows) ? NULL : &E.row[E.c.y];
	int rowlen = row ? row->size : 0;
	if (E.c.x > rowlen) {
		E.c.x = rowlen;
	}
}

void editorProcessKeypress() {
	static int quit_times = KILO_QUIT_TIMES;
	
	int c = editorReadKey();
	
	switch (c) {
		case '\r':
			editorInsertNewLine();
			break;
			
		case CTRL_KEY('q'):
			if (E.dirty && quit_times > 0) {
				editorSetStatusMessage("WARNING!!! File has unsaved changes. "
					"Press Ctrl-Q %d more times to quit.", quit_times);
				quit_times--;
				return;
			}
			//write(STDOUT_FILENO, "\x1b[2J", 4);
			//write(STDOUT_FILENO, "\x1b[H", 3);
			editorClearScreen();
			exit(0);
			break;
			
		case CTRL_KEY('s'):
			editorSave();
			break;
			
		case HOME_KEY:
			E.c.x = 0;
			break;
			
		case END_KEY:
			if (E.c.y < E.numrows)
				E.c.x = E.row[E.c.y].size;
			break;
		
		case CTRL_KEY('f'):
			editorFind();
			break;
			
		case BACKSPACE:
		case CTRL_KEY('h'):
		case DEL_KEY:
			if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
			editorDelChar();
			break;
			
		case PAGE_UP:
		case PAGE_DOWN:
			{
				if (c == PAGE_UP) {
					E.c.y = E.offset.y;
				} else if (c == PAGE_DOWN) {
					E.c.y = E.offset.y + E.screen.y - 1;
				}
				
				int times = E.screen.y;
				while(times--)
					editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
			}
			break;
		
		case ARROW_UP:
		case ARROW_DOWN:
		case ARROW_LEFT:
		case ARROW_RIGHT:
			editorMoveCursor(c);
			break;
		
		case CTRL_KEY('l'):
		case '\x1b':
			break;
			
		case CTRL_KEY('p'):
			consoleBufferClose();
			kill(0, SIGTSTP);
			break;
		
		default:
			editorInsertChar(c);
			break;
	}
	
	quit_times = KILO_QUIT_TIMES;
}


/*** init ***/

void initEditor() {
	E.c.x = 0;
	E.c.y = 0;
	E.rx = 0;
	E.offset.x = 0;
	E.offset.y = 0;
	E.numrows = 0;
	E.row = NULL;
	E.dirty = 0;
	E.filename = NULL;
	E.statusmsg[0] = '\0';
	E.statusmsg_time = 0;
	E.syntax = NULL;
	
	editorUpdateWindowSize();
	
	signal(SIGWINCH, editorHandleSigwinch); // signal sent when terminal changes size
	signal(SIGCONT, editorHandleSigcont);  // signal to restart when previously paused

}

int main(int argc, char *argv[]) {
	enableRawMode();
	initEditor();
	if (argc >= 2) {
		editorOpen(argv[1]);
	}
	debug("Processed args");
	debug("this works too %i - amazing! and a string too %s", 10, argv[0]);
	editorSetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");
	
	while(1) {
		editorRefreshScreen();
		editorProcessKeypress();
	}
	
    return 0;
}
