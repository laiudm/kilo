# kilo

The kilo tiny terminal based linux text editor written in C. It's minimal, and had no
dependencies, including curses.

The source originated from [antirez](http://antirez.com) and was the basis of a tutorial by [Jeremy Ruten](http://viewsourcecode.org/snaptoken/kilo/)

Additional tweak (pause) was taken from (https://github.com/GrenderG/tte)

## Installation

### Compiling

make

### Usage

kilo [filename]


### My extensions

Improved searching:
* search from current cursor position
* search forward on the same line (doesn't search back on the same line)
* only reposition the scroll position if needed

Added foreground/background mode:
* type ^P (pause)
* type fg at the command line to restore. (can alter the screen size in between)