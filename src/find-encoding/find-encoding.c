#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static void usage(const char *program)
{
	printf("usage: %s <file>...\n", program);
	printf("Scans text in <file> and outputs best guess for encoding:\n");
	printf("  ascii	   - no high characters used at all\n");
	printf("  utf8	   - all high characters seem like valid utf8 encodings\n");
	printf("  windows  - probably high characters in 0x80-0x9F are used\n");
	printf("  iso8859  - no 0x80-0x9F, but 0xA4 (euro) used\n");
	printf("  unknown  - conflicting sequences found\n");
	printf("All these indications are probabilistic, except for ASCII.\n");
	exit(1);
}

const int ISO_EURO = 0xA4;

typedef unsigned char uchar;

typedef struct {
	int invalid_windows_count;
	int invalid_utf8_count;
	int invalid_iso_count;
	int valid_windows_count;
	int valid_utf8_count;
	int valid_iso_count;
	int hi_count;
	int euro_count;
	int curr_utf8;
} CurrentGuess;

typedef struct {
	// last read character
	int ch;
	FILE *f;
	int cur;
	int end;
	uchar buf[512];
} FileBuffer;

static int nextch(FileBuffer *fb)
{
	if (fb->cur >= fb->end)
	{
		int res = fread(fb->buf, 1, sizeof(fb->buf), fb->f);
		if (res <= 0)
			return fb->ch = -1;
		fb->cur = 0;
		fb->end = res;
	}
	return fb->ch = fb->buf[fb->cur++];
}

static int utf8_len(int ch)
{
	ch >>= 4;
	ch &= 7;
	return ch == 4 ? 1 : ch - 4;
}

static int is_utf8_part(int ch)
{
	return (ch & 0xC0) == 0x80;
}

static int is_utf8_start(int ch)
{
	return (ch & 0xC0) == 0xC0;
}

static void scan_hi_bytes(FileBuffer *fb, CurrentGuess *guess)
{
	++guess->hi_count;

	if (fb->ch >= 0x80 && fb->ch <= 0x9F)
		++guess->invalid_iso_count;
	else
		++guess->valid_iso_count;

	switch (fb->ch)
	{
	case 0x81:
	case 0x8D:
	case 0x8F:
	case 0x90:
	case 0x9D:
		++guess->invalid_windows_count;
		break;
	default:
		++guess->valid_windows_count;
		break;
	}

	if (is_utf8_start(fb->ch))
	{
		if (guess->curr_utf8 != 0)
		{
			++guess->invalid_utf8_count;
			guess->curr_utf8 = 0;
		}
		else
		{
			// give a high rating to valid utf8 sequences, as they are less likely
			guess->curr_utf8 = utf8_len(fb->ch);
			++guess->valid_utf8_count;
		}
	}

	if (is_utf8_part(fb->ch))
	{
		if (guess->curr_utf8 == 0)
		{
			++guess->invalid_utf8_count;
		}
		else
		{
			++guess->valid_utf8_count;
			--guess->curr_utf8;
			/* if (guess->utf8 == 0) */
			/* 	++guess->utf8Count; */
		}
	}

	if (fb->ch == ISO_EURO)
	{
		++guess->euro_count;
	}
}

static void print_guess(CurrentGuess *guess)
{
	if (guess->hi_count == 0)
	{
		printf("ascii\n");
		return;
	}
	
	// if it could be utf8, trust that it is
	if (guess->valid_utf8_count > 0 && guess->invalid_utf8_count == 0)
	{
		printf("utf8\n");
		return;
	}

	// could be windows or iso
	if (guess->valid_windows_count > 0 && guess->valid_iso_count > 0 && guess->invalid_windows_count == 0
		&& guess->invalid_iso_count == 0)
	{
		if (guess->euro_count > 0)
			printf("iso8859-15 (probably, due to euro)\n");
		else
			printf("windows-1252 (or maybe iso)\n");
		return;
	}
	
	if (guess->valid_windows_count > 0 && guess->invalid_windows_count == 0 && guess->invalid_iso_count > 0)
	{
		printf("windows-1252\n");
		return;
	}

	if (guess->valid_iso_count > 0 && guess->invalid_iso_count == 0 && guess->invalid_windows_count > 0)
	{
		// this case isn't actually possible, as all iso characters are valid windows characters
		printf("iso8859-15\n");
		return;
	}

	printf("unknown\n");	
}

static void detect(const char *filename)
{
	FileBuffer fb;
	CurrentGuess guess;

	memset(&guess, 0, sizeof(guess));

	fb.cur = fb.end = 0;
	fb.f = fopen(filename, "r");
	if (!fb.f)
	{
		printf("couldn't open %s\n", filename);
		return;
	}
	while (nextch(&fb) >= 0)
	{
		// printf("%.2x ", fb.ch);
		if (fb.ch >= 128)
			scan_hi_bytes(&fb, &guess);
		else if (guess.curr_utf8 > 0)
		{
			guess.curr_utf8 = 0;
			++guess.invalid_utf8_count;
		}
		// print_guess(&guess);
	}
	fclose(fb.f);
	
	printf("%s: ", filename);
	print_guess(&guess);
}

int main(int argc, char **argv)
{
	int i;

	if (argc <= 1)
		usage(argv[0]);

	for (i = 1; i < argc; ++i)
		detect(argv[i]);
	return 0;
}


