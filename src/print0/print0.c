#include <stdio.h>
#include <memory.h>

typedef struct State
{
	char buf[1024];
	int read;
	int start;
	int cp;
	FILE *in;
	FILE *out;
	const char *name;
} State;

#define do_or_return(f) do { int ret; if (!!(ret = (f))) return ret; } while (0)

static int return_error(State *state, const char *message, int ret)
{
	fprintf(stderr, "%s: %s\n", state->name, message);
	return ret;
}

static int read_chunk(State *state)
{
	state->read = fread(state->buf, 1, sizeof(state->buf), state->in);
	if (!state->read)
	{
		if (ferror(state->in))
			return return_error(state, "couldn't read input", 1);
		return 1;
	}
	state->cp = 0;
	state->start = 0;
	return 0;
}

static int write_chunk(State *state)
{
	if (state->cp <= state->start || state->start >= state->read)
		return 0;
	int written = fwrite(&state->buf[state->start], 1, state->cp - state->start, state->out);
	if (!written)
		return return_error(state, "couldn't write to output", 2);
	state->start = state->cp;
	return 0;
}

static int write_null(State *state)
{
	if (fputc(0, state->out) == EOF)
		return return_error(state, "couldn't write null to output", 3);
	state->start = state->cp;
	return 0;
}

typedef enum Token
{
	TK_EOF,
	TK_NEWLINE,
	TK_TEXT
} Token;

static Token scan(State *state)
{
	if (state->cp >= state->read)
	{
		return TK_EOF;
	}
	
	state->start = state->cp;
	switch (state->buf[state->cp++])
	{
		case '\n':
			if (state->cp < state->read && state->buf[state->cp] == '\r')
				++state->cp;
			return TK_NEWLINE;
			
		case '\r':
			if (state->cp < state->read && state->buf[state->cp] == '\n')
				++state->cp;
			return TK_NEWLINE;
		
		default:
			while (state->cp < state->read)
			{
				switch (state->buf[state->cp++])
				{
					default:
						continue;
						
					case '\n':
					case '\r':
						--state->cp;
						break;
				}
				break;
			}
			return TK_TEXT;
	}
}

static int translate(const char *name, FILE *in, FILE *out)
{
	State state;
	
	state.in = in;
	state.out = out;
	state.name = name;
	
	for (;;)
	{
		do_or_return(read_chunk(&state));
		for (;;)
		{
			switch (scan(&state))
			{
				case TK_EOF:
					break;
				
				case TK_NEWLINE:
					do_or_return(write_null(&state));
					continue;
				
				case TK_TEXT:
					do_or_return(write_chunk(&state));
					continue;
			}
			break;
		}
	}
}

int main(int argc, char **argv)
{
	return translate(argv[0], stdin, stdout);
}
