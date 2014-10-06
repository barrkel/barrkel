#include <windows.h>
#include <stdio.h>

static char *program;

static int fail(char *message)
{
	printf("%s: %s\n", program, message);
	exit(1);
}

int main(int argc, char **argv)
{
	char *bmp;
	BOOL ret;
	
	program = *argv++;
	
	if (argc != 2)
		fail("missing argument");

	bmp = *argv;

	ret = SystemParametersInfo(
		SPI_SETDESKWALLPAPER,
		0,
		bmp,
		SPIF_SENDCHANGE);
	if (!ret)
		fail("SPI call failed");

	return 0;
}
	
