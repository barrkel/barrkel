#define WINVER 0x0602
#define _WIN32_WINNT 0x0602

#include <string.h>
#include <initguid.h>
#include <windows.h>
#include <stdio.h>
#include <objbase.h>
#include <shobjidl.h>

using namespace std;

static char *program;

static int fail(const char *message)
{
	printf("%s: %s\n", program, message);
	exit(1);
}

wchar_t *fake_convert(char *source)
{
	wchar_t *result = (wchar_t *) malloc(strlen(source) * 2 + 2);
	wchar_t *dest = result;
	for (; *source; ++dest, ++source)
		*dest = (wchar_t) *source;
	*dest = 0;
	return result;
}

int set_wallpaper(IDesktopWallpaper *wp, char *bmp_path, int index)
{
	HRESULT hr;

	LPWSTR monitorID = 0;
	hr = wp->GetMonitorDevicePathAt((UINT) index, &monitorID);
	if (!SUCCEEDED(hr)) fail("failed on GetMonitorDevicePathAt");

	hr = wp->SetWallpaper(monitorID, fake_convert(bmp_path));
	if (!SUCCEEDED(hr)) fail("failed on SetWallpaper");

	return 0;
}

int main(int argc, char **argv)
{
	char *bmp;
	BOOL ret;
	
	program = *argv++;
	--argc;
	
	if (argc < 1)
		fail("missing argument");
		
	HRESULT hr = CoInitialize(0);
	if (!SUCCEEDED(hr)) fail("couldn't initialize");

	IDesktopWallpaper *wp = 0;
	hr = CoCreateInstance(CLSID_DesktopWallpaper, 0, CLSCTX_ALL, IID_IDesktopWallpaper,
		(void **) &wp);
	if (!SUCCEEDED(hr)) fail("failed on CoCreateInstance");
	if (!wp) fail("wp is null");

	for (int index = 0; index < argc; ++index)
		set_wallpaper(wp, *argv++, index);

	DESKTOP_WALLPAPER_POSITION pos;
	hr = wp->GetPosition(&pos);
	if (!SUCCEEDED(hr)) fail("failed on GetPosition");
	if (pos != DWPOS_TILE)
	{
		hr = wp->SetPosition(DWPOS_TILE);
		if (!SUCCEEDED(hr)) fail("failed on SetPosition");
	}

	return 0;
}
	

