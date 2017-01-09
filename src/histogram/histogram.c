#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

struct ByteCount
{
	long count;
	int byte;
};

static int compare_longs(const void *left, const void *right)
{
	const struct ByteCount *leftVal = (struct ByteCount *) left;
	const struct ByteCount *rightVal = (struct ByteCount *) right;
	
	if (leftVal->count < rightVal->count)
		return -1;
	if (leftVal->count > rightVal->count)
		return 1;
	return 0;
}

static void print_largest_indexes(long byteCount[], int maxCount)
{
	struct ByteCount countArray[256];
	int i;
	
	for (i = 0; i < 256; ++i)
	{
		countArray[i].byte = i;
		countArray[i].count = byteCount[i];
	}

	/* for (i = 0; i < 256; ++i) */
	/* 	printf("before: %d => %ld\n", countArray[i].byte, countArray[i].count); */
	qsort(countArray, sizeof(countArray) / sizeof(countArray[0]), sizeof(struct ByteCount), compare_longs);
	/* for (i = 0; i < 256; ++i) */
	/* 	printf("after: %d => %ld\n", countArray[i].byte, countArray[i].count); */
	
	for (i = 0; i < maxCount; ++i)
	{
		int index = 255 - i;
		printf("%d => %ld\n", countArray[index].byte, countArray[index].count);
		
	}
}

int main(int argc, char **argv)
{
	int i;

	for (i = 1; i < argc; ++i)
	{
		char *filename = argv[i];
		long byteCount[256];
		unsigned char buffer[65536];
		FILE *in;
		
		memset(byteCount, 0, sizeof(byteCount));

		in = fopen(filename, "r");
		if (!in)
		{
			printf("Could not open '%s'\n", filename);
			continue;
		}
		for (;;)
		{
			size_t cnt = fread(buffer, 1, sizeof(buffer), in);
			size_t j;

			for (j = 0; j < cnt; ++j)
				++byteCount[buffer[j]];

			if (cnt != sizeof(buffer))
				break;
		}
		fclose(in);
		
		print_largest_indexes(byteCount, 3);
	}

	return 0;
}

