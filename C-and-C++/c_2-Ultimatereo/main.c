#include "return_codes.h"
#include <malloc.h>
#include <stdbool.h>
#include <stdio.h>
#if defined(ZLIB)
	#include <zlib.h>
#elif defined(LIBDEFLATE)
	#include <libdeflate.h>
#elif defined(ISAL)
	#include <include/igzip_lib.h>
#else
	#error "Defined library is not supported"
#endif
static void dump(const unsigned char *mas, size_t n)
{
	for (size_t i = 0; i < n; i++)
	{
		printf("%02X ", mas[i]);
	}
	printf("\n");
}
static bool isEqual(const unsigned char *mas1, const unsigned char *mas2, size_t n)
{
	size_t eq = 0;
	for (size_t i = 0; i < n; i++)
	{
		if (mas1[i] == mas2[i])
		{
			eq++;
		}
	}
	return eq == n;
}

static int readNextUInt(unsigned int *answer, unsigned char *buffer, FILE *in)
{
	size_t n = fread(buffer, 1, 4, in);
	if (n != 4)
	{
		fprintf(stderr, "It was impossible to read integer!");
		return ERROR_UNKNOWN;
	}
	*answer = buffer[3] + (buffer[2] << 8) + (buffer[1] << 16) + (buffer[0] << 24);
	return 0;
}
static int readNextUChar(unsigned char *answer, FILE *in)
{
	size_t n = fread(answer, 1, 1, in);
	if (n != 1)
	{
		fprintf(stderr, "It was impossible to read char!");
		return ERROR_UNKNOWN;
	}
	return 0;
}

static int skipChunks(FILE *in, unsigned char *buffer, int check, unsigned int *lengthOfChunk, const unsigned char *chunk, bool logs)
{
	while (true)
	{
		if (logs)
		{
			printf("%zu\n", ftell(in));
		}
		check = readNextUInt(lengthOfChunk, buffer, in);
		if (check != 0)
		{
			return check;
		}
		if (logs)
		{
			printf("length of chunk: %u\n", (*lengthOfChunk));
		}
		size_t result = fread(buffer, 1, 4, in);
		if (result != 4)
		{
			fprintf(stderr, "It was impossible to read magic numbers!");
			return ERROR_UNKNOWN;
		}
		if (!isEqual(buffer, chunk, 4))
		{
			fseek(in, (*lengthOfChunk) + 4, SEEK_CUR);
		}
		else
		{
			break;
		}
	}
	return 0;
}
int getLengthOfSourceData(FILE *in, unsigned char *buffer, int check, unsigned int *lengthOfChunk, size_t *lengthOfSourceData, const unsigned char *chunk, bool logs)
{
	fseek(in, *lengthOfChunk + 4, SEEK_CUR);
	long long offset = *lengthOfChunk + 4;
	while (true)
	{
		if (logs)
		{
			printf("%zu\n", ftell(in));
		}
		check = readNextUInt(lengthOfChunk, buffer, in);
		if (check != 0)
		{
			return check;
		}
		if (logs)
		{
			printf("length of chunk: %u\n", (*lengthOfChunk));
		}
		size_t result = fread(buffer, 1, 4, in);
		if (result != 4)
		{
			fprintf(stderr, "It was impossible to read magic numbers!");
			return ERROR_UNKNOWN;
		}
		if (isEqual(buffer, chunk, 4))
		{
			*lengthOfSourceData += *lengthOfChunk;
			offset += (*lengthOfChunk) + 12;
			fseek(in, (*lengthOfChunk) + 4, SEEK_CUR);
		}
		else
		{
			break;
		}
	}
	fseek(in, -offset - 16, SEEK_CUR);
	return 0;
}
int fillSourceData(FILE *in, unsigned char *buffer, int check, unsigned int *lengthOfChunk, unsigned char *sourceData, const unsigned char *chunk, bool logs)
{
	size_t index = 0;
	while (true)
	{
		if (logs)
		{
			printf("%zu\n", ftell(in));
		}
		check = readNextUInt(lengthOfChunk, buffer, in);
		if (check != 0)
		{
			return check;
		}
		if (logs)
		{
			printf("length of chunk: %u\n", (*lengthOfChunk));
		}
		size_t result = fread(buffer, 1, 4, in);
		if (result != 4)
		{
			fprintf(stderr, "It was impossible to read magic numbers!");
			return ERROR_UNKNOWN;
		}
		if (isEqual(buffer, chunk, 4))
		{
			for (unsigned int i = 0; i < *lengthOfChunk; i++)
			{
				check = readNextUChar(&sourceData[index + i], in);
				if (check != 0)
				{
					return check;
				}
			}
			index += *lengthOfChunk;
			fseek(in, 4, SEEK_CUR);
		}
		else
		{
			break;
		}
	}
	fseek(in, -8, SEEK_CUR);
	return 0;
}
unsigned char paethPredictor(unsigned char a, unsigned char b, unsigned char c)
{
	int p = a + b - c;
	int pa = p - a > 0 ? p - a : a - p;
	int pb = p - b > 0 ? p - b : b - p;
	int pc = p - c > 0 ? p - c : c - p;
	if (pa <= pb && pa <= pc)
	{
		return a;
	}
	else if (pb <= pc)
	{
		return b;
	}
	else
	{
		return c;
	}
}
int main(int argc, char *argv[])
{
	bool logs = false;
	const unsigned char MAGIC_NUMBERS[] = { 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A };
	const unsigned char IHDR_CHUNK_TYPE[] = { 'I', 'H', 'D', 'R' };
	const unsigned char IDAT_CHUNK_TYPE[] = { 'I', 'D', 'A', 'T' };
	const unsigned char IEND_CHUNK_TYPE[] = { 'I', 'E', 'N', 'D' };
	if (logs)
	{
		dump(IHDR_CHUNK_TYPE, 4);
	}
	if (argc != 3)
	{
		fprintf(stderr, "Invalid number of parameters! Expected: 3, got: %d", argc);
	}

	FILE *in = fopen(argv[1], "rb");
	if (!in)
	{
		fprintf(stderr, "Input file not found");
		return ERROR_FILE_NOT_FOUND;
	}
	unsigned char buffer[8];
	size_t result = fread(buffer, 1, 8, in);
	if (result != 8)
	{
		fclose(in);
		fprintf(stderr, "It was impossible to read magic numbers!");
		return ERROR_UNKNOWN;
	}
	if (logs)
	{
		dump(buffer, 8);
	}
	if (!isEqual(buffer, MAGIC_NUMBERS, 8))
	{
		fclose(in);
		fprintf(stderr, "Input file is not PNG!");
		return ERROR_INVALID_DATA;
	}
	int check;
	unsigned int lengthOfChunk = 0, width = 0, height = 0;
	unsigned char bitDepth = 0, colorType = 0, compressionMethod = 0, filterMethod = 0, interlaceMethod = 0;
	check = readNextUInt(&lengthOfChunk, buffer, in);
	if (check != 0)
	{
		fclose(in);
		return check;
	}
	if (lengthOfChunk != 13)
	{
		fclose(in);
		fprintf(stderr, "IHDR length must be 13 bytes but instead it's %d bytes", lengthOfChunk);
		return ERROR_INVALID_DATA;
	}
	result = fread(buffer, 1, 4, in);
	if (result != 4)
	{
		fclose(in);
		fprintf(stderr, "It was impossible to read magic numbers!");
		return ERROR_UNKNOWN;
	}
	if (!isEqual(buffer, IHDR_CHUNK_TYPE, 4))
	{
		fclose(in);
		fprintf(stderr, "IHDR chunk supposed to be first of all chunks!");
		return ERROR_INVALID_DATA;
	}
	check = readNextUInt(&width, buffer, in);
	if (check != 0)
	{
		fclose(in);
		return check;
	}
	check = readNextUInt(&height, buffer, in);
	if (check != 0)
	{
		fclose(in);
		return check;
	}
	check = readNextUChar(&bitDepth, in);
	if (check != 0)
	{
		fclose(in);
		return check;
	}
	check = readNextUChar(&colorType, in);
	if (check != 0)
	{
		fclose(in);
		return check;
	}
	check = readNextUChar(&compressionMethod, in);
	if (check != 0)
	{
		fclose(in);
		return check;
	}
	check = readNextUChar(&filterMethod, in);
	if (check != 0)
	{
		fclose(in);
		return check;
	}
	check = readNextUChar(&interlaceMethod, in);
	if (check != 0)
	{
		fclose(in);
		return check;
	}
	if (logs)
	{
		printf(
			"width: %d\nheight: %d\n"
			"bit depth: %d\ncolor type: %d\ncompression method: %d\n"
			"filter method: %d\ninterlace method: %d\n",
			width,
			height,
			bitDepth,
			colorType,
			compressionMethod,
			filterMethod,
			interlaceMethod);
	}
	if ((colorType != 0 && colorType != 2) || bitDepth != 8 || filterMethod != 0 || compressionMethod != 0)
	{
		fclose(in);
		fprintf(
			stderr,
			"PNG file with color type %d, bit depth %d, filter method %d and compression method %d is not "
			"supported!",
			colorType,
			bitDepth,
			filterMethod,
			compressionMethod);
		return ERROR_CALL_NOT_IMPLEMENTED;
	}
	fseek(in, 4, SEEK_CUR);	   // skip crc
	check = skipChunks(in, buffer, check, &lengthOfChunk, IDAT_CHUNK_TYPE, logs);
	if (check != 0)
	{
		fclose(in);
		return check;
	}
	size_t lengthOfSourceData = lengthOfChunk;
	if (logs)
	{
		printf("BEFORE %zu\n", ftell(in));
	}
	check = getLengthOfSourceData(in, buffer, check, &lengthOfChunk, &lengthOfSourceData, IDAT_CHUNK_TYPE, logs);
	if (check != 0)
	{
		fclose(in);
		return check;
	}
	if (logs)
	{
		printf("SourceData length is %zu bytes\n", lengthOfSourceData);
		printf("AFTER %zu\n", ftell(in));
	}
	// Go through all IDATs to get length of source data and then uncompress everything
	// Here I should work with IDAT and then skip to IEND
	// Try writing a code with one IDAT

	unsigned char *sourceData = malloc(sizeof(char) * lengthOfSourceData);
	if (sourceData == NULL)
	{
		fclose(in);
		fprintf(stderr, "It was impossible to allocate memory during processing of the code!");
		return ERROR_NOT_ENOUGH_MEMORY;
	}
	check = fillSourceData(in, buffer, check, &lengthOfChunk, sourceData, IDAT_CHUNK_TYPE, logs);
	if (check != 0)
	{
		fclose(in);
		free(sourceData);
		return check;
	}
	if (logs)
	{
		dump(sourceData, 100);
	}
	int channelsPerPixel = colorType == 0 ? 1 : 3;
	unsigned long destLen = height * (width * channelsPerPixel + 1);
	unsigned char *destData = malloc(sizeof(unsigned char) * destLen);
	if (destData == NULL)
	{
		fclose(in);
		free(sourceData);
		fprintf(stderr, "It was impossible to allocate memory during processing of the code!");
		return ERROR_NOT_ENOUGH_MEMORY;
	}
#ifdef ZLIB
	check = uncompress(destData, &destLen, sourceData, lengthOfSourceData);
#endif
#ifdef LIBDEFLATE
	check = libdeflate_zlib_decompress(libdeflate_alloc_decompressor(), sourceData, lengthOfSourceData, destData, destLen, NULL);
#endif
#ifdef ISAL
	struct inflate_state uncompress;
	isal_inflate_init(&uncompress);
	uncompress.next_in = sourceData;
	uncompress.avail_in = lengthOfSourceData;
	uncompress.next_out = destData;
	uncompress.avail_out = destLen;
	uncompress.crc_flag = IGZIP_ZLIB;
	isal_inflate(&uncompress);
#endif
	free(sourceData);
	if (check != 0)
	{
		fclose(in);
		free(destData);
		fprintf(stderr, "It was impossible to uncompress the compressed data!");
		return check;
	}
	if (logs)
	{
		printf("First string\n");
		dump(destData, width * channelsPerPixel + 1);
		printf("Second string\n");
		dump(&destData[channelsPerPixel * width + 1], width * channelsPerPixel + 1);
	}

	//	uLongf *destLen = NULL;
	//	uncompress(data, destLen, (const Bytef *)in, lengthOfChunk);
	//	dump(data, sizeof(data));
	// skipping to iend
	check = skipChunks(in, buffer, check, &lengthOfChunk, IEND_CHUNK_TYPE, logs);
	if (check != 0)
	{
		fclose(in);
		free(destData);
		return check;
	}
	unsigned int lastCRC = 0;
	check = readNextUInt(&lastCRC, buffer, in);
	if (check != 0)
	{
		fclose(in);
		free(destData);
		return check;
	}
	size_t size = fread(buffer, 1, 1, in);
	if (lengthOfChunk != 0 || size != 0)
	{
		fclose(in);
		free(destData);
		fprintf(stderr, "There is no ending for the file where it's supposed to be!");
		return ERROR_INVALID_DATA;
	}
	fclose(in);
	unsigned char *answer = malloc(sizeof(unsigned char) * width * height * channelsPerPixel);
	if (answer == NULL)
	{
		free(destData);
		fprintf(stderr, "It was impossible to allocate memory during processing of the code!");
		return ERROR_NOT_ENOUGH_MEMORY;
	}
	unsigned char filter;
	for (size_t i = 0; i < height; i++)
	{
		filter = destData[i * (width * channelsPerPixel + 1)];
		for (size_t j = 0; j < width * channelsPerPixel; j++)
		{
			if (filter == 0)
			{
				answer[i * (width * channelsPerPixel) + j] = destData[i * (width * channelsPerPixel + 1) + j + 1];
			}
			else if (filter == 1)
			{
				unsigned char left = j < channelsPerPixel ? 0 : answer[i * (width * channelsPerPixel) + j - channelsPerPixel];
				answer[i * (width * channelsPerPixel) + j] = destData[i * (width * channelsPerPixel + 1) + j + 1] + left;
			}
			else if (filter == 2)
			{
				unsigned char up = i == 0 ? 0 : answer[(i - 1) * (width * channelsPerPixel) + j];
				answer[i * (width * channelsPerPixel) + j] = destData[i * (width * channelsPerPixel + 1) + j + 1] + up;
			}
			else if (filter == 3)
			{
				unsigned char left = j < channelsPerPixel ? 0 : answer[i * (width * channelsPerPixel) + j - channelsPerPixel];
				unsigned char up = i == 0 ? 0 : answer[(i - 1) * (width * channelsPerPixel) + j];
				answer[i * (width * channelsPerPixel) + j] =
					destData[i * (width * channelsPerPixel + 1) + j + 1] + ((int)left + (int)up) / 2;
			}
			else if (filter == 4)
			{
				unsigned char left = j < channelsPerPixel ? 0 : answer[i * (width * channelsPerPixel) + j - channelsPerPixel];
				unsigned char up = i == 0 ? 0 : answer[(i - 1) * (width * channelsPerPixel) + j];
				unsigned char upperLeft =
					(i == 0 || j < channelsPerPixel) ? 0 : answer[(i - 1) * (width * channelsPerPixel) + j - channelsPerPixel];
				answer[i * (width * channelsPerPixel) + j] =
					destData[i * (width * channelsPerPixel + 1) + j + 1] + paethPredictor(left, up, upperLeft);
			}
			else
			{
				free(destData);
				free(answer);
				fprintf(stderr, "Unsupported filter type %d!", filter);
				return ERROR_UNKNOWN;
			}
		}
	}
	if (logs)
	{
		printf("First string\n");
		dump(answer, width * channelsPerPixel);
		printf("Second string\n");
		dump(&answer[channelsPerPixel * width], width * channelsPerPixel);
	}
	FILE *out = fopen(argv[2], "wb");
	if (!out)
	{
		free(destData);
		fprintf(stderr, "It was impossible to open output file.");
		return ERROR_FILE_NOT_FOUND;
	}
	if (colorType == 2)
	{
		fprintf(out, "P6\n%d %d\n255\n", width, height);
	}
	else
	{
		fprintf(out, "P5\n%d %d\n255\n", width, height);
	}
	for (size_t i = 0; i < height; i++)
	{
		for (size_t j = 0; j < width * channelsPerPixel; j++)
		{
			size_t elements_written = fwrite(&answer[i * (width * channelsPerPixel) + j], 1, 1, out);
			if (elements_written != 1)
			{
				free(destData);
				free(answer);
				fclose(out);
				fprintf(stderr, "It was impossible to write into the file during the process of writing.");
				return ERROR_UNKNOWN;
			}
		}
	}
	free(destData);
	free(answer);
	fclose(out);
	return ERROR_SUCCESS;
}