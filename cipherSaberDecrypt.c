#include<stdio.h>
#include<stdlib.h>
#include<string.h>

int main(int argc, char** argv)
{
	FILE *fp;
	int keyLen = 0;
	unsigned long messageLen = 0;

	unsigned char* message;
	unsigned char* key;
	unsigned char state[256];
	unsigned char keyEnd[10];

	int byte = 0;
	unsigned char i = 0;
	unsigned char j = 0;
	unsigned char c = 0;

	// Handle arguments
	if( argc != 4) 
	{
		fprintf(stderr, "Usage: %s key inputFile outputFile\n", argv[0]);
		return -1;
	}
	key =(unsigned char *)malloc(strlen(argv[1]) + 10 + 1);
	memcpy(key, argv[1], strlen(argv[1]) + 1);
	
	// Open file
	fp=fopen(argv[2], "rb");
	if( !fp ) 
	{
		fprintf(stderr, "File Not Found: %s\n", argv[2]);
		return -1;
	}
	fseek(fp, 0, SEEK_END);
	messageLen=ftell(fp) - 10;
	rewind(fp);
	fread((key + strlen(argv[1])), 10, 1, fp);
	message =(unsigned char *)malloc(messageLen+1);
	fread(message, messageLen, 1, fp);
	fclose(fp);
	keyLen = strlen(argv[1]) + 10;

	// Setup
	for( byte = 0; byte < 256; byte ++ ) state[byte] = byte;

	// Mix
	for( byte = 0; byte < 256; byte ++ )
	{
		j += state[i] + key[ i % keyLen ];
		c = state[i];
		state[i] = state[j];
		state[j] = c;
		i ++;
	}
	i = 0;
	j = 0;

	// Cipher
	for(byte = 0; byte < messageLen; byte ++)
	{
		i ++;
		j += state[i];
		c = state[i];
		state[i] = state[j];
		state[j] = c;
		c += state[i];
		message[byte] ^= state[c];
	}

	// Write output
	fp=fopen(argv[3], "wb");
	fwrite(message, messageLen, 1, fp);
	fclose(fp);
}
