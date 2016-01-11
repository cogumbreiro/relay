
#define BITS_PER_LONG 32
/* 2^31 + 2^29 - 2^25 + 2^22 - 2^19 - 2^16 + 1 */
#define GOLDEN_RATIO_PRIME 0x9e370001UL

static inline unsigned long hash_long(unsigned long val, unsigned int bits)
{
	unsigned long hash = val;

	/* On some cpus multiply is faster, on others gcc will do shifts */
	hash *= GOLDEN_RATIO_PRIME;

	/* High bits are more random, so use them. */
	return hash >> (BITS_PER_LONG - bits);
}


static inline unsigned long hash_ptr(void *ptr, unsigned int bits)
{
	return hash_long((unsigned long)ptr, bits);
}
