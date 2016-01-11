#include "types.h"
#include "ipv6.h"



//from /include/linux/jhash.h

#define __jhash_mix(a, b, c) \
{ \
  a -= b; a -= c; a ^= (c>>13); \
  b -= c; b -= a; b ^= (a<<8); \
  c -= a; c -= b; c ^= (b>>13); \
  a -= b; a -= c; a ^= (c>>12);  \
  b -= c; b -= a; b ^= (a<<16); \
  c -= a; c -= b; c ^= (b>>5); \
  a -= b; a -= c; a ^= (c>>3);  \
  b -= c; b -= a; b ^= (a<<10); \
  c -= a; c -= b; c ^= (b>>15); \
}

/* The golden ration: an arbitrary value */
#define JHASH_GOLDEN_RATIO	0x9e3779b9



//from /net/ipv6/reassembly.c


static u32 ip6_frag_hash_rnd;

#define IP6Q_HASHSZ	64

unsigned int ip6qhashfn(u32 id, struct in6_addr *saddr,
			       struct in6_addr *daddr)
{
	u32 a, b, c;

	a = saddr->s6_addr32[0];
	b = saddr->s6_addr32[1];
	c = saddr->s6_addr32[2];

	a += JHASH_GOLDEN_RATIO;
	b += JHASH_GOLDEN_RATIO;
	c += ip6_frag_hash_rnd;
	__jhash_mix(a, b, c);

	a += saddr->s6_addr32[3];
	b += daddr->s6_addr32[0];
	c += daddr->s6_addr32[1];
	__jhash_mix(a, b, c);

	a += daddr->s6_addr32[2];
	b += daddr->s6_addr32[3];
	c += id;
	__jhash_mix(a, b, c);

	return c & (IP6Q_HASHSZ - 1);
}


