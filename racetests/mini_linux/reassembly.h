

#ifndef _REASSEMBLY___FOO_BAR_
#define _REASSEMBLY___FOO_BAR_

#include "ipv6.h"

unsigned int ip6qhashfn(u32 id, struct in6_addr *saddr,
                        struct in6_addr *daddr);


#endif
