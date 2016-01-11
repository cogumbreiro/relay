
#include "cpqphp_pci.h"
#include "dnotify.h"
#include "oid_mgt.h"
#include "pci_i386.h"
#include "ieee80211_rx.h"
#include "reassembly.h"
#include "sleep.h"
#include "aio.h"
#include <pthread.h>
#include <string.h>



int main (int argc, char *argv[]) {

  pthread_t temp_t;
  int i;
  char *blah = "hello world\n";


  pthread_create (&temp_t, (void *)0, &event_thread, (void*) 0);

  i = strlen (blah);

  callDNotifyStuff ();
  i = ieee80211_parse_info_param((struct ieee80211_info_element *) 0, 13,
                                 (struct ieee80211_network *)0);

  mgt_le_to_cpu(0, (void *)0);

  mgt_cpu_to_le(0, (void *)0);

  pcibios_allocate_bus_resources ((struct list_head *)0);

  ip6qhashfn ( 10, (struct in6_addr *) 0,  (struct in6_addr *) 0);

  init_low_mapping ((pgd_t *)0, 0);

  ioctx_alloc(10);

  return 0;

}
