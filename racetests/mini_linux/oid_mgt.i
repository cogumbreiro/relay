# 1 "oid_mgt.c"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "oid_mgt.c"
# 15 "oid_mgt.c"
struct obj_ssid {
 unsigned char length;
 char octets[33];
} __attribute__ ((packed));

struct obj_key {
 unsigned char type;
 unsigned char length;
 char key[32];
} __attribute__ ((packed));

struct obj_mlme {
 unsigned char address[6];
 unsigned short id;
 unsigned short state;
 unsigned short code;
} __attribute__ ((packed));

struct obj_mlmeex {
 unsigned char address[6];
 unsigned short id;
 unsigned short state;
 unsigned short code;
 unsigned short size;
 unsigned char data[0];
} __attribute__ ((packed));

struct obj_buffer {
 unsigned long size;
 unsigned long addr;
} __attribute__ ((packed));

struct obj_bss {
 unsigned char address[6];
 int:16;

 char state;
 char reserved;
 short age;

 char quality;
 char rssi;

 struct obj_ssid ssid;
 short channel;
 char beacon_period;
 char dtim_period;
 short capinfo;
 short rates;
 short basic_rates;
 int:16;
} __attribute__ ((packed));

struct obj_bsslist {
 unsigned long nr;
 struct obj_bss bsslist[0];
} __attribute__ ((packed));

struct obj_frequencies {
 unsigned short nr;
 unsigned short mhz[0];
} __attribute__ ((packed));

struct obj_attachment {
 char type;
 char reserved;
 short id;
 short size;
 char data[0];
} __attribute__((packed));
# 114 "oid_mgt.c"
void
mgt_le_to_cpu(int type, void *data)
{
 switch (type) {
 case 0x01:
  *(unsigned long *) data = __le32_to_cpu(*(unsigned long *) data);
  break;
 case 0x04:{
   struct obj_buffer *buff = data;
   buff->size = __le32_to_cpu(buff->size);
   buff->addr = __le32_to_cpu(buff->addr);
   break;
  }
 case 0x05:{
   struct obj_bss *bss = data;
   bss->age = le16_to_cpu(bss->age);
   bss->channel = le16_to_cpu(bss->channel);
   bss->capinfo = le16_to_cpu(bss->capinfo);
   bss->rates = le16_to_cpu(bss->rates);
   bss->basic_rates = le16_to_cpu(bss->basic_rates);
   break;
  }
 case 0x06:{
   struct obj_bsslist *list = data;
   int i;
   list->nr = __le32_to_cpu(list->nr);
   for (i = 0; i < list->nr; i++)
    mgt_le_to_cpu(0x05, &list->bsslist[i]);
   break;
  }
 case 0x07:{
   struct obj_frequencies *freq = data;
   int i;
   freq->nr = le16_to_cpu(freq->nr);
   for (i = 0; i < freq->nr; i++)
    freq->mhz[i] = le16_to_cpu(freq->mhz[i]);
   break;
  }
 case 0x08:{
   struct obj_mlme *mlme = data;
   mlme->id = le16_to_cpu(mlme->id);
   mlme->state = le16_to_cpu(mlme->state);
   mlme->code = le16_to_cpu(mlme->code);
   break;
  }
 case 0x09:{
   struct obj_mlmeex *mlme = data;
   mlme->id = le16_to_cpu(mlme->id);
   mlme->state = le16_to_cpu(mlme->state);
   mlme->code = le16_to_cpu(mlme->code);
   mlme->size = le16_to_cpu(mlme->size);
   break;
  }
 case 0x0C:{
   struct obj_attachment *attach = data;
   attach->id = le16_to_cpu(attach->id);
   attach->size = le16_to_cpu(attach->size);;
   break;
 }
 case 0x02:
 case 0x03:
 case 0x0A:
 case 0x0B:
  break;
 default:
  BUG();
 }
}

static void
mgt_cpu_to_le(int type, void *data)
{
 switch (type) {
 case 0x01:
  *(unsigned long *) data = __cpu_to_le32(*(unsigned long *) data);
  break;
 case 0x04:{
   struct obj_buffer *buff = data;
   buff->size = __cpu_to_le32(buff->size);
   buff->addr = __cpu_to_le32(buff->addr);
   break;
  }
 case 0x05:{
   struct obj_bss *bss = data;
   bss->age = cpu_to_le16(bss->age);
   bss->channel = cpu_to_le16(bss->channel);
   bss->capinfo = cpu_to_le16(bss->capinfo);
   bss->rates = cpu_to_le16(bss->rates);
   bss->basic_rates = cpu_to_le16(bss->basic_rates);
   break;
  }
 case 0x06:{
   struct obj_bsslist *list = data;
   int i;
   list->nr = __cpu_to_le32(list->nr);
   for (i = 0; i < list->nr; i++)
    mgt_cpu_to_le(0x05, &list->bsslist[i]);
   break;
  }
 case 0x07:{
   struct obj_frequencies *freq = data;
   int i;
   freq->nr = cpu_to_le16(freq->nr);
   for (i = 0; i < freq->nr; i++)
    freq->mhz[i] = cpu_to_le16(freq->mhz[i]);
   break;
  }
 case 0x08:{
   struct obj_mlme *mlme = data;
   mlme->id = cpu_to_le16(mlme->id);
   mlme->state = cpu_to_le16(mlme->state);
   mlme->code = cpu_to_le16(mlme->code);
   break;
  }
 case 0x09:{
   struct obj_mlmeex *mlme = data;
   mlme->id = cpu_to_le16(mlme->id);
   mlme->state = cpu_to_le16(mlme->state);
   mlme->code = cpu_to_le16(mlme->code);
   mlme->size = cpu_to_le16(mlme->size);
   break;
  }
 case 0x0C:{
   struct obj_attachment *attach = data;
   attach->id = cpu_to_le16(attach->id);
   attach->size = cpu_to_le16(attach->size);;
   break;
 }
 case 0x02:
 case 0x03:
 case 0x0A:
 case 0x0B:
  break;
 default:
  BUG();
 }
}


int main (int argc, char *argv[]) {

  mgt_le_to_cpu(0, (void *)0);

  mgt_cpu_to_le(0, (void *)0);

  return 0;

}
