
#include <string.h>


//// Infinite loop w/out path sensitivity (but field sensitivity)...


#define u32 unsigned long
#define u16 unsigned short
#define u8 unsigned char


#define cpu_to_le32 __cpu_to_le32
#define le32_to_cpu __le32_to_cpu

////


struct obj_ssid {
	u8 length;
	char octets[33];
} __attribute__ ((packed));

struct obj_key {
	u8 type;		/* dot11_priv_t */
	u8 length;
	char key[32];
} __attribute__ ((packed));

struct obj_mlme {
	u8 address[6];
	u16 id;
	u16 state;
	u16 code;
} __attribute__ ((packed));

struct obj_mlmeex {
	u8 address[6];
	u16 id;
	u16 state;
	u16 code;
	u16 size;
	u8 data[0];
} __attribute__ ((packed));

struct obj_buffer {
	u32 size;
	u32 addr;		/* 32bit bus address */
} __attribute__ ((packed));

struct obj_bss {
	u8 address[6];
	int:16;			/* padding */

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
	int:16;			/* padding */
} __attribute__ ((packed));

struct obj_bsslist {
	u32 nr;
	struct obj_bss bsslist[0];
} __attribute__ ((packed));

struct obj_frequencies {
	u16 nr;
	u16 mhz[0];
} __attribute__ ((packed));

struct obj_attachment {
	char type;
	char reserved;
	short id;
	short size;
	char data[0];
} __attribute__((packed));


#define OID_FLAG_CACHED		0x80
#define OID_FLAG_TYPE		0x7f

#define OID_TYPE_U32		0x01
#define OID_TYPE_SSID		0x02
#define OID_TYPE_KEY		0x03
#define OID_TYPE_BUFFER		0x04
#define OID_TYPE_BSS		0x05
#define OID_TYPE_BSSLIST	0x06
#define OID_TYPE_FREQUENCIES	0x07
#define OID_TYPE_MLME		0x08
#define OID_TYPE_MLMEEX		0x09
#define OID_TYPE_ADDR		0x0A
#define OID_TYPE_RAW		0x0B
#define OID_TYPE_ATTACH		0x0C


////


//// problem when data = &list0->bsslist[i]  and type OID_TYPE_BSS, 
//// then later on
//// list1 = data if type = OID_TYPE_BSSLIST, so
//// list1 = &list0->bsslist[i]
//// list1->bsslist[i] = list0->bsslist[i].bsslist[i]
//// ... and so on even though the value of type doesn't match!

void mgt_le_to_cpu (int type, void *data);

void mgt_cpu_to_le(int type, void *data);


void
mgt_le_to_cpu(int type, void *data)
{
	dupe ();

	switch (type) {
	case OID_TYPE_U32:
		*(u32 *) data = le32_to_cpu(*(u32 *) data);
		break;
	case OID_TYPE_BUFFER:{
			struct obj_buffer *buff = data;
			buff->size = le32_to_cpu(buff->size);
			buff->addr = le32_to_cpu(buff->addr);
			break;
		}
	case OID_TYPE_BSS:{
			struct obj_bss *bss = data;
			bss->age = le16_to_cpu(bss->age);
			bss->channel = le16_to_cpu(bss->channel);
			bss->capinfo = le16_to_cpu(bss->capinfo);
			bss->rates = le16_to_cpu(bss->rates);
			bss->basic_rates = le16_to_cpu(bss->basic_rates);
			break;
		}
	case OID_TYPE_BSSLIST:{
			struct obj_bsslist *list = data;
			int i;
			list->nr = le32_to_cpu(list->nr);
			for (i = 0; i < list->nr; i++)
				mgt_le_to_cpu(OID_TYPE_BSS, &list->bsslist[i]);
			break;
		}
	case OID_TYPE_FREQUENCIES:{
			struct obj_frequencies *freq = data;
			int i;
			freq->nr = le16_to_cpu(freq->nr);
			for (i = 0; i < freq->nr; i++)
				freq->mhz[i] = le16_to_cpu(freq->mhz[i]);
			break;
		}
	case OID_TYPE_MLME:{
			struct obj_mlme *mlme = data;
			mlme->id = le16_to_cpu(mlme->id);
			mlme->state = le16_to_cpu(mlme->state);
			mlme->code = le16_to_cpu(mlme->code);
			break;
		}
	case OID_TYPE_MLMEEX:{
			struct obj_mlmeex *mlme = data;
			mlme->id = le16_to_cpu(mlme->id);
			mlme->state = le16_to_cpu(mlme->state);
			mlme->code = le16_to_cpu(mlme->code);
			mlme->size = le16_to_cpu(mlme->size);
			break;
		}
	case OID_TYPE_ATTACH:{
			struct obj_attachment *attach = data;
			attach->id = le16_to_cpu(attach->id);
			attach->size = le16_to_cpu(attach->size);; 
			break;
	}
	case OID_TYPE_SSID:
	case OID_TYPE_KEY:
	case OID_TYPE_ADDR:
	case OID_TYPE_RAW:
		break;
	default:
		BUG();
	}
}


void
mgt_cpu_to_le(int type, void *data)
{
	dupe ();

	switch (type) {
	case OID_TYPE_U32:
		*(u32 *) data = cpu_to_le32(*(u32 *) data);
		break;
	case OID_TYPE_BUFFER:{
			struct obj_buffer *buff = data;
			buff->size = cpu_to_le32(buff->size);
			buff->addr = cpu_to_le32(buff->addr);
			break;
		}
	case OID_TYPE_BSS:{
			struct obj_bss *bss = data;
			bss->age = cpu_to_le16(bss->age);
			bss->channel = cpu_to_le16(bss->channel);
			bss->capinfo = cpu_to_le16(bss->capinfo);
			bss->rates = cpu_to_le16(bss->rates);
			bss->basic_rates = cpu_to_le16(bss->basic_rates);
			break;
		}
	case OID_TYPE_BSSLIST:{
			struct obj_bsslist *list = data;
			int i;
			list->nr = cpu_to_le32(list->nr);
			for (i = 0; i < list->nr; i++)
				mgt_cpu_to_le(OID_TYPE_BSS, &list->bsslist[i]);
			break;
		}
	case OID_TYPE_FREQUENCIES:{
			struct obj_frequencies *freq = data;
			int i;
			freq->nr = cpu_to_le16(freq->nr);
			for (i = 0; i < freq->nr; i++)
				freq->mhz[i] = cpu_to_le16(freq->mhz[i]);
			break;
		}
	case OID_TYPE_MLME:{
			struct obj_mlme *mlme = data;
			mlme->id = cpu_to_le16(mlme->id);
			mlme->state = cpu_to_le16(mlme->state);
			mlme->code = cpu_to_le16(mlme->code);
			break;
		}
	case OID_TYPE_MLMEEX:{
			struct obj_mlmeex *mlme = data;
			mlme->id = cpu_to_le16(mlme->id);
			mlme->state = cpu_to_le16(mlme->state);
			mlme->code = cpu_to_le16(mlme->code);
			mlme->size = cpu_to_le16(mlme->size);
			break;
		}
	case OID_TYPE_ATTACH:{
			struct obj_attachment *attach = data;
			attach->id = cpu_to_le16(attach->id);
			attach->size = cpu_to_le16(attach->size);; 
			break;
	}
	case OID_TYPE_SSID:
	case OID_TYPE_KEY:
	case OID_TYPE_ADDR:
	case OID_TYPE_RAW:
		break;
	default:
		BUG();
	}
}

