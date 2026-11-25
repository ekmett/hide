/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
#include "accessibility-menu.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

static char *copy_text(const char *text,size_t max_bytes,size_t *budget) {
    if (!text) return NULL;
    size_t length=0;
    while (length<=max_bytes && text[length]) {
        unsigned char c=(unsigned char)text[length];
        if (c<32 || c==127) return NULL;
        ++length;
    }
    if (length>max_bytes || length>*budget) return NULL;
    char *value=malloc(length+1);
    if (!value) return NULL;
    memcpy(value,text,length+1);*budget-=length;return value;
}
void thc_ax_menu_snapshot_free(thc_ax_menu_snapshot *value) {
    if (!value) return;
    for (size_t i=0;i<value->count;++i) {
        free((void *)value->nodes[i].id);
        free((void *)value->nodes[i].name);
        free((void *)value->nodes[i].shortcut);
    }
    free((void *)value->nodes);free(value);
}
thc_ax_menu_snapshot *thc_ax_menu_snapshot_clone(const thc_ax_menu_snapshot *source) {
    if (!source || source->count>THC_AX_MENU_MAX_NODES ||
        !isfinite(source->width) || !isfinite(source->height) ||
        source->width<=0 || source->height<=0 ||
        (!source->present && source->count) || (source->count && !source->nodes)) return NULL;
    thc_ax_menu_snapshot *value=calloc(1,sizeof(*value));
    if (!value) return NULL;
    value->present=!!source->present;value->width=source->width;value->height=source->height;
    thc_ax_menu_node *nodes=calloc(source->count?source->count:1,sizeof(*nodes));
    if (!nodes) { free(value);return NULL; }
    value->nodes=nodes;value->count=source->count;
    size_t budget=THC_AX_MENU_MAX_TEXT_BYTES;int current=0;
    for (size_t i=0;i<source->count;++i) {
        const thc_ax_menu_node *node=&source->nodes[i];
        thc_ax_rect r=node->bounds;
        if (node->token<2 || node->role<THC_AX_MENU_ROOT || node->role>THC_AX_MENU_SEPARATOR ||
            !isfinite(r.x) || !isfinite(r.y) || !isfinite(r.width) || !isfinite(r.height) ||
            r.x<0 || r.y<0 || r.width<=0 || r.height<=0 || r.x+r.width>source->width || r.y+r.height>source->height ||
            (i==0 ? node->parent!=-1 || node->role!=THC_AX_MENU_ROOT || node->current || !node->enabled : node->parent!=0 || node->role==THC_AX_MENU_ROOT) ||
            (node->role==THC_AX_MENU_RADIO ? node->checked<0 || node->checked>1 : node->checked!=-1) ||
            (node->role==THC_AX_MENU_SEPARATOR && (node->enabled || node->current)) ||
            (current+=!!node->current)>1) goto invalid;
        if (i) {
            thc_ax_rect parent=source->nodes[0].bounds;
            if (r.x<parent.x || r.y<parent.y || r.x+r.width>parent.x+parent.width || r.y+r.height>parent.y+parent.height) goto invalid;
        }
        nodes[i]=*node;nodes[i].id=NULL;nodes[i].name=NULL;nodes[i].shortcut=NULL;
        nodes[i].id=copy_text(node->id,1024,&budget);
        nodes[i].name=copy_text(node->name,1024,&budget);
        nodes[i].shortcut=copy_text(node->shortcut,256,&budget);
        if (!nodes[i].id || !nodes[i].name || !nodes[i].shortcut || !nodes[i].id[0]) goto invalid;
        for (size_t j=0;j<i;++j) if (nodes[j].token==node->token || !strcmp(nodes[j].id,node->id)) goto invalid;
    }
    return value;
invalid:
    thc_ax_menu_snapshot_free(value);return NULL;
}

/* SDL-thread staging. Readers only see platform-owned copies after end. */
static thc_ax_menu_snapshot *pending_menu,*live_menu;
static uint64_t next_menu_token=2;
static size_t pending_capacity;
static size_t pending_text_budget;
static double last_width=1,last_height=1;
static int publish_menu(const thc_ax_menu_snapshot *value) {
#ifdef WITH_ACCESSKIT
    return thc_ax_platform_menu_publish(value);
#else
    (void)value;return 1;
#endif
}
void thc_ax_menu_clear(void) {
    thc_ax_menu_snapshot_free(pending_menu);pending_menu=NULL;pending_capacity=0;pending_text_budget=THC_AX_MENU_MAX_TEXT_BYTES;
    thc_ax_menu_snapshot_free(live_menu);live_menu=NULL;
    thc_ax_menu_snapshot empty={0,last_width,last_height,0,NULL};
    (void)publish_menu(&empty);
}
int thc_ax_menu_begin(int present,double width,double height) {
    thc_ax_menu_snapshot_free(pending_menu);pending_menu=NULL;pending_capacity=0;pending_text_budget=THC_AX_MENU_MAX_TEXT_BYTES;
    if (!isfinite(width) || !isfinite(height) || width<=0 || height<=0) { thc_ax_menu_clear();return 0; }
    pending_menu=calloc(1,sizeof(*pending_menu));
    if (!pending_menu) { thc_ax_menu_clear();return 0; }
    pending_menu->present=!!present;pending_menu->width=last_width=width;pending_menu->height=last_height=height;
    return 1;
}
int thc_ax_menu_item(const char *id,int parent,int role,const char *name,const char *shortcut,
                    double x,double y,double width,double height,int enabled,int current,int checked) {
    if (!pending_menu) { thc_ax_menu_clear();return 0; }
    if (!pending_menu->present || pending_menu->count==THC_AX_MENU_MAX_NODES) { thc_ax_menu_clear();return 0; }
    if (pending_menu->count==pending_capacity) {
        size_t capacity=pending_capacity?pending_capacity*2:16;
        void *nodes=realloc((void *)pending_menu->nodes,capacity*sizeof(thc_ax_menu_node));
        if (!nodes) { thc_ax_menu_clear();return 0; }
        pending_menu->nodes=nodes;pending_capacity=capacity;
    }
    thc_ax_menu_node *node=(thc_ax_menu_node *)pending_menu->nodes+pending_menu->count++;
    memset(node,0,sizeof(*node));
    node->id=copy_text(id,1024,&pending_text_budget);node->name=copy_text(name,1024,&pending_text_budget);node->shortcut=copy_text(shortcut,256,&pending_text_budget);
    node->parent=parent;node->role=(thc_ax_menu_role)role;node->bounds=(thc_ax_rect){x,y,width,height};
    node->enabled=!!enabled;node->current=!!current;node->checked=checked;
    if (!node->id || !node->name || !node->shortcut) { thc_ax_menu_clear();return 0; }
    return 1;
}
int thc_ax_menu_end(void) {
    if (!pending_menu) { thc_ax_menu_clear();return 0; }
    if (!pending_menu->present && !pending_menu->count && live_menu && !live_menu->present && !live_menu->count &&
        pending_menu->width==live_menu->width && pending_menu->height==live_menu->height) {
        thc_ax_menu_snapshot_free(pending_menu);pending_menu=NULL;pending_capacity=0;return 1;
    }
    for (size_t i=0;i<pending_menu->count;++i) {
        thc_ax_menu_node *node=(thc_ax_menu_node *)pending_menu->nodes+i;
        if (live_menu) for (size_t j=0;j<live_menu->count;++j) {
            const thc_ax_menu_node *old=live_menu->nodes+j;
            if (node->role==old->role && !strcmp(node->id,old->id)) { node->token=old->token;break; }
        }
        if (!node->token) {
            if (next_menu_token==UINT64_MAX) { thc_ax_menu_clear();return 0; }
            node->token=next_menu_token++;
        }
    }
    thc_ax_menu_snapshot *validated=thc_ax_menu_snapshot_clone(pending_menu);
    thc_ax_menu_snapshot_free(pending_menu);pending_menu=NULL;pending_capacity=0;
    if (!validated) { thc_ax_menu_clear();return 0; }
    thc_ax_menu_snapshot_free(live_menu);live_menu=validated;
    if (!publish_menu(live_menu)) { thc_ax_menu_clear();return 0; }
    return 1;
}
