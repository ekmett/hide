/* SPDX-FileCopyrightText: 2026 Edward Kmett
 * SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 */
#include "accessibility-accesskit.h"

static accesskit_role node_role(thc_ax_menu_role role) {
    switch (role) {
    case THC_AX_MENU_ROOT: return ACCESSKIT_ROLE_MENU;
    case THC_AX_MENU_RADIO: return ACCESSKIT_ROLE_MENU_ITEM_RADIO;
    default: return ACCESSKIT_ROLE_MENU_ITEM;
    }
}
accesskit_tree_update *thc_ax_menu_accesskit_tree(const thc_ax_menu_snapshot *s) {
    accesskit_tree_update *update=accesskit_tree_update_with_capacity_and_focus(s?s->count+1:1,1);
    accesskit_tree_info *info=accesskit_tree_info_new(1);
    accesskit_tree_info_set_toolkit_name(info,"hide");
    accesskit_tree_update_set_tree_info(update,info);
    accesskit_node *root=accesskit_node_new(ACCESSKIT_ROLE_WINDOW);
    accesskit_node_set_label(root,"Hide");
    accesskit_node_set_read_only(root);
    if (s) for (size_t i=0;i<s->count;i++) {
        const thc_ax_menu_node *r=&s->nodes[i];
        if (r->role==THC_AX_MENU_SEPARATOR) continue;
        if (r->parent<0) accesskit_node_push_child(root,r->token);
        accesskit_node *node=accesskit_node_new(node_role(r->role));
        accesskit_node_set_label(node,r->name);
        accesskit_node_set_read_only(node);
        if (*r->shortcut) accesskit_node_set_keyboard_shortcut(node,r->shortcut);
        if (!r->enabled) accesskit_node_set_disabled(node);
        accesskit_node_set_selected(node,r->current!=0);
        if (r->checked>=0) accesskit_node_set_toggled(node,r->checked?ACCESSKIT_TOGGLED_TRUE:ACCESSKIT_TOGGLED_FALSE);
        accesskit_rect rect={r->bounds.x,r->bounds.y,r->bounds.x+r->bounds.width,r->bounds.y+r->bounds.height};
        accesskit_node_set_bounds(node,rect);
        /* The shared boundary validates one menu root and immediate children. */
        if (r->role==THC_AX_MENU_ROOT) for (size_t j=1;j<s->count;j++)
            if (s->nodes[j].role!=THC_AX_MENU_SEPARATOR)
                accesskit_node_push_child(node,s->nodes[j].token);
        accesskit_tree_update_push_node(update,r->token,node);
    }
    accesskit_tree_update_push_node(update,1,root);
    return update;
}
