/* SPDX-License-Identifier: BSD-3-Clause */
/* The SDL window stays hidden and never becomes key or ordered. */
#import <Cocoa/Cocoa.h>
#include <SDL3/SDL.h>
#include "../cbits/window.h"
#include "../cbits/accessibility.h"
#include <assert.h>
#include <math.h>
#include <stdio.h>

static NSDictionary *node(NSArray *ident,NSArray *parent,NSString *role,NSString *name,id rectangle,id expanded,int index,int level) {
    return @{@"id":ident,@"parent":parent ?: (id)NSNull.null,@"role":role,@"name":name,@"bounds":rectangle,
        @"selected":index==1?@YES:@NO,@"focused":index==1?@YES:@NO,@"expanded":expanded,@"loading":@NO,@"moreChildren":@NO,
        @"childrenKnown":@1,@"generation":@1,@"level":@(level),@"posInSet":@(level?1:0),@"setSize":@1,
        @"index":index<0?(id)NSNull.null:@(index)};
}
static int publish(NSDictionary *value) {
    NSData *json=[NSJSONSerialization dataWithJSONObject:value options:0 error:nil];
    return thc_accessibility(json.bytes,json.length);
}
static id findOutline(id element,NSMutableSet *seen) {
    if (!element || [seen containsObject:element]) return nil;
    [seen addObject:element];
    /* AppKit's window children include legacy reparenting proxies. */
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
    id role=[element respondsToSelector:@selector(accessibilityRole)]?[element accessibilityRole]:[element accessibilityAttributeValue:NSAccessibilityRoleAttribute];
    if ([role isEqual:NSAccessibilityOutlineRole]) return element;
    NSArray *children=[element respondsToSelector:@selector(accessibilityChildren)]?[element accessibilityChildren]:[element accessibilityAttributeValue:NSAccessibilityChildrenAttribute];
#pragma clang diagnostic pop
    for (id child in children) { id found=findOutline(child,seen); if (found) return found; }
    return nil;
}
static void near(double x,double y) { if (fabs(x-y)>0.000001) { fprintf(stderr,"Expected %.6f, got %.6f\n",y,x); abort(); } }
int main(int argc,char **argv) {
 @autoreleasepool {
    assert(SDL_SetEnvironmentVariable(SDL_GetEnvironment(),"THC_EDIT_CAPTURE_EXIT","1",true));
    SDL_SetHint(SDL_HINT_MAC_BACKGROUND_APP,"1");
    const char *backend=argc>1?argv[1]:"software";
    assert(thc_open(backend,2.125,80,25,16));
    int count=0; SDL_Window **windows=SDL_GetWindows(&count); assert(count==1);
    SDL_Window *sdl=windows[0]; SDL_free(windows);
    assert(SDL_GetWindowFlags(sdl)&SDL_WINDOW_HIDDEN);
    NSWindow *window=(__bridge NSWindow *)SDL_GetPointerProperty(SDL_GetWindowProperties(sdl),SDL_PROP_WINDOW_COCOA_WINDOW_POINTER,NULL);
    assert(window && !window.visible && !window.keyWindow);
    int cols,rows; thc_size(&cols,&rows);
    NSArray *hostId=@[@"sidebar"],*provider=@[@"tree",[@"a" stringByPaddingToLength:48 withString:@"a" startingAtIndex:0],@"1.1"],*fileId=@[@"tree",provider[1],@"1.2"];
    NSDictionary *root=node(hostId,nil,@"tree",@"Sidebar",@[@1,@2,@22,@(rows-2)],NSNull.null,-1,0);
    NSDictionary *folder=node(provider,hostId,@"treeitem",@"Files",@[@1,@2,@22,@1],@YES,0,1);
    NSDictionary *file=node(fileId,provider,@"treeitem",@"safe λ <script>.hs",@[@1,@3,@22,@1],NSNull.null,1,2);
    NSDictionary *snapshot=@{@"revision":@1,@"layout":@[@(cols),@(rows),@24,@0,@1,@1,@0],@"visibleStart":@0,@"visibleCount":@2,@"logicalRows":@2,@"readOnly":@YES,@"nodes":@[root,folder,file]};
    assert(publish(snapshot));
    assert(findOutline(window,[NSMutableSet new]));
    id tree=nil;for (NSView *view in window.contentView.subviews) if ([view isKindOfClass:NSClassFromString(@"HideAXHost")]) tree=view;
    assert(tree);
    assert([[tree accessibilityLabel] isEqual:@"Sidebar"]);
    NSArray *items=[tree accessibilityRows]; assert(items.count==2);
    id oldFolder=items[0],oldFile=items[1];
    assert([[oldFolder accessibilityLabel] isEqual:@"Files"] && [[oldFile accessibilityLabel] isEqual:file[@"name"]]);
    assert([oldFile accessibilityParent]==tree && [oldFile accessibilityDisclosedByRow]==oldFolder);
    assert([[oldFolder accessibilityDisclosedRows] containsObject:oldFile]);
    assert([oldFile accessibilityDisclosureLevel]==1 && [oldFile accessibilityIndex]==1 && [oldFile isAccessibilitySelected]);
    assert([[tree accessibilitySelectedRows] containsObject:oldFile]);
    assert(![oldFile isAccessibilityFocused] && [[oldFile accessibilityHelp] containsString:@"Focused in editor"]);
    assert(![tree acceptsFirstResponder] && [tree hitTest:NSMakePoint(2,2)]==nil);
    for (id item in @[tree,oldFolder,oldFile]) {
        assert(![item isAccessibilitySelectorAllowed:@selector(accessibilityPerformPress)]);
        assert(![item isAccessibilitySelectorAllowed:@selector(setAccessibilitySelected:)]);
        assert(![item isAccessibilitySelectorAllowed:@selector(setAccessibilityFocused:)]);
    }
    int ww,wh,pw,ph; SDL_GetWindowSize(sdl,&ww,&wh);
    assert(SDL_GetRenderOutputSize(SDL_GetRenderer(sdl),&pw,&ph));
    double sx=(double)ww/pw,sy=(double)wh/ph;
    int ox=(pw-cols*8*2.125)/2,oy=(ph-rows*16*2.125)/2;
    double rect[4]; assert(thc_accessibility_cell_rect(1,3,22,1,rect));
    near(rect[0],(ox+floor(8*2.125))*sx); near(rect[1],(oy+floor(3*16*2.125))*sy);
    near(rect[2],(floor(23*8*2.125)-floor(8*2.125))*sx); near(rect[3],(floor(4*16*2.125)-floor(3*16*2.125))*sy);
    NSRect expected=[window convertRectToScreen:NSMakeRect(rect[0],NSHeight(window.contentView.bounds)-rect[1]-rect[3],rect[2],rect[3])];
    NSRect actual=[oldFile accessibilityFrame]; near(actual.origin.x,expected.origin.x);near(actual.origin.y,expected.origin.y);near(actual.size.width,expected.size.width);near(actual.size.height,expected.size.height);
    [window setFrameOrigin:NSMakePoint(-300,-800)];
    expected=[window convertRectToScreen:NSMakeRect(rect[0],NSHeight(window.contentView.bounds)-rect[1]-rect[3],rect[2],rect[3])];
    actual=[oldFile accessibilityFrame];near(actual.origin.x,expected.origin.x);near(actual.origin.y,expected.origin.y);assert(actual.origin.x<0);
    assert(!thc_accessibility_cell_rect(cols,0,1,1,rect));
    NSMutableDictionary *replacement=[snapshot mutableCopy],*renamed=[file mutableCopy]; renamed[@"name"]=@"same revision replacement.hs";
    replacement[@"nodes"]=@[root,folder,renamed];assert(publish(replacement));
    assert([tree accessibilityRows][1]==oldFile && [[oldFile accessibilityLabel] isEqual:renamed[@"name"]]);
    NSMutableDictionary *loading=[folder mutableCopy]; loading[@"bounds"]=NSNull.null;loading[@"loading"]=@YES;loading[@"moreChildren"]=@YES;
    replacement[@"nodes"]=@[root,loading];replacement[@"visibleStart"]=@1;replacement[@"visibleCount"]=@1;
    assert(publish(replacement));assert([tree accessibilityRows][0]==oldFolder);
    assert([[oldFolder accessibilityHelp] containsString:@"Loading children"] && [[oldFolder accessibilityHelp] containsString:@"Children are not fully loaded"]);
    assert(NSEqualRects([oldFolder accessibilityFrame],NSZeroRect));
    assert(![oldFile isAccessibilityElement] && ![oldFile accessibilityLabel] && ![oldFile accessibilityParent]);
    NSMutableDictionary *later=[file mutableCopy];later[@"index"]=@65535;
    replacement[@"nodes"]=@[root,folder,later];replacement[@"logicalRows"]=@65536;
    assert(publish(replacement));assert([[tree accessibilityRows][1] accessibilityIndex]==65535);
    replacement[@"logicalRows"]=@65537;assert(!publish(replacement));assert(!findOutline(window,[NSMutableSet new]));
    assert(![oldFolder isAccessibilityElement] && ![oldFolder accessibilityLabel]);
    for (NSDictionary *bad in @[
        @{@"bounds":@[@(cols),@0,@1,@1]},@{@"name":[@"x" stringByPaddingToLength:257 withString:@"x" startingAtIndex:0]},@{@"parent":fileId},@{@"selected":@1}
    ]) {
        assert(publish(snapshot));NSMutableDictionary *invalidFile=[file mutableCopy];[invalidFile addEntriesFromDictionary:bad];
        replacement=[snapshot mutableCopy];replacement[@"nodes"]=@[root,folder,invalidFile];assert(!publish(replacement));assert(!findOutline(window,[NSMutableSet new]));
    }
    assert(publish(snapshot));assert(!thc_accessibility("x",2097153));assert(!findOutline(window,[NSMutableSet new])); // Oversize is refused before reading bytes.
    assert(publish(snapshot));assert(thc_accessibility(NULL,0));assert(!findOutline(window,[NSMutableSet new]));
    assert(!window.visible && !window.keyWindow);
    printf("%s hidden SDL accessibility discovery, hierarchy, identity, bounds (density %.3f), read-only selectors and retirement checks passed\n",backend,(double)pw/ww);
    thc_close();
 }
 return 0;
}
