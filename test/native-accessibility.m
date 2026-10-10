/* SPDX-License-Identifier: BSD-3-Clause */
/* The SDL window stays hidden and never becomes key or ordered. */
#import <Cocoa/Cocoa.h>
#import <objc/runtime.h>
#include <SDL3/SDL.h>
#include "../cbits/window.h"
#include "../cbits/accessibility.h"
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

static NSDictionary *node(NSArray *ident,NSArray *parent,NSString *role,NSString *name,id rectangle,id expanded,int index,int level) {
    return @{@"id":ident,@"parent":parent ?: (id)NSNull.null,@"role":role,@"name":name,@"bounds":rectangle,
        @"selected":index==1?@YES:@NO,@"focused":index==1?@YES:@NO,@"expanded":expanded,@"loading":@NO,@"moreChildren":@NO,
        @"childrenKnown":@1,@"generation":@1,@"level":@(level),@"posInSet":@(level?1:0),@"setSize":@1,
        @"index":index<0?(id)NSNull.null:@(index)};
}
static NSMutableDictionary *dialogNode(NSArray *ident,NSArray *parent,NSString *role,NSString *name,id value,NSArray *rectangle) {
    return [@{@"id":ident,@"parent":parent ?: (id)NSNull.null,@"role":role,@"name":name,@"value":value,
        @"bounds":rectangle,@"focused":@NO,@"checked":NSNull.null,@"selected":NSNull.null,@"expanded":NSNull.null,@"multiline":@NO} mutableCopy];
}
static int publish(NSDictionary *value) {
    NSData *json=[NSJSONSerialization dataWithJSONObject:value options:0 error:nil];
    return thc_accessibility(json.bytes,json.length);
}
static id findRole(id element,NSString *wanted,NSMutableSet *seen) {
    if (!element || [seen containsObject:element]) return nil;
    [seen addObject:element];
    /* AppKit's window children include legacy reparenting proxies. */
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
    id role=[element respondsToSelector:@selector(accessibilityRole)]?[element accessibilityRole]:[element accessibilityAttributeValue:NSAccessibilityRoleAttribute];
    if ([role isEqual:wanted]) return element;
    NSArray *children=[element respondsToSelector:@selector(accessibilityChildren)]?[element accessibilityChildren]:[element accessibilityAttributeValue:NSAccessibilityChildrenAttribute];
#pragma clang diagnostic pop
    for (id child in children) { id found=findRole(child,wanted,seen); if (found) return found; }
    return nil;
}
static id sourceExcerpt(NSWindow *window) {
    for (NSView *view in window.contentView.subviews) if ([view isKindOfClass:NSClassFromString(@"HideAXSource")]) return view;
    return nil;
}
@protocol HapticEdges
- (void)crossedButton:(NSTrackingArea *)area dragging:(BOOL)dragging;
@end
static unsigned hapticRequests;
static void countHapticRequest(id receiver,SEL selector) { (void)receiver;(void)selector;++hapticRequests; }
static void near(double x,double y) { if (fabs(x-y)>0.000001) { fprintf(stderr,"Expected %.6f, got %.6f\n",y,x); abort(); } }
static NSDictionary *canvasActionReceipt(void) {
    Uint64 deadline=SDL_GetTicks()+1000;
    while (SDL_GetTicks()<deadline) {
        int32_t event[6];assert(thc_wait(event));
        if (event[0]==18) {
            const char *json=thc_text();
            NSDictionary *value=[NSJSONSerialization JSONObjectWithData:[NSData dataWithBytes:json length:strlen(json)] options:0 error:nil];
            assert([value isKindOfClass:NSDictionary.class]);return value;
        }
    }
    assert(!"Canvas action did not deliver its own SDL receipt");return nil;
}
static bool rejectCanvasActionPush(void *context,SDL_Event *event) { (void)context;return event->type<SDL_EVENT_USER; }
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
    assert(findRole(window,NSAccessibilityOutlineRole,[NSMutableSet new]));
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
    replacement[@"logicalRows"]=@65537;assert(!publish(replacement));assert(!findRole(window,NSAccessibilityOutlineRole,[NSMutableSet new]));
    assert(![oldFolder isAccessibilityElement] && ![oldFolder accessibilityLabel]);
    for (NSDictionary *bad in @[
        @{@"bounds":@[@(cols),@0,@1,@1]},@{@"name":[@"x" stringByPaddingToLength:257 withString:@"x" startingAtIndex:0]},@{@"parent":fileId},@{@"selected":@1}
    ]) {
        assert(publish(snapshot));NSMutableDictionary *invalidFile=[file mutableCopy];[invalidFile addEntriesFromDictionary:bad];
        replacement=[snapshot mutableCopy];replacement[@"nodes"]=@[root,folder,invalidFile];assert(!publish(replacement));assert(!findRole(window,NSAccessibilityOutlineRole,[NSMutableSet new]));
    }
    assert(publish(snapshot));assert(!thc_accessibility("x",2097153));assert(!findRole(window,NSAccessibilityOutlineRole,[NSMutableSet new])); // Oversize is refused before reading bytes.
    assert(publish(snapshot));assert(thc_accessibility(NULL,0));assert(!findRole(window,NSAccessibilityOutlineRole,[NSMutableSet new]));
    assert(publish(snapshot));
    NSDictionary *picture=@{@"id":@91,@"name":@"safe λ <script>.png",@"description":@"PNG image, 2 by 2 pixels",@"bounds":@[@1,@3,@22,@1]};
    NSDictionary *canvas=@{@"size":@[@(cols),@(rows)],@"images":@[picture]};
    assert(publish(canvas));
    id image=findRole(window,NSAccessibilityImageRole,[NSMutableSet new]);assert(image);
    assert([[image accessibilityLabel] isEqual:picture[@"name"]] && [[image accessibilityHelp] isEqual:picture[@"description"]]);
    actual=[image accessibilityFrame];near(actual.origin.x,expected.origin.x);near(actual.origin.y,expected.origin.y);
    assert(![image isAccessibilityFocused] && ![image isAccessibilitySelectorAllowed:@selector(accessibilityPerformPress)] &&
        ![image isAccessibilitySelectorAllowed:@selector(setAccessibilityFocused:)]);
    id imageGroup=[image accessibilityParent];assert(![imageGroup acceptsFirstResponder] && [imageGroup hitTest:NSMakePoint(2,2)]==nil);
    NSMutableDictionary *moved=[picture mutableCopy];moved[@"bounds"]=@[@2,@4,@5,@2];
    assert(publish(@{@"size":@[@(cols),@(rows)],@"images":@[moved]}));assert(findRole(window,NSAccessibilityImageRole,[NSMutableSet new])==image);
    assert(publish(@{@"size":@[@(cols),@(rows)],@"images":@[]}));
    assert(!findRole(window,NSAccessibilityImageRole,[NSMutableSet new]) && ![image isAccessibilityElement] && ![image accessibilityLabel]);
    assert(findRole(window,NSAccessibilityOutlineRole,[NSMutableSet new])); // Image clear leaves the independent sidebar intact.
    assert(publish(canvas));
    assert(!publish(@{@"size":@[@(cols),@(rows)],@"images":@[picture,picture]}));assert(!findRole(window,NSAccessibilityImageRole,[NSMutableSet new]));
    assert(publish(canvas));assert(thc_accessibility(NULL,0));
    assert(!findRole(window,NSAccessibilityImageRole,[NSMutableSet new]) && !findRole(window,NSAccessibilityOutlineRole,[NSMutableSet new]));
    assert(publish(snapshot));assert(publish(canvas));
    id firstResponder=window.firstResponder;
    NSDictionary *controls=@{@"id":@91,@"view":@"1",@"resource":[@"a" stringByPaddingToLength:48 withString:@"a" startingAtIndex:0],@"anchor":@[@1,@3]};
    NSMutableDictionary *actionPicture=[picture mutableCopy];actionPicture[@"controls"]=controls;
    NSDictionary *actionCanvas=@{@"size":@[@(cols),@(rows)],@"images":@[actionPicture]};
    assert(publish(actionCanvas));id actionImage=findRole(window,NSAccessibilityImageRole,[NSMutableSet new]);assert(actionImage);
    NSArray<NSAccessibilityCustomAction *> *actions=[actionImage accessibilityCustomActions];assert(actions.count==4);
    NSArray *actionNames=@[@"fit",@"actual-size",@"zoom-in",@"zoom-out"],*actionLabels=@[@"Fit Image",@"Actual Size",@"Zoom In",@"Zoom Out"];
    assert([actionImage isAccessibilitySelectorAllowed:@selector(setAccessibilityFocused:)]);
    [actionImage setAccessibilityFocused:YES];assert([actionImage isAccessibilityFocused] && window.firstResponder==firstResponder);
    for (NSUInteger i=0;i<actions.count;++i) {
        assert([actions[i].name isEqual:actionLabels[i]] && actions[i].handler());
        NSDictionary *reply=canvasActionReceipt();
        assert(reply.count==3 && [reply[@"type"] isEqual:@"canvas-action"] && [reply[@"target"] isEqual:controls] && [reply[@"action"] isEqual:actionNames[i]]);
    }
    actionPicture[@"description"]=@"refreshed viewport description";
    assert(publish(actionCanvas));assert(findRole(window,NSAccessibilityImageRole,[NSMutableSet new])==actionImage && [actionImage isAccessibilityFocused]);
    assert([actionImage accessibilityCustomActions][0]==actions[0]);
    NSMutableDictionary *nextControls=[controls mutableCopy];nextControls[@"view"]=@"2";actionPicture[@"controls"]=nextControls;
    assert(publish(actionCanvas));assert(findRole(window,NSAccessibilityImageRole,[NSMutableSet new])==actionImage && [actionImage isAccessibilityFocused]);
    assert(!actions[0].handler());
    NSArray<NSAccessibilityCustomAction *> *nextActions=[actionImage accessibilityCustomActions];assert(nextActions.count==4 && nextActions[0].handler());
    assert([canvasActionReceipt()[@"target"] isEqual:nextControls]);
    actionPicture[@"controls"]=controls;assert(publish(actionCanvas));assert(!actions[0].handler()); // Returning fields cannot revive a retired action lifetime.
    NSArray<NSAccessibilityCustomAction *> *currentActions=[actionImage accessibilityCustomActions];
    NSView *canvasView=[actionImage accessibilityParent];canvasView.hidden=YES;assert(!currentActions[0].handler());canvasView.hidden=NO;
    actionPicture[@"controls"]=NSNull.null;assert(publish(actionCanvas));assert(!currentActions[0].handler() && ![[actionImage accessibilityCustomActions] count]);
    actionPicture[@"controls"]=controls;assert(publish(actionCanvas));assert(!currentActions[0].handler());
    currentActions=[actionImage accessibilityCustomActions];
    nextControls=[controls mutableCopy];nextControls[@"resource"]=[@"b" stringByPaddingToLength:48 withString:@"b" startingAtIndex:0];actionPicture[@"controls"]=nextControls;
    assert(publish(actionCanvas));assert(findRole(window,NSAccessibilityImageRole,[NSMutableSet new])!=actionImage && ![actionImage isAccessibilityElement] && !currentActions[0].handler());
    for (NSDictionary *invalidControls in @[@{ @"id":@92 },@{ @"view":@"0" },@{ @"view":@"01" },@{ @"resource":@"bad" },@{ @"anchor":@[@(cols),@3] }]) {
        actionPicture[@"controls"]=controls;assert(publish(actionCanvas));id previous=findRole(window,NSAccessibilityImageRole,[NSMutableSet new]);
        NSArray<NSAccessibilityCustomAction *> *previousActions=[previous accessibilityCustomActions];
        NSMutableDictionary *invalid=[controls mutableCopy];[invalid addEntriesFromDictionary:invalidControls];actionPicture[@"controls"]=invalid;
        assert(!publish(actionCanvas));assert(![previous isAccessibilityElement] && !previousActions[0].handler());
    }
    assert(!thc_post_canvas_action(NULL,1) && !thc_post_canvas_action("x",1025));
    actionPicture[@"controls"]=controls;assert(publish(actionCanvas));
    NSArray<NSAccessibilityCustomAction *> *failedPushActions=[findRole(window,NSAccessibilityImageRole,[NSMutableSet new]) accessibilityCustomActions];
    SDL_EventFilter previousFilter;void *previousFilterContext;bool hadFilter=SDL_GetEventFilter(&previousFilter,&previousFilterContext);
    SDL_SetEventFilter(rejectCanvasActionPush,NULL);assert(!failedPushActions[0].handler());SDL_SetEventFilter(hadFilter?previousFilter:NULL,hadFilter?previousFilterContext:NULL);
    assert(publish(canvas));assert(!failedPushActions[0].handler());
    NSMutableDictionary *source=[@{@"present":@YES,@"readOnly":@YES,@"id":@[@"source",@"7",@"9007199254740992"],
        @"revision":@1,@"name":@"safe λ <script>.hs",@"bounds":@[@1,@3,@22,@1],@"firstLine":@4,@"firstColumn":@0,
        @"lineCount":@1,@"value":@"visible λ <script> text",@"truncated":@NO} mutableCopy];
    NSDictionary *sourceWrapper=@{@"size":@[@(cols),@(rows)],@"source":source};
    assert(publish(sourceWrapper));
    id sourceElement=findRole(window,NSAccessibilityStaticTextRole,[NSMutableSet new]);assert(sourceElement);
    assert(sourceElement==sourceExcerpt(window) && [sourceElement isAccessibilitySelectorAllowed:@selector(accessibilityValue)]);
    assert([[sourceElement accessibilityLabel] isEqual:@"safe λ <script>.hs"] && [[sourceElement accessibilityValue] isEqual:@"visible λ <script> text"]);
    assert([[sourceElement accessibilityHelp] containsString:@"line 4"] && ![sourceElement isAccessibilityFocused]);
    actual=[sourceElement accessibilityFrame];near(actual.origin.x,expected.origin.x);near(actual.origin.y,expected.origin.y);
    near(actual.size.width,expected.size.width);near(actual.size.height,expected.size.height);
    assert(![sourceElement acceptsFirstResponder] && [sourceElement hitTest:NSMakePoint(2,2)]==nil && window.firstResponder==firstResponder);
    for (NSString *selector in @[@"setAccessibilityValue:",@"setAccessibilityFocused:",@"accessibilityPerformPress",
        @"accessibilitySelectedText",@"accessibilitySelectedTextRange",@"accessibilityStringForRange:",@"accessibilityRangeForLine:",@"accessibilityFrameForRange:"]) {
        assert(![sourceElement isAccessibilitySelectorAllowed:NSSelectorFromString(selector)]);
    }
    source[@"revision"]=@2;source[@"bounds"]=@[@2,@4,@5,@2];source[@"value"]=@"changed\nvisible";source[@"lineCount"]=@2;source[@"truncated"]=@YES;
    assert(publish(sourceWrapper));assert(findRole(window,NSAccessibilityStaticTextRole,[NSMutableSet new])==sourceElement);
    assert([[sourceElement accessibilityValue] isEqual:@"changed\nvisible"] && [[sourceElement accessibilityHelp] containsString:@"omitted"]);
    assert(thc_accessibility_cell_rect(2,4,5,2,rect));
    NSRect sourceExpected=[window convertRectToScreen:NSMakeRect(rect[0],NSHeight(window.contentView.bounds)-rect[1]-rect[3],rect[2],rect[3])];
    actual=[sourceElement accessibilityFrame];near(actual.origin.x,sourceExpected.origin.x);near(actual.origin.y,sourceExpected.origin.y);
    near(actual.size.width,sourceExpected.size.width);near(actual.size.height,sourceExpected.size.height);
    // 2048 + 15*2047 text scalars and 15 separators reach 32768; the pair counts once.
    NSString *sourceLine=[@"λ" stringByPaddingToLength:2047 withString:@"λ" startingAtIndex:0];
    NSMutableArray *sourceLines=[NSMutableArray arrayWithObject:[@"😀" stringByAppendingString:sourceLine]];
    for (int i=1;i<16;++i) [sourceLines addObject:sourceLine];
    source[@"value"]=[sourceLines componentsJoinedByString:@"\n"];source[@"lineCount"]=@16;source[@"bounds"]=@[@2,@4,@5,@16];
    assert(publish(sourceWrapper));assert([[sourceElement accessibilityValue] length]==32769);
    source[@"value"]=@"";source[@"lineCount"]=@1;source[@"firstLine"]=@9007199254740991;
    assert(publish(sourceWrapper));assert([[sourceElement accessibilityValue] isEqual:@""]);
    source[@"value"]=@"changed\nvisible";source[@"lineCount"]=@2;source[@"firstLine"]=@4;source[@"bounds"]=@[@2,@4,@5,@2];
    assert(publish(sourceWrapper));
    NSMutableDictionary *emptySidebar=[snapshot mutableCopy];emptySidebar[@"nodes"]=@[];emptySidebar[@"visibleCount"]=@0;
    assert(publish(emptySidebar));assert(findRole(window,NSAccessibilityStaticTextRole,[NSMutableSet new])==sourceElement);
    assert(publish(snapshot));
    source[@"id"]=@[@"source",@"8",@"9007199254740992"];
    assert(publish(sourceWrapper));
    id replacementSource=findRole(window,NSAccessibilityStaticTextRole,[NSMutableSet new]);assert(replacementSource && replacementSource!=sourceElement);
    assert(![sourceElement isAccessibilityElement] && ![sourceElement accessibilityValue] && ![sourceElement accessibilityLabel] && ![sourceElement accessibilityParent]);
    NSDictionary *absentSource=@{@"size":@[@(cols),@(rows)],@"source":@{@"present":@NO,@"readOnly":@YES}};
    assert(publish(absentSource));assert(!sourceExcerpt(window));
    assert(![replacementSource isAccessibilityElement] && ![replacementSource accessibilityValue]); // Privacy replacement forgets the old excerpt.
    assert(findRole(window,NSAccessibilityOutlineRole,[NSMutableSet new]) && findRole(window,NSAccessibilityImageRole,[NSMutableSet new]));
    // All 17 lines remain within their own limit; only the aggregate exceeds it.
    sourceLines[0]=sourceLine;[sourceLines addObject:@"x"];
    for (NSDictionary *bad in @[@{@"present":@1},@{@"readOnly":@NO},@{@"id":@[@"source",@"8",@"+9"]},@{@"revision":@9007199254740992},
        @{@"name":[@"x" stringByPaddingToLength:257 withString:@"x" startingAtIndex:0]},@{@"bounds":@[@(cols),@1,@1,@1]},
        @{@"firstLine":@0},@{@"firstLine":@9007199254740991},@{@"firstColumn":@YES},@{@"lineCount":@0},@{@"lineCount":@257},
        @{@"bounds":@[@2,@4,@5,@1]},@{@"value":@"one line"},@{@"value":@"bad\t\ncontrol"},@{@"value":[NSString stringWithFormat:@"bad%C\ncontrol",(unichar)0x85]},
        @{@"value":[[@"x" stringByPaddingToLength:2049 withString:@"x" startingAtIndex:0] stringByAppendingString:@"\nend"]},
        @{@"value":[sourceLines componentsJoinedByString:@"\n"],@"lineCount":@17,@"bounds":@[@2,@4,@5,@17]},@{@"truncated":@1}]) {
        assert(publish(sourceWrapper));id previous=findRole(window,NSAccessibilityStaticTextRole,[NSMutableSet new]);assert(previous);
        NSMutableDictionary *invalid=[source mutableCopy];[invalid addEntriesFromDictionary:bad];
        assert(!publish(@{@"size":@[@(cols),@(rows)],@"source":invalid}));
        assert(!sourceExcerpt(window) && ![previous accessibilityValue] && ![previous isAccessibilityElement]);
    }
    assert(publish(sourceWrapper));
    assert(!publish(@{@"size":@[@(cols),@(rows)],@"source":source,@"padding":[@"x" stringByPaddingToLength:262144 withString:@"x" startingAtIndex:0]}));
    assert(!sourceExcerpt(window));
    assert(publish(sourceWrapper));id malformedSource=sourceExcerpt(window);assert(malformedSource);
    assert(!thc_accessibility("{",1));assert(!sourceExcerpt(window) && ![malformedSource accessibilityValue]);
    assert(publish(snapshot));assert(publish(canvas));
    assert(publish(sourceWrapper));id coveredSource=findRole(window,NSAccessibilityStaticTextRole,[NSMutableSet new]);assert(coveredSource);
    NSArray *dialogId=@[@"dialog"],*inputId=@[@"dialog",@"field",@"0"],*checkId=@[@"dialog",@"field",@"1"];
    NSMutableDictionary *modalRoot=dialogNode(dialogId,nil,@"dialog",@"Options λ",NSNull.null,@[@0,@1,@30,@12]);
    NSMutableDictionary *input=dialogNode(inputId,dialogId,@"textbox",@"Name",@"plain <script> text",@[@1,@3,@22,@1]);input[@"focused"]=@YES;
    NSMutableDictionary *tick=dialogNode(checkId,dialogId,@"checkbox",@"Enabled",NSNull.null,@[@1,@5,@12,@1]);tick[@"checked"]=@YES;
    NSArray *listId=@[@"dialog",@"field",@"2"],*choiceId=@[@"dialog",@"field",@"2",@"option",@"0"];
    NSMutableDictionary *list=dialogNode(listId,dialogId,@"listbox",@"Choices",NSNull.null,@[@1,@6,@12,@3]);
    NSMutableDictionary *choice=dialogNode(choiceId,listId,@"option",@"Selected item",NSNull.null,@[@1,@7,@12,@1]);choice[@"selected"]=@YES;
    NSArray *radioId=@[@"dialog",@"field",@"3"],*comboId=@[@"dialog",@"field",@"4"];
    NSMutableDictionary *radioGroup=dialogNode(radioId,dialogId,@"radiogroup",@"Radio choices",NSNull.null,@[@14,@5,@12,@2]);
    NSMutableDictionary *radio=dialogNode(@[@"dialog",@"field",@"3",@"option",@"0"],radioId,@"radio",@"Choice A",NSNull.null,@[@14,@6,@12,@1]);radio[@"checked"]=@YES;
    NSMutableDictionary *combo=dialogNode(comboId,dialogId,@"combobox",@"Combo",@"Choice A",@[@14,@8,@12,@1]);combo[@"expanded"]=@YES;
    NSMutableDictionary *popup=dialogNode(@[@"dialog",@"field",@"4",@"option",@"0"],comboId,@"option",@"Popup choice",NSNull.null,@[@14,@9,@12,@1]);popup[@"selected"]=@YES;
    NSMutableDictionary *button=dialogNode(@[@"dialog",@"button",@"0"],dialogId,@"button",@"OK",NSNull.null,@[@14,@11,@8,@1]);
    NSMutableDictionary *multiline=dialogNode(@[@"dialog",@"field",@"5"],dialogId,@"textbox",@"Notes",@"line one\nline two",@[@1,@10,@12,@2]);multiline[@"multiline"]=@YES;
    NSMutableDictionary *bodyText=dialogNode(@[@"dialog",@"body",@"0"],dialogId,@"text",@"Plain explanation",NSNull.null,@[@1,@2,@22,@1]);
    NSMutableDictionary *modal=[@{@"present":@YES,@"readOnly":@YES,@"truncated":@NO,
        @"nodes":@[modalRoot,input,tick,list,choice,radioGroup,radio,combo,popup,button,multiline,bodyText]} mutableCopy];
    NSDictionary *modalWrapper=@{@"size":@[@(cols),@(rows)],@"dialog":modal};
    assert(publish(actionCanvas));
    NSArray<NSAccessibilityCustomAction *> *coveredActions=[findRole(window,NSAccessibilityImageRole,[NSMutableSet new]) accessibilityCustomActions];assert(coveredActions.count==4);
    assert(publish(modalWrapper));assert(!coveredActions[0].handler());
    assert(![coveredSource isAccessibilityElement] && ![coveredSource accessibilityValue] && ![coveredSource accessibilityParent]);
    id dialogHost=nil;for (NSView *view in window.contentView.subviews) if ([view isKindOfClass:NSClassFromString(@"HideAXDialog")]) dialogHost=view;
    assert(dialogHost && [[dialogHost accessibilitySubrole] isEqual:NSAccessibilityDialogSubrole]);
    assert([[dialogHost accessibilityLabel] isEqual:@"Options λ"] && [[dialogHost accessibilityChildren] count]==8);
    id inputElement=findRole(window,NSAccessibilityTextFieldRole,[NSMutableSet new]);assert(inputElement);
    assert([[inputElement accessibilityLabel] isEqual:@"Name"] && [[inputElement accessibilityValue] isEqual:@"plain <script> text"]);
    assert([[inputElement accessibilityHelp] containsString:@"Focused in editor"] && ![inputElement isAccessibilityFocused]);
    actual=[inputElement accessibilityFrame];near(actual.origin.x,expected.origin.x);near(actual.origin.y,expected.origin.y);
    near(actual.size.width,expected.size.width);near(actual.size.height,expected.size.height);
    id checkbox=findRole(window,NSAccessibilityCheckBoxRole,[NSMutableSet new]);assert([[checkbox accessibilityValue] isEqual:@YES]);
    assert([inputElement isAccessibilitySelectorAllowed:@selector(accessibilityValue)]);
    assert([[findRole(window,NSAccessibilityRadioButtonRole,[NSMutableSet new]) accessibilityValue] isEqual:@YES]);
    assert(findRole(window,NSAccessibilityRadioGroupRole,[NSMutableSet new]));
    id comboElement=findRole(window,NSAccessibilityComboBoxRole,[NSMutableSet new]);assert(comboElement && [comboElement isAccessibilityExpanded]);
    assert([[comboElement accessibilityChildren] count]==1); // Popup bounds need not lie inside the combo field.
    assert([[findRole(window,NSAccessibilityTextAreaRole,[NSMutableSet new]) accessibilityValue] isEqual:@"line one\nline two"]);
    assert([[findRole(window,NSAccessibilityStaticTextRole,[NSMutableSet new]) accessibilityLabel] isEqual:@"Plain explanation"]);
    assert(findRole(window,NSAccessibilityButtonRole,[NSMutableSet new]));
    id listing=findRole(window,NSAccessibilityListRole,[NSMutableSet new]);assert(listing && [[listing accessibilityChildren] count]==1);
    assert([[[listing accessibilityChildren] firstObject] accessibilityParent]==listing);
    assert([[[listing accessibilitySelectedChildren] firstObject] isAccessibilitySelected]);
    for (id element in @[dialogHost,inputElement,checkbox,listing,comboElement]) {
        assert(![element isAccessibilitySelectorAllowed:@selector(accessibilityPerformPress)] &&
            ![element isAccessibilitySelectorAllowed:@selector(setAccessibilityValue:)] &&
            ![element isAccessibilitySelectorAllowed:@selector(setAccessibilityFocused:)]);
    }
    assert(![dialogHost acceptsFirstResponder] && [dialogHost hitTest:NSMakePoint(2,2)]==nil);
    assert(window.firstResponder==firstResponder && !findRole(window,NSAccessibilityOutlineRole,[NSMutableSet new]) && !findRole(window,NSAccessibilityImageRole,[NSMutableSet new]));
    NSView *dialogView=dialogHost;
    assert(dialogView.trackingAreas.count==0); // Off unless explicitly enabled.
    NSMutableDictionary *hapticModal=[modalWrapper mutableCopy];hapticModal[@"hapticFeedback"]=@YES;
    assert(publish(hapticModal));assert(dialogView.trackingAreas.count==1);
    NSTrackingArea *edge=dialogView.trackingAreas.firstObject;
    double edgeRect[4];assert(thc_accessibility_cell_rect(14,11,8,1,edgeRect));
    assert(NSEqualRects(edge.rect,NSMakeRect(edgeRect[0],edgeRect[1],edgeRect[2],edgeRect[3])));
    Method feedback=class_getInstanceMethod([dialogView class],NSSelectorFromString(@"requestHapticFeedback"));assert(feedback);
    IMP originalFeedback=method_setImplementation(feedback,(IMP)countHapticRequest);
    // Either native edge can arrive first when a dialog opens under the pointer.
    [(id<HapticEdges>)dialogHost crossedButton:edge dragging:NO];assert(hapticRequests==1);
    input[@"value"]=@"edit while hovering";assert(publish(hapticModal));
    assert(dialogView.trackingAreas.firstObject==edge && hapticRequests==1);
    [(id<HapticEdges>)dialogHost crossedButton:edge dragging:NO];assert(hapticRequests==2);
    [(id<HapticEdges>)dialogHost crossedButton:edge dragging:YES];assert(hapticRequests==2);
    [(id<HapticEdges>)dialogHost crossedButton:edge dragging:NO];assert(hapticRequests==3);
    hapticModal[@"hapticFeedback"]=@NO;assert(publish(hapticModal));assert(dialogView.trackingAreas.count==0);
    [(id<HapticEdges>)dialogHost crossedButton:edge dragging:NO];assert(hapticRequests==3);
    hapticModal[@"hapticFeedback"]=@"yes";assert(!publish(hapticModal));
    method_setImplementation(feedback,originalFeedback);
    assert(publish(modalWrapper));
    dialogHost=nil;for (NSView *view in window.contentView.subviews) if ([view isKindOfClass:NSClassFromString(@"HideAXDialog")]) dialogHost=view;
    inputElement=findRole(window,NSAccessibilityTextFieldRole,[NSMutableSet new]);
    modal[@"truncated"]=@YES;assert(publish(modalWrapper));assert([[dialogHost accessibilityHelp] containsString:@"Some visible content is omitted"]);modal[@"truncated"]=@NO;
    input[@"value"]=@"updated";input[@"bounds"]=@[@2,@4,@5,@2];assert(publish(modalWrapper));
    assert(findRole(window,NSAccessibilityTextFieldRole,[NSMutableSet new])==inputElement && [[inputElement accessibilityValue] isEqual:@"updated"]);
    input[@"value"]=NSNull.null;assert(publish(modalWrapper));
    assert(![inputElement accessibilityValue] && ![inputElement isAccessibilitySelectorAllowed:@selector(accessibilityValue)]);
    assert(publish(snapshot));assert(publish(canvas));assert(publish(sourceWrapper)); // Lower snapshots cannot reappear through an active modal.
    assert(!findRole(window,NSAccessibilityOutlineRole,[NSMutableSet new]) && !findRole(window,NSAccessibilityImageRole,[NSMutableSet new]));
    assert(!sourceExcerpt(window));
    NSDictionary *emptyModal=@{@"present":@YES,@"readOnly":@YES,@"truncated":@NO,@"nodes":@[]};
    assert(publish(@{@"size":@[@(cols),@(rows)],@"dialog":emptyModal}));
    assert(![inputElement isAccessibilityElement] && ![inputElement accessibilityValue] && ![inputElement accessibilityParent]);
    assert(publish(snapshot));assert(!findRole(window,NSAccessibilityOutlineRole,[NSMutableSet new]));
    NSDictionary *dismissed=@{@"present":@NO,@"readOnly":@YES,@"truncated":@NO,@"nodes":@[]};
    assert(publish(@{@"size":@[@(cols),@(rows)],@"dialog":dismissed}));
    assert(publish(snapshot));assert(publish(canvas));assert(publish(sourceWrapper));
    assert(findRole(window,NSAccessibilityOutlineRole,[NSMutableSet new]) && findRole(window,NSAccessibilityImageRole,[NSMutableSet new]));
    id dismissedSource=findRole(window,NSAccessibilityStaticTextRole,[NSMutableSet new]);assert(dismissedSource);
    for (NSDictionary *bad in @[@{@"role":@"action"},@{@"parent":inputId},@{@"value":[@"x" stringByPaddingToLength:2049 withString:@"x" startingAtIndex:0]},@{@"focused":@1},@{@"bounds":@[@(cols),@1,@1,@1]}]) {
        NSMutableDictionary *invalid=[input mutableCopy];[invalid addEntriesFromDictionary:bad];
        modal[@"nodes"]=@[modalRoot,invalid];assert(!publish(modalWrapper));
        assert(![inputElement isAccessibilityElement]);
    }
    NSMutableArray *overBudget=[NSMutableArray arrayWithObject:modalRoot];
    for (int i=0;i<15;++i) [overBudget addObject:dialogNode(@[@"dialog",@"body",[@(i) stringValue]],dialogId,@"text",
        [@"x" stringByPaddingToLength:256 withString:@"x" startingAtIndex:0],[@"y" stringByPaddingToLength:2048 withString:@"y" startingAtIndex:0],@[@1,@2,@22,@1])];
    modal[@"nodes"]=overBudget;assert(!publish(modalWrapper));
    NSMutableArray *tooMany=[NSMutableArray new];for (int i=0;i<257;++i) [tooMany addObject:modalRoot];modal[@"nodes"]=tooMany;assert(!publish(modalWrapper));
    modal[@"nodes"]=@[modalRoot,input,input];assert(!publish(modalWrapper));
    modal[@"present"]=@NO;modal[@"nodes"]=@[modalRoot];assert(!publish(modalWrapper));
    assert(thc_accessibility(NULL,0));assert(window.firstResponder==firstResponder);
    assert(![dismissedSource isAccessibilityElement] && ![dismissedSource accessibilityValue]);
    assert(publish(sourceWrapper));id closingSource=findRole(window,NSAccessibilityStaticTextRole,[NSMutableSet new]);assert(closingSource);
    assert(thc_accessibility(NULL,0));assert(![closingSource isAccessibilityElement] && ![closingSource accessibilityValue]);
    assert(!window.visible && !window.keyWindow);
    assert(publish(sourceWrapper));id windowClosingSource=sourceExcerpt(window);assert(windowClosingSource);
    assert(publish(actionCanvas));
    NSArray<NSAccessibilityCustomAction *> *closingActions=[findRole(window,NSAccessibilityImageRole,[NSMutableSet new]) accessibilityCustomActions];assert(closingActions.count==4 && closingActions[0].handler());
    thc_close();assert(!closingActions[0].handler() && !thc_post_canvas_action("{}",2));
    assert(![windowClosingSource isAccessibilityElement] && ![windowClosingSource accessibilityValue] && ![windowClosingSource accessibilityParent]);
    printf("%s hidden SDL accessibility discovery, hierarchy, identity, bounds (density %.3f), read-only sidebar/source/modal selectors, exact image action receipts and retirement checks passed\n",backend,(double)pw/ww);
 }
 return 0;
}
