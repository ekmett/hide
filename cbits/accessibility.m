/* SPDX-License-Identifier: BSD-3-Clause */
#import <Cocoa/Cocoa.h>
#include "accessibility.h"
#include <math.h>

static BOOL integer(id value, double maximum) {
    if (![value isKindOfClass:NSNumber.class] || CFGetTypeID((__bridge CFTypeRef)value)==CFBooleanGetTypeID()) return NO;
    double n=[value doubleValue]; return isfinite(n) && n>=0 && n<=maximum && floor(n)==n;
}
static BOOL boolean(id value) { return [value isKindOfClass:NSNumber.class] && CFGetTypeID((__bridge CFTypeRef)value)==CFBooleanGetTypeID(); }
static BOOL text(id value, NSUInteger limit, BOOL nonempty) {
    if (![value isKindOfClass:NSString.class]) return NO;
    NSUInteger count=0;
    for (NSUInteger i=0;i<[value length];++i) {
        unichar c=[value characterAtIndex:i];
        if (CFStringIsSurrogateHighCharacter(c)) {
            if (++i==[value length] || !CFStringIsSurrogateLowCharacter([value characterAtIndex:i])) return NO;
        } else if (CFStringIsSurrogateLowCharacter(c)) return NO;
        if (++count>limit) return NO;
    }
    return !nonempty || count>0;
}
static BOOL identity(id value) {
    if (![value isKindOfClass:NSArray.class]) return NO;
    NSArray *parts=value;
    if (parts.count==1) return [parts[0] isEqual:@"sidebar"];
    if (parts.count!=3 || ![parts[0] isEqual:@"tree"] || !text(parts[1],48,YES) || [parts[1] length]!=48 || !text(parts[2],128,YES)) return NO;
    return [parts[1] rangeOfCharacterFromSet:[[NSCharacterSet characterSetWithCharactersInString:@"0123456789abcdef"] invertedSet]].location==NSNotFound;
}
static BOOL nullableBoolean(id value) { return value==NSNull.null || boolean(value); }
static NSInteger rowIndex(NSDictionary *node) { return node[@"index"]==NSNull.null ? NSNotFound : [node[@"index"] integerValue]; }
static BOOL bounds(id value, NSInteger cols, NSInteger rows) {
    if (value==NSNull.null) return YES;
    if (![value isKindOfClass:NSArray.class] || [value count]!=4) return NO;
    NSArray *r=value;
    return integer(r[0],cols) && integer(r[1],rows) && integer(r[2],cols) && integer(r[3],rows) &&
           [r[2] integerValue]>0 && [r[3] integerValue]>0 && [r[0] integerValue]+[r[2] integerValue]<=cols && [r[1] integerValue]+[r[3] integerValue]<=rows;
}

@class HideAXHost;
@interface HideAXRow : NSAccessibilityElement <NSAccessibilityRow>
@property(weak) HideAXHost *host;
@property(weak) HideAXRow *discloser;
@property(copy) NSDictionary *record;
@property(copy) NSArray<HideAXRow *> *disclosedRows;
- (void)retire;
@end
@interface HideAXHost : NSView <NSAccessibilityOutline>
@property(copy) NSDictionary *record;
@property(copy) NSArray<HideAXRow *> *rows;
@property(copy) NSDictionary<NSArray *,HideAXRow *> *nodes;
@end
static HideAXHost *host;
static NSRect screenFrame(NSView *view, id value) {
    if (!view.window || value==NSNull.null || !value) return NSZeroRect;
    NSArray *r=value; double rectangle[4];
    if (!thc_accessibility_cell_rect([r[0] intValue],[r[1] intValue],[r[2] intValue],[r[3] intValue],rectangle)) return NSZeroRect;
    NSRect frame=NSMakeRect(rectangle[0],rectangle[1],rectangle[2],rectangle[3]);
    if (!view.isFlipped) frame.origin.y=NSHeight(view.bounds)-NSMaxY(frame);
    return NSAccessibilityFrameInView(view,frame);
}
static BOOL readOnlySelector(SEL selector) {
    NSString *name=NSStringFromSelector(selector);
    return ![name hasPrefix:@"setAccessibility"] && ![name hasPrefix:@"accessibilityPerform"];
}
@implementation HideAXRow
- (BOOL)isAccessibilityElement { return self.record!=nil; }
- (NSAccessibilityRole)accessibilityRole { return NSAccessibilityRowRole; }
- (NSString *)accessibilityLabel { return self.record[@"name"]; }
- (id)accessibilityParent { return self.host; }
- (NSRect)accessibilityFrame { return screenFrame(self.host,self.record[@"bounds"]); }
- (NSInteger)accessibilityIndex { return rowIndex(self.record); }
- (NSInteger)accessibilityDisclosureLevel { return [self.record[@"level"] integerValue]-1; }
- (BOOL)isAccessibilitySelected { return [self.record[@"selected"] boolValue]; }
- (BOOL)isAccessibilityDisclosed { return self.record[@"expanded"]!=NSNull.null && [self.record[@"expanded"] boolValue]; }
- (id)accessibilityDisclosedByRow { return self.discloser; }
- (id)accessibilityDisclosedRows { return self.disclosedRows; }
- (BOOL)isAccessibilityFocused { return NO; }
- (NSString *)accessibilityHelp {
    NSMutableArray *parts=[NSMutableArray new];
    if ([self.record[@"focused"] boolValue]) [parts addObject:@"Focused in editor"];
    if ([self.record[@"loading"] boolValue]) [parts addObject:@"Loading children"];
    if ([self.record[@"moreChildren"] boolValue]) [parts addObject:@"Children are not fully loaded"];
    return [parts componentsJoinedByString:@". "];
}
- (BOOL)isAccessibilitySelectorAllowed:(SEL)selector {
    return readOnlySelector(selector) && !(selector==@selector(accessibilityIndex) && self.record[@"index"]==NSNull.null) && [super isAccessibilitySelectorAllowed:selector];
}
- (void)retire { self.record=nil; self.disclosedRows=nil; self.discloser=nil; self.host=nil; }
@end
@implementation HideAXHost
- (BOOL)isFlipped { return YES; }
- (BOOL)acceptsFirstResponder { return NO; }
- (NSView *)hitTest:(NSPoint)point { (void)point; return nil; }
- (BOOL)isAccessibilityElement { return self.rows.count>0; }
- (NSAccessibilityRole)accessibilityRole { return NSAccessibilityOutlineRole; }
- (NSString *)accessibilityLabel { return self.record[@"name"]; }
- (NSString *)accessibilityHelp { return @"Read-only visible sidebar; use editor controls to open or change files."; }
- (NSRect)accessibilityFrame { return screenFrame(self,self.record[@"bounds"]); }
- (NSArray *)accessibilityChildren { return self.rows; }
- (NSArray *)accessibilityRows { return self.rows; }
- (NSArray *)accessibilityVisibleRows {
    NSMutableArray *visible=[NSMutableArray new]; for (HideAXRow *row in self.rows) if (row.record[@"bounds"]!=NSNull.null) [visible addObject:row]; return visible;
}
- (NSArray *)accessibilitySelectedRows {
    NSMutableArray *selected=[NSMutableArray new]; for (HideAXRow *row in self.rows) if (row.isAccessibilitySelected) [selected addObject:row]; return selected;
}
- (BOOL)isAccessibilitySelectorAllowed:(SEL)selector { return readOnlySelector(selector) && [super isAccessibilitySelectorAllowed:selector]; }
@end

void thc_accessibility_close(void) {
    NSView *parent=host.superview;
    for (HideAXRow *row in host.rows) [row retire];
    host.rows=nil; host.nodes=nil; host.record=nil;
    [host removeFromSuperview]; host=nil;
    if (parent) NSAccessibilityPostNotification(parent,NSAccessibilityLayoutChangedNotification);
}
void thc_accessibility_geometry_changed(void) {
    if (host.rows.count) NSAccessibilityPostNotification(host,NSAccessibilityLayoutChangedNotification);
}
int thc_accessibility_update(void *native_window, const char *json, size_t length) {
    if (!NSThread.isMainThread) return 0;
    if (!length) { thc_accessibility_close(); return 1; }
    if (!native_window || !json || length>2097152) goto invalid;
    @autoreleasepool {
        NSDictionary *snapshot=[NSJSONSerialization JSONObjectWithData:[NSData dataWithBytes:json length:length] options:0 error:nil];
        if (![snapshot isKindOfClass:NSDictionary.class] || !boolean(snapshot[@"readOnly"]) || ![snapshot[@"readOnly"] boolValue] || !integer(snapshot[@"revision"],9007199254740991)) goto invalid;
        NSArray *layout=snapshot[@"layout"],*values=snapshot[@"nodes"];
        if (![layout isKindOfClass:NSArray.class] || layout.count!=7 || ![values isKindOfClass:NSArray.class] || values.count>512) goto invalid;
        for (id n in layout) if (!integer(n,9007199254740991)) goto invalid;
        NSInteger cols=[layout[0] integerValue],rows=[layout[1] integerValue];
        if (cols<1 || cols>512 || rows<1 || rows>256 || !integer(snapshot[@"visibleStart"],65536) || !integer(snapshot[@"visibleCount"],256) || !integer(snapshot[@"logicalRows"],65536)) goto invalid;
        NSMutableDictionary<NSArray *,NSDictionary *> *records=[NSMutableDictionary new];
        NSDictionary *root=nil; NSMutableArray<NSDictionary *> *ordered=[NSMutableArray new];
        for (NSDictionary *node in values) {
            if (![node isKindOfClass:NSDictionary.class] || !identity(node[@"id"]) || !text(node[@"name"],256,NO) || !bounds(node[@"bounds"],cols,rows) ||
                !boolean(node[@"selected"]) || !boolean(node[@"focused"]) || !boolean(node[@"loading"]) || !boolean(node[@"moreChildren"]) || !nullableBoolean(node[@"expanded"]) ||
                !integer(node[@"generation"],9007199254740991) || !integer(node[@"level"],65) || !integer(node[@"posInSet"],32768) || !integer(node[@"childrenKnown"],32768) ||
                !([node[@"setSize"] isEqual:@(-1)] || integer(node[@"setSize"],32768)) ||
                !(node[@"index"]==NSNull.null || (integer(node[@"index"],65535) && [node[@"index"] integerValue]<[snapshot[@"logicalRows"] integerValue]))) goto invalid;
            NSArray *key=node[@"id"]; if (records[key]) goto invalid; records[key]=node;
            if ([node[@"role"] isEqual:@"tree"] && [key isEqual:@[@"sidebar"]] && node[@"parent"]==NSNull.null) root=node;
            else if ([node[@"role"] isEqual:@"treeitem"] && [key[0] isEqual:@"tree"] && identity(node[@"parent"]) && [node[@"level"] integerValue]>0 && [node[@"posInSet"] integerValue]>0) [ordered addObject:node];
            else goto invalid;
        }
        if (!ordered.count || ![snapshot[@"visibleCount"] integerValue]) { thc_accessibility_close(); return 1; }
        if (!root || root[@"bounds"]==NSNull.null) goto invalid;
        for (NSDictionary *node in ordered) {
            NSDictionary *parent=records[node[@"parent"]]; NSInteger level=1;
            while (parent && parent!=root && level<=65) { ++level; parent=records[parent[@"parent"]]; }
            if (parent!=root || level!=[node[@"level"] integerValue]) goto invalid;
        }
        [ordered sortUsingComparator:^NSComparisonResult(NSDictionary *a,NSDictionary *b) { NSInteger x=rowIndex(a),y=rowIndex(b); return x<y?NSOrderedAscending:x>y?NSOrderedDescending:NSOrderedSame; }];
        NSWindow *window=(__bridge NSWindow *)native_window;
        if (host && host.window!=window) thc_accessibility_close();
        if (!host) {
            host=[[HideAXHost alloc] initWithFrame:window.contentView.bounds];
            host.autoresizingMask=NSViewWidthSizable|NSViewHeightSizable;
            [window.contentView addSubview:host];
        }
        NSArray *oldSelected=[host.accessibilitySelectedRows copy];
        NSMutableDictionary<NSArray *,HideAXRow *> *nodes=[NSMutableDictionary new]; NSMutableArray<HideAXRow *> *newRows=[NSMutableArray new];
        for (NSDictionary *node in ordered) {
            HideAXRow *row=host.nodes[node[@"id"]] ?: [HideAXRow new];
            row.record=node; row.host=host; row.discloser=nil; row.disclosedRows=@[];
            nodes[node[@"id"]]=row; [newRows addObject:row];
        }
        NSMutableDictionary<NSArray *,NSMutableArray *> *children=[NSMutableDictionary new];
        for (HideAXRow *row in newRows) {
            NSArray *parent=row.record[@"parent"];
            row.discloser=nodes[parent];
            if (!children[parent]) children[parent]=[NSMutableArray new];
            [children[parent] addObject:row];
        }
        for (HideAXRow *row in newRows) row.disclosedRows=children[row.record[@"id"]] ?: @[];
        for (NSArray *key in host.nodes) if (!nodes[key]) [host.nodes[key] retire];
        host.record=root; host.nodes=nodes; host.rows=newRows;
        NSAccessibilityPostNotification(host,NSAccessibilityLayoutChangedNotification);
        if (![oldSelected isEqual:host.accessibilitySelectedRows]) NSAccessibilityPostNotification(host,NSAccessibilitySelectedRowsChangedNotification);
        return 1;
    }
invalid:
    thc_accessibility_close(); return 0;
}
