#import <Cocoa/Cocoa.h>
#include "window.h"
#include <limits.h>
#include <math.h>
#include <stdlib.h>

@interface THCMenuTarget : NSObject
- (void)invoke:(id)sender;
@end
@implementation THCMenuTarget
- (void)invoke:(id)sender { thc_post_command((int)[sender tag], [[sender representedObject] intValue]); }
@end
static THCMenuTarget *target;
static NSMenu *current;
static NSMutableDictionary<NSNumber *, NSMutableArray<NSMenuItem *> *> *items;
static int generation;
int thc_menu_generation(void) { return generation; }

/* Set before SDL creates NSApplication: unbundled launches otherwise inherit
 * the executable name for the application menu. */
void thc_menu_prepare(void) { [[NSProcessInfo processInfo] setProcessName:@"Haskell"]; }

static NSEventModifierFlags shortcutModifiers(int mods) {
    return ((mods & 1) ? NSEventModifierFlagShift : 0) |
           ((mods & 2) ? NSEventModifierFlagControl : 0) |
           ((mods & 4) ? NSEventModifierFlagOption : 0) |
           ((mods & 8) ? NSEventModifierFlagCommand : 0);
}
static NSMenuItem *commandItem(NSString *title, NSString *shortcut, int mods, int command, int enabled) {
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:title action:@selector(invoke:) keyEquivalent:shortcut];
    [item setKeyEquivalentModifierMask:shortcutModifiers(mods)];
    [item setTarget:target]; [item setTag:command]; [item setEnabled:enabled];
    [item setRepresentedObject:@(generation)];
    if (!items[@(command)]) items[@(command)] = [NSMutableArray new];
    [items[@(command)] addObject:item];
    return item;
}

static void installDockDelegate(void);

void thc_menu_clear(int about, int settings, int quit) {
    if (generation == INT_MAX) abort(); /* Never reuse a queued event incarnation. */
    ++generation;
    installDockDelegate();
    if (!target) target = [THCMenuTarget new];
    items = [NSMutableDictionary new];
    NSMenu *bar = [NSMenu new];
    NSMenuItem *appItem = [[NSMenuItem alloc] initWithTitle:@"Haskell" action:nil keyEquivalent:@""];
    NSMenu *appMenu = [[NSMenu alloc] initWithTitle:@"Haskell"];
    [appMenu setAutoenablesItems:NO];
    [appMenu addItem:commandItem(@"About Haskell", @"", 0, about, 1)];
    [appMenu addItem:[NSMenuItem separatorItem]];
    [appMenu addItem:commandItem(@"Settings…", @"", 0, settings, 1)];
    [appMenu addItem:[NSMenuItem separatorItem]];
    NSMenu *services = [[NSMenu alloc] initWithTitle:@"Services"];
    NSMenuItem *servicesItem = [appMenu addItemWithTitle:@"Services" action:nil keyEquivalent:@""];
    [servicesItem setSubmenu:services];
    [NSApp setServicesMenu:services];
    [appMenu addItem:[NSMenuItem separatorItem]];
    [appMenu addItemWithTitle:@"Hide Haskell" action:@selector(hide:) keyEquivalent:@"h"];
    NSMenuItem *hideOthers = [appMenu addItemWithTitle:@"Hide Others" action:@selector(hideOtherApplications:) keyEquivalent:@"h"];
    [hideOthers setKeyEquivalentModifierMask:NSEventModifierFlagCommand | NSEventModifierFlagOption];
    [appMenu addItemWithTitle:@"Show All" action:@selector(unhideAllApplications:) keyEquivalent:@""];
    [appMenu addItem:[NSMenuItem separatorItem]];
    [appMenu addItem:commandItem(@"Quit Haskell", @"", 0, quit, 1)];
    [appItem setSubmenu:appMenu];
    [bar addItem:appItem];
    [NSApp setMainMenu:bar];
}
void thc_menu_add(const char *title) {
    NSString *name = [NSString stringWithUTF8String:title];
    current = [[NSMenu alloc] initWithTitle:name];
    [current setAutoenablesItems:NO];
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:name action:nil keyEquivalent:@""];
    [item setSubmenu:current];
    [[NSApp mainMenu] addItem:item];
}
void thc_menu_separator(void) { [current addItem:[NSMenuItem separatorItem]]; }
void thc_menu_item(const char *title, const char *key, int mods, int command, int enabled) {
    [current addItem:commandItem([NSString stringWithUTF8String:title], [NSString stringWithUTF8String:key], mods, command, enabled)];
}
void thc_menu_enabled(int command, int enabled) {
    for (NSMenuItem *item in items[@(command)]) [item setEnabled:enabled];
}

void thc_menu_shortcut(int command, const char *key, int mods) {
    for (NSMenuItem *item in items[@(command)]) {
        [item setKeyEquivalent:[NSString stringWithUTF8String:key]];
        [item setKeyEquivalentModifierMask:shortcutModifiers(mods)];
    }
}

/* Only this delegate instance gains the Dock hook. SDL still owns the instance,
 * inherited launch/open-file/quit callbacks, ivars and notification behavior. */
#import <objc/runtime.h>
static __weak id dockDelegate;
static Class dockOriginalClass, dockDelegateClass;
static NSArray<NSDictionary *> *dockWindows;
static NSMutableArray<NSDictionary *> *pendingDockWindows;
static int dockGeneration;
int thc_dock_generation(void) { return dockGeneration; }

@interface THCDockTarget : NSObject
- (void)invoke:(NSMenuItem *)sender;
- (BOOL)validateMenuItem:(NSMenuItem *)sender;
@end
@implementation THCDockTarget
- (BOOL)validateMenuItem:(NSMenuItem *)sender {
    if ([sender.representedObject intValue] != dockGeneration) return NO;
    for (NSDictionary *window in dockWindows)
        if ([window[@"id"] intValue] == sender.tag) return [window[@"enabled"] boolValue];
    return NO;
}
- (void)invoke:(NSMenuItem *)sender {
    if (!sender.enabled || [sender.representedObject intValue] != dockGeneration) return;
    for (NSDictionary *window in dockWindows) {
        if ([window[@"id"] intValue] == sender.tag && [window[@"enabled"] boolValue]) {
            thc_post_window((int)sender.tag, dockGeneration);
            return;
        }
    }
}
@end
static THCDockTarget *dockTarget;
static NSMenu *dockMenu(id delegate, SEL selector, NSApplication *application) {
    NSMenu *original = nil;
    if (class_getInstanceMethod(dockOriginalClass, selector)) {
        NSMenu *(*inherited)(id,SEL,NSApplication *) = (void *)class_getMethodImplementation(dockOriginalClass, selector);
        original = inherited(delegate, selector, application);
    }
    if (!dockWindows.count) return original;
    NSMenu *menu = original ? [original copy] : [NSMenu new];
    if (!original) [menu setAutoenablesItems:NO];
    if (menu.numberOfItems) [menu addItem:[NSMenuItem separatorItem]];
    NSMenuItem *heading = [menu addItemWithTitle:@"Editor windows" action:nil keyEquivalent:@""];
    heading.enabled = NO;
    for (NSDictionary *window in dockWindows) {
        NSMenuItem *item = [menu addItemWithTitle:window[@"title"] action:@selector(invoke:) keyEquivalent:@""];
        item.target = dockTarget;
        item.tag = [window[@"id"] intValue];
        item.representedObject = @(dockGeneration);
        item.enabled = [window[@"enabled"] boolValue];
        item.state = [window[@"selected"] boolValue] ? NSControlStateValueOn : NSControlStateValueOff;
    }
    return menu;
}
static void installDockDelegate(void) {
    id delegate = NSApp.delegate;
    if (!delegate || delegate == dockDelegate) return;
    if (dockDelegate) thc_dock_close();
    dockDelegate = delegate;
    dockOriginalClass = object_getClass(delegate);
    NSString *name = [@"THCDock_" stringByAppendingString:NSStringFromClass(dockOriginalClass)];
    dockDelegateClass = NSClassFromString(name);
    if (!dockDelegateClass) {
        dockDelegateClass = objc_allocateClassPair(dockOriginalClass, name.UTF8String, 0);
        if (!dockDelegateClass || !class_addMethod(dockDelegateClass, @selector(applicationDockMenu:), (IMP)dockMenu, "@@:@")) abort();
        objc_registerClassPair(dockDelegateClass);
    }
    if (class_getInstanceSize(dockDelegateClass) != class_getInstanceSize(dockOriginalClass)) abort();
    object_setClass(delegate, dockDelegateClass);
    /* Refresh optional delegate-method discovery without replacing SDL's owner. */
    [NSApp setDelegate:nil];
    [NSApp setDelegate:delegate];
    if (!dockTarget) dockTarget = [THCDockTarget new];
}
void thc_dock_begin(void) { installDockDelegate(); pendingDockWindows = [NSMutableArray new]; }
void thc_dock_item(int ident, const char *title, int selected, int enabled) {
    [pendingDockWindows addObject:@{@"id":@(ident), @"title":[NSString stringWithUTF8String:title], @"selected":@(selected), @"enabled":@(enabled)}];
}
void thc_dock_end(void) {
    /* Title/focus updates leave queued actions valid; list identities retire it. */
    if (![[dockWindows valueForKey:@"id"] isEqual:[pendingDockWindows valueForKey:@"id"]]) {
        if (dockGeneration == INT_MAX) abort();
        ++dockGeneration;
    }
    dockWindows = [pendingDockWindows copy]; pendingDockWindows = nil;
}
void thc_dock_close(void) {
    id delegate = dockDelegate;
    if (delegate && object_getClass(delegate) == dockDelegateClass) {
        BOOL currentDelegate = NSApp.delegate == delegate;
        if (currentDelegate) [NSApp setDelegate:nil];
        object_setClass(delegate, dockOriginalClass);
        if (currentDelegate) [NSApp setDelegate:delegate];
    }
    dockDelegate = nil; dockOriginalClass = Nil; dockDelegateClass = Nil;
    dockWindows = nil; pendingDockWindows = nil;
    if (dockGeneration == INT_MAX) abort();
    ++dockGeneration;
}
void thc_dock_raise(void *nativeWindow) {
    NSWindow *window = (__bridge NSWindow *)nativeWindow;
    if (window.miniaturized) [window deminiaturize:nil];
    [window makeKeyAndOrderFront:nil];
    if (@available(macOS 14.0, *)) [NSApp activate];
    else [NSApp activateIgnoringOtherApps:YES];
}

/* Cocoa owns file dragging; it must start from a real mouse gesture rather than
 * an editor-input request. An armed row expires on the next unrelated click. */
@interface HideFileDragSource : NSObject <NSDraggingSource>
@end
@implementation HideFileDragSource
- (NSDragOperation)draggingSession:(NSDraggingSession *)session sourceOperationMaskForDraggingContext:(NSDraggingContext)context {
    (void)session; (void)context; return NSDragOperationCopy;
}
- (BOOL)ignoreModifierKeysForDraggingSession:(NSDraggingSession *)session { (void)session; return YES; }
- (void)draggingSession:(NSDraggingSession *)session endedAtPoint:(NSPoint)point operation:(NSDragOperation)operation {
    (void)session; (void)point; (void)operation; thc_file_drag_ended();
}
@end
static id fileDragMonitor;
static HideFileDragSource *fileDragSource;
void thc_file_drag_close(void) {
    if (fileDragMonitor) [NSEvent removeMonitor:fileDragMonitor];
    fileDragMonitor=nil;
}
void thc_file_drag_arm(void *native_window, const char *path, double x, double y, double width, double height) {
    thc_file_drag_close();
    NSWindow *window=(__bridge NSWindow *)native_window;
    NSView *view=window.contentView;
    NSURL *url=[NSURL fileURLWithPath:[NSString stringWithUTF8String:path]];
    NSRect row=NSMakeRect(x,y,width,height);
    __block BOOL pressed=NO;
    __block NSPoint start;
    if (!fileDragSource) fileDragSource=[HideFileDragSource new];
    fileDragMonitor=[NSEvent addLocalMonitorForEventsMatchingMask:(NSEventMaskLeftMouseDown|NSEventMaskLeftMouseDragged|NSEventMaskLeftMouseUp|NSEventMaskKeyDown) handler:^NSEvent *(NSEvent *event) {
        if (event.type==NSEventTypeKeyDown || event.window!=window) { thc_file_drag_close(); return event; }
        NSPoint point=[view convertPoint:event.locationInWindow fromView:nil];
        if (!view.isFlipped) point.y=view.bounds.size.height-point.y;
        if (event.type==NSEventTypeLeftMouseDown) {
            pressed=NSPointInRect(point,row); start=point;
            if (!pressed) thc_file_drag_close();
        } else if (event.type==NSEventTypeLeftMouseUp) {
            thc_file_drag_close();
        } else if (pressed && hypot(point.x-start.x,point.y-start.y)>=5) {
            BOOL directory=NO;
            if (![[NSFileManager defaultManager] fileExistsAtPath:url.path isDirectory:&directory] || directory) { thc_file_drag_close(); return event; }
            NSDraggingItem *item=[[NSDraggingItem alloc] initWithPasteboardWriter:url];
            NSImage *icon=[[NSWorkspace sharedWorkspace] iconForFile:url.path];
            NSPoint anchor=[view convertPoint:event.locationInWindow fromView:nil];
            [item setDraggingFrame:NSMakeRect(anchor.x-16,anchor.y-16,32,32) contents:icon];
            thc_file_drag_close();
            [view beginDraggingSessionWithItems:@[item] event:event source:fileDragSource];
            return nil;
        }
        return event;
    }];
}
