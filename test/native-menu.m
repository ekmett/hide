/* Run tools/check-native.sh on macOS. This owns its application, menus and
 * temporary window; no existing editor session or configuration is touched. */
#import <Cocoa/Cocoa.h>
#include <assert.h>
#include <stdio.h>
#import <objc/runtime.h>

void thc_menu_clear(int, int, int);
void thc_dock_begin(void);
void thc_dock_item(int, const char *, int, int);
void thc_dock_end(void);
void thc_dock_close(void);
void thc_dock_raise(void *);
int thc_dock_generation(void);
void thc_menu_add(const char *);
void thc_menu_item(const char *, const char *, int, int, int);
void thc_menu_separator(void);
void thc_menu_enabled(int, int);
void thc_menu_shortcut(int, const char *, int);
int thc_menu_generation(void);
static int postedCommand, postedGeneration, postedWindow, postedDockGeneration;
void thc_post_window(int ident, int generation) { postedWindow=ident; postedDockGeneration=generation; }
void thc_post_command(int command, int generation) {
    postedCommand = command; postedGeneration = generation;
}
@protocol MenuAction
- (void)invoke:(id)sender;
@end

@interface DockDelegate : NSObject <NSApplicationDelegate>
@property(strong) NSMenu *original;
- (int)preservedBehavior;
@end
@implementation DockDelegate
- (NSMenu *)applicationDockMenu:(NSApplication *)application { (void)application; return self.original; }
- (int)preservedBehavior { return 42; }
@end

int main(void) {
    @autoreleasepool {
        [NSApplication sharedApplication];
        DockDelegate *delegate = [DockDelegate new];
        delegate.original = [NSMenu new];
        [delegate.original addItemWithTitle:@"Existing delegate action" action:nil keyEquivalent:@""];
        [NSApp setDelegate:delegate];
        thc_menu_clear(1, 2, 3);
        assert(NSApp.delegate == delegate);
        assert(class_getSuperclass(object_getClass(delegate)) == [DockDelegate class]);
        assert(class_getInstanceSize(object_getClass(delegate)) == class_getInstanceSize([DockDelegate class]));
        assert([delegate preservedBehavior] == 42);
        int original = thc_menu_generation();
        thc_menu_add("First"); thc_menu_item("Shared", "", 0, 37, 1);
        thc_menu_add("Second"); thc_menu_item("Shared again", "", 0, 37, 1);
        thc_menu_separator();
        NSMenuItem *first = [[NSApp.mainMenu itemAtIndex:1].submenu itemAtIndex:0];
        NSMenuItem *second = [[NSApp.mainMenu itemAtIndex:2].submenu itemAtIndex:0];
        assert(first.tag == 37 && second.tag == 37);
        thc_menu_shortcut(37, "j", 9);
        assert([first.keyEquivalent isEqualToString:@"j"] && [second.keyEquivalent isEqualToString:@"j"]);
        assert(first.keyEquivalentModifierMask == (NSEventModifierFlagCommand | NSEventModifierFlagShift));
        thc_menu_shortcut(37, "k", 2);
        assert(first.keyEquivalentModifierMask == NSEventModifierFlagControl);
        thc_menu_shortcut(37, "", 0);
        assert(first.keyEquivalent.length == 0 && second.keyEquivalent.length == 0);
        thc_menu_shortcut(3, "x", 12);
        NSMenuItem *quit = [[NSApp.mainMenu itemAtIndex:0].submenu itemAtIndex:10];
        assert([quit.keyEquivalent isEqualToString:@"x"] && quit.keyEquivalentModifierMask == (NSEventModifierFlagCommand | NSEventModifierFlagOption));
        thc_menu_enabled(37, 0);
        assert(!first.enabled && !second.enabled);
        thc_menu_enabled(37, 1);
        assert(first.enabled && second.enabled);
        assert([[NSApp.mainMenu itemAtIndex:2].submenu itemAtIndex:1].separatorItem);
        thc_menu_clear(1, 2, 3);
        int replacement = thc_menu_generation();
        assert(replacement > original);
        [(id<MenuAction>)first.target invoke:first];
        assert(postedCommand == 37 && postedGeneration == original);
        thc_menu_add("Replacement"); thc_menu_item("New action", "", 0, 37, 1);
        NSMenuItem *fresh = [[NSApp.mainMenu itemAtIndex:1].submenu itemAtIndex:0];
        [(id<MenuAction>)fresh.target invoke:fresh];
        assert(postedCommand == 37 && postedGeneration == replacement);
        thc_dock_begin(); thc_dock_item(71, "Main.hs", 1, 1); thc_dock_item(93, "Terminal", 0, 1); thc_dock_end();
        NSMenu *dock = [delegate applicationDockMenu:NSApp];
        assert(delegate.original.numberOfItems == 1 && dock.numberOfItems == 5);
        assert([[dock itemAtIndex:0].title isEqualToString:@"Existing delegate action"]);
        assert([[dock itemAtIndex:2].title isEqualToString:@"Editor windows"]);
        NSMenuItem *view = [dock itemAtIndex:3];
        assert(view.tag == 71 && view.state == NSControlStateValueOn && view.enabled);
        int incarnation = thc_dock_generation();
        thc_dock_begin(); thc_dock_item(71, "Renamed.hs", 0, 1); thc_dock_item(93, "Terminal", 1, 1); thc_dock_end();
        assert(thc_dock_generation() == incarnation);
        NSMenu *renamed = [delegate applicationDockMenu:NSApp];
        assert([[renamed itemAtIndex:3].title isEqualToString:@"Renamed.hs"] && [renamed itemAtIndex:4].state == NSControlStateValueOn);
        [(id<MenuAction>)view.target invoke:view];
        assert(postedWindow == 71 && postedDockGeneration == incarnation);
        thc_dock_begin(); thc_dock_item(71, "Renamed.hs", 0, 0); thc_dock_item(93, "Terminal", 1, 0); thc_dock_end();
        postedWindow = 0;
        [(id<MenuAction>)view.target invoke:view];
        assert(postedWindow == 0); /* New modal ownership refuses a queued row. */
        thc_dock_begin(); thc_dock_item(93, "Terminal", 1, 1); thc_dock_end();
        assert(thc_dock_generation() > incarnation);
        [(id<MenuAction>)view.target invoke:view];
        assert(postedWindow == 0); /* Closed stable ID cannot alias a row slot. */
        NSMenuItem *remaining = [[delegate applicationDockMenu:NSApp] itemAtIndex:3];
        [(id<MenuAction>)remaining.target invoke:remaining];
        assert(postedWindow == 93 && postedDockGeneration == thc_dock_generation());
        int connectedGeneration = thc_dock_generation();
        thc_dock_begin(); thc_dock_end(); /* Lost session publishes no available targets. */
        assert([delegate applicationDockMenu:NSApp] == delegate.original);
        thc_dock_begin(); thc_dock_item(93, "Terminal", 1, 1); thc_dock_end();
        assert(thc_dock_generation() > connectedGeneration);
        postedWindow = 0;
        [(id<MenuAction>)remaining.target invoke:remaining];
        assert(postedWindow == 0);
        NSMenuItem *reconnected = [[delegate applicationDockMenu:NSApp] itemAtIndex:3];
        [(id<MenuAction>)reconnected.target invoke:reconnected];
        assert(postedWindow == 93 && postedDockGeneration == thc_dock_generation());
        assert(thc_menu_generation() == replacement); /* Dock updates leave main tokens intact. */
        NSWindow *native = [[NSWindow alloc] initWithContentRect:NSMakeRect(0,0,200,100) styleMask:NSWindowStyleMaskTitled|NSWindowStyleMaskMiniaturizable backing:NSBackingStoreBuffered defer:NO];
        [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
        [NSApp finishLaunching];
        [native makeKeyAndOrderFront:nil]; [native miniaturize:nil];
        NSDate *deadline = [NSDate dateWithTimeIntervalSinceNow:3];
        while (!native.miniaturized && deadline.timeIntervalSinceNow > 0)
            {
                NSEvent *event = [NSApp nextEventMatchingMask:NSEventMaskAny untilDate:[NSDate dateWithTimeIntervalSinceNow:0.01] inMode:NSDefaultRunLoopMode dequeue:YES];
                if (event) [NSApp sendEvent:event];
            }
        assert(native.miniaturized);
        thc_dock_raise((__bridge void *)native);
        deadline = [NSDate dateWithTimeIntervalSinceNow:3];
        while ((native.miniaturized || !native.visible) && deadline.timeIntervalSinceNow > 0)
            {
                NSEvent *event = [NSApp nextEventMatchingMask:NSEventMaskAny untilDate:[NSDate dateWithTimeIntervalSinceNow:0.01] inMode:NSDefaultRunLoopMode dequeue:YES];
                if (event) [NSApp sendEvent:event];
            }
        /* macOS cooperative activation may refuse a background CLI test.
         * Foreground/key focus requires an actual Dock user-gesture smoke. */
        assert(!native.miniaturized && native.visible);
        [native orderOut:nil];
        thc_dock_close();
        assert(object_getClass(delegate) == [DockDelegate class] && NSApp.delegate == delegate);
        assert([delegate applicationDockMenu:NSApp] == delegate.original);
        puts("native Dock delegate, identity, title, modal, restore and teardown checks passed");
        puts("native menu duplicate occurrence and incarnation checks passed");
    }
}
