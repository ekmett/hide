/* Run tools/check-native.sh on macOS. This owns an unshown NSApplication menu;
 * no editor session, window, input or user configuration is touched. */
#import <Cocoa/Cocoa.h>
#include <assert.h>
#include <stdio.h>

void thc_menu_clear(int, int, int);
void thc_menu_add(const char *);
void thc_menu_item(const char *, const char *, int, int);
void thc_menu_separator(void);
void thc_menu_enabled(int, int);
int thc_menu_generation(void);
static int postedCommand, postedGeneration;
void thc_post_command(int command, int generation) {
    postedCommand = command; postedGeneration = generation;
}
@protocol MenuAction
- (void)invoke:(id)sender;
@end

int main(void) {
    @autoreleasepool {
        [NSApplication sharedApplication];
        thc_menu_clear(1, 2, 3);
        int original = thc_menu_generation();
        thc_menu_add("First"); thc_menu_item("Shared", "", 37, 1);
        thc_menu_add("Second"); thc_menu_item("Shared again", "", 37, 1);
        thc_menu_separator();
        NSMenuItem *first = [[NSApp.mainMenu itemAtIndex:1].submenu itemAtIndex:0];
        NSMenuItem *second = [[NSApp.mainMenu itemAtIndex:2].submenu itemAtIndex:0];
        assert(first.tag == 37 && second.tag == 37);
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
        thc_menu_add("Replacement"); thc_menu_item("New action", "", 37, 1);
        NSMenuItem *fresh = [[NSApp.mainMenu itemAtIndex:1].submenu itemAtIndex:0];
        [(id<MenuAction>)fresh.target invoke:fresh];
        assert(postedCommand == 37 && postedGeneration == replacement);
        puts("native menu duplicate occurrence and incarnation checks passed");
    }
}
