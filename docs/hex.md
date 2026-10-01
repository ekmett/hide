# Hex editing

Open a binary file as you would a source file. Files containing NUL bytes or
invalid UTF-8 open in hex mode automatically. **Edit > Text / hex mode** switches
a valid text file between its text and byte views without changing its contents.

Each row shows a hexadecimal offset, byte pairs and an ASCII column. Wide windows
show 16 bytes per row; narrower windows show 8. A complete 16-byte row needs 76
window columns including the frame. Split views choose their widths independently,
and resizing retains the selected byte.

## Change bytes

| Action | Keys |
| --- | --- |
| Replace a byte | Type two hexadecimal digits |
| Switch between hex and ASCII entry | Tab |
| Insert a zero byte | Insert |
| Remove bytes | Delete or Backspace |
| Undo / redo | Ctrl+Z / Ctrl+Y |

Copy and Paste use hexadecimal byte pairs. Undo and Redo preserve exact bytes,
including across changes between text and hex mode. Save uses the same external
change checks as text files.

A buffer containing NUL or invalid UTF-8 stays in hex mode until its contents
are valid text. HLS and conversation text-file operations use text buffers.
The hex view is for inspecting and changing bytes; it does not reinterpret them
as numeric values or apply an endian setting.

In the browser, **File > Download** exports the current bytes, including unsaved
changes. See [display and frontends](display.md) for uploads and downloads, and
[editing](editing.md#save-close-and-external-changes) for save conflicts.
