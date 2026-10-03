#!/bin/sh
# Run from the repository root. No editor, network or user settings are touched.
set -eu
scratch=$(mktemp -d "${TMPDIR:-/tmp}/hide-launchers.XXXXXX")
trap 'rm -rf "$scratch"' EXIT HUP INT TERM
mkdir "$scratch/install space" "$scratch/aliases"
cp bin/thc-edit "$scratch/install space/thc-edit"
cp bin/th "$scratch/install space/th"
cat > "$scratch/install space/hide" <<'PROGRAM'
#!/bin/sh
printf '%s\n' "$#" "$@"
exit 23
PROGRAM
chmod +x "$scratch/install space/"*
ln -s '../install space/thc-edit' "$scratch/aliases/editor"
printf '%s\n' 4 'file with spaces.hs' '' '--metal' '$(not a command)' > "$scratch/expected"
export PATH="$scratch/install space:$PATH"
for launcher in "$scratch/install space/thc-edit" "$scratch/install space/th" "$scratch/aliases/editor" thc-edit th; do
  result=0
  "$launcher" 'file with spaces.hs' '' '--metal' '$(not a command)' > "$scratch/actual" || result=$?
  test "$result" = 23
  cmp "$scratch/expected" "$scratch/actual"
done
printf 'Adjacent launcher, PATH, symlink, argument and exit-status checks passed\n'
