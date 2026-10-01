# Build this site

The documentation uses the same navigation, typography and light/dark appearance
controls as [THC's documentation](https://ekmett.github.io/thc/). Its standalone
Haskell generator renders an explicit list of editor guides with Pandoc and
checks the resulting links before publication.

## Local build

Install GHC, Cabal, Pandoc and Make, then run from the repository root:

```sh
make docs
```

Open `build/site/index.html`, or serve the generated directory with a local web
server. The output also works below the GitHub Pages project path `/thc-edit/`.
All CSS, JavaScript and fonts used by the theme are local files or system fonts;
the site does not load a third-party script or font service.

The generator has its own package in `tools/docs/` and uses `build/docs/cabal/`
for compiled output. It does not build the editor, its native graphics or
terminal backends, THC, or Java. Cabal may download the generator's Haskell
dependencies on the first build. Pandoc must be on `PATH`, or set `PANDOC` to its
executable path; `CABAL` may similarly select a Cabal executable.

To check an already generated site again:

```sh
make docs-check
```

The check covers local links, fragments, required pages, shared theme assets and
revision metadata. Missing targets fail the build. Generated files belong in
`build/` and are not committed.

## What gets published

`tools/docs/Main.hs` lists the published guides explicitly. Adding a guide to
that list adds it to the navigation and to link checking. Relative Markdown
links to another listed guide become site links. Links to other repository
files, packaged skills, design records and implementation plans become GitHub
links pinned to the build's full commit ID; those directories are not copied
into the site.

Every page carries the revision and links to its source at that exact commit.
`DOCS_REVISION` defaults to the checkout's `HEAD`; an explicit value must match
it. Local edits appear in a preview, but commit them before publishing so the
source links describe the same content.

## GitHub Pages

The [documentation workflow](../../.github/workflows/docs.yml) runs on pushes to
`main` and can also be dispatched manually on `main`. It checks out the exact
triggering revision, runs `make docs`, and uploads only `build/site` as the Pages
artifact. Deployment runs only after that build and link check succeed.

In the repository's Pages settings, choose **GitHub Actions** as the build and
deployment source. The published site is
[ekmett.github.io/thc-edit](https://ekmett.github.io/thc-edit/).

The framework was adapted from
[THC's documentation generator](https://github.com/ekmett/thc/blob/8dec8726eae35fee25f3d22d743052a73dc06649/src/tools/docs/Main.hs).
