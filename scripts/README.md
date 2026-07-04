# Miscellaneous scripts related to the project

## Gource visualization

```console
% mkdir /tmp/images
% vi scripts/github-name-map
% scripts/github-fetch-images \
    https://github.com/ekmett/lens/graphs/contributors-data \
    scripts/github-name-map /tmp/images
% scripts/run-gource /tmp/images audio.ogg /tmp/lens-gource-"$(date +%Y%m%d)"
```

* `github-fetch-images` downloads avatar images for committers based on
  `github-name-map`. (TODO: list cabal dependencies; reduce them as well.)
* `run-gource` runs [Gource][], [avconv][] and [MP4Box][] to generate a video.
  The audio input file must be at least as long as the resulting video. (TODO:
  can one make avconv loop the audio?)

[Gource]: http://code.google.com/p/gource/
[avconv]: http://libav.org/avconv.html
[MP4Box]: http://gpac.wp.mines-telecom.fr/mp4box/

## Spellcheck

```console
% scripts/spellcheck
```

* `spellcheck` grabs a list of files in the repository using Git and runs
  [Aspell][] against them. (TODO: only spellcheck comments from Haskell files.)

[Aspell]: http://aspell.net/

## IRC Topic Stats

```console
% scripts/stats
```

* Generates a fragment for the `#haskell-lens` channel topic summarising the
  number of `unsafeCoerce`s, doctests, operators and modules.

##

```console
% scripts/operators
```

* Generate a list of operators defined under `src/Control/Lens/`, for
  pasting into the `hiding` clause of `Control.Lens.Combinators` and the
  export section of `Control.Lens.Operators`.

## Class-hierarchy diagram

```console
% brew install graphviz   # provides `dot`
% scripts/hierarchy        # regenerates images/Hierarchy.png from images/Hierarchy.dot
```

* The diagram's source of truth is `images/Hierarchy.dot` (Graphviz). Each optic
  is one box, and each combinator is one `<TR>` row in that box's HTML-label
  table, so correcting or adding a signature is a one-line edit. Re-run
  `scripts/hierarchy` and commit the regenerated `images/Hierarchy.png` together
  with the `.dot` change.
* Release note: the Hackage package description (`description:` in `lens.cabal`)
  embeds this PNG from a release-tagged raw URL (e.g. `.../v5.3.7/...`), since
  Hackage can't render a repo-relative image there. Bump that tag in lockstep
  with `version:` at release time so each release's page shows its own diagram.
