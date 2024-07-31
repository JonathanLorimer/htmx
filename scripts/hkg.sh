set -u # or set -o nounset
set -x
set -e
: "$HACKAGE_USERNAME"
: "$HACKAGE_PASSWORD"

# Upload package
SDIST=$(cabal sdist | tail -1)
cabal upload -u "$HACKAGE_USERNAME" -p "$HACKAGE_PASSWORD" --publish "$SDIST"
rm "$SDIST"

# Upload docs
DOCS=$(cabal haddock --haddock-html-location='https://hackage.haskell.org/package/$pkg-$version/docs' --haddock-hyperlink-source --haddock-quickjump --haddock-for-hackage --verbose=1 | tail -1)
cabal upload -d -u "$HACKAGE_USERNAME" -p "$HACKAGE_PASSWORD" --publish "$DOCS"
rm "$DOCS"
