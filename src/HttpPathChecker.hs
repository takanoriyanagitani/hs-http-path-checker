module HttpPathChecker (
    UriLike (..),
    PathLike (..),
    RawPath (..),
    HasValidPrefix,
    StringChecker,
    PrefixChecker,
    urilike2err,
    convert2uri,
    strStartsWith,
    pathlike2err,
    pathlike2raw,
    newPathStringChecker,
    defaultPrefix,
    pathStringChecker,
    newPrefixChecker,
    prefixChecker,
    pathlike2checker2raw,
    pathlike2rawDefault,
    raw2urilike,
    prefixDefault,
    raw2urilikeDefault,
    urilike2uri,
    raw2uri,
    pathlike2uri,
    Path (..),
    pathlike2path,
    str2path,
) where

import qualified Network.URI as U

import Data.List (isPrefixOf)

type StringChecker = String -> Bool

strStartsWith :: Char -> StringChecker
strStartsWith c s = [c] `isPrefixOf` s

newtype UriLike = UriLike {getUriLike :: String}

urilike2err :: UriLike -> String
urilike2err ulike = "invalid uri: " ++ getUriLike ulike

convert2uri :: UriLike -> Maybe U.URI -> Either String U.URI
convert2uri ulike Nothing = Left (urilike2err ulike)
convert2uri _ (Just u) = Right u

urilike2uri :: UriLike -> Either String U.URI
urilike2uri ulike = convert2uri ulike (U.parseURI (getUriLike ulike))

newtype PathLike = PathLike {getPathLike :: String}

pathlike2err :: PathLike -> String
pathlike2err (PathLike plike) = "invalid path: " ++ plike

newtype RawPath = RawPath {getRawPath :: String}

newtype HasValidPrefix = HasValidPrefix Bool

pathlike2raw :: PathLike -> HasValidPrefix -> Either String RawPath
pathlike2raw pl (HasValidPrefix True) = Right (RawPath (getPathLike pl))
pathlike2raw pl (HasValidPrefix False) = Left (pathlike2err pl)

type PrefixChecker = PathLike -> HasValidPrefix

newPathStringChecker :: Char -> StringChecker
newPathStringChecker = strStartsWith

defaultPrefix :: Char
defaultPrefix = '/'

pathStringChecker :: StringChecker
pathStringChecker = newPathStringChecker defaultPrefix

newPrefixChecker :: StringChecker -> PrefixChecker
newPrefixChecker schk plike = HasValidPrefix (schk (getPathLike plike))

prefixChecker :: PrefixChecker
prefixChecker = newPrefixChecker pathStringChecker

pathlike2checker2raw :: PrefixChecker -> PathLike -> Either String RawPath
pathlike2checker2raw chk pat = pathlike2raw pat (chk pat)

pathlike2rawDefault :: PathLike -> Either String RawPath
pathlike2rawDefault = pathlike2checker2raw prefixChecker

raw2urilike :: String -> RawPath -> UriLike
raw2urilike prefix raw = UriLike (prefix ++ getRawPath raw)

prefixDefault :: String
prefixDefault = "http://localhost"

raw2urilikeDefault :: RawPath -> UriLike
raw2urilikeDefault = raw2urilike prefixDefault

raw2uri :: RawPath -> Either String U.URI
raw2uri = urilike2uri . raw2urilikeDefault

pathlike2uri :: PathLike -> Either String U.URI
pathlike2uri plike = do
    rpath :: RawPath <- pathlike2rawDefault plike
    raw2uri rpath

newtype Path = Path String
    deriving (Show)

pathlike2path :: PathLike -> Either String Path
pathlike2path plike = do
    _uri :: U.URI <- pathlike2uri plike
    let raw :: String = getPathLike plike
    return (Path raw)

str2path :: String -> Either String Path
str2path s = pathlike2path (PathLike s)
