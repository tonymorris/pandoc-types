{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric,
FlexibleContexts, GeneralizedNewtypeDeriving, PatternGuards, CPP, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, RankNTypes #-}

{-
Copyright (c) 2006-2016, John MacFarlane

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of John MacFarlane nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{- |
   Module      : Text.Pandoc.Definition
   Copyright   : Copyright (C) 2006-2016 John MacFarlane
   License     : BSD3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Definition of 'Pandoc' data structure for format-neutral representation
of documents.
-}
module Text.Pandoc.Definition ( Pandoc(..)
                              , Meta(..)
                              , MetaValue(..)
                              , nullMeta
                              , isNullMeta
                              , lookupMeta
                              , docTitle
                              , docAuthors
                              , docDate
                              , Block(..)
                              , Inline(..)
                              , Alignment(..)
                              , ListAttributes
                              , ListNumberStyle(..)
                              , ListNumberDelim(..)
                              , Format(..)
                              , Attr
                              , nullAttr
                              , TableCell
                              , QuoteType(..)
                              , Target
                              , MathType(..)
                              , Citation(..)
                              , CitationMode(..)
                              , pandocTypesVersion
                              ) where

import Control.Lens hiding ((.=))
import Data.Generics (Data, Typeable)
import Data.Ord (comparing)
import Data.Aeson hiding (Null)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as M
import GHC.Generics (Generic)
import Data.String
import Data.Char (toLower)
#if MIN_VERSION_base(4,8,0)
import Control.DeepSeq
#else
import Data.Monoid
import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq.Generics
#endif
import Paths_pandoc_types (version)
import Data.Version (Version, versionBranch)

data Pandoc = Pandoc Meta [Block]
              deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance Monoid Pandoc where
  mempty = Pandoc mempty mempty
  (Pandoc m1 bs1) `mappend` (Pandoc m2 bs2) =
    Pandoc (m1 `mappend` m2) (bs1 `mappend` bs2)

-- | Metadata for the document:  title, authors, date.
newtype Meta = Meta { unMeta :: M.Map String MetaValue }
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

instance Monoid Meta where
  mempty = Meta (M.empty)
  (Meta m1) `mappend` (Meta m2) = Meta (M.union m1 m2)
  -- note: M.union is left-biased, so if there are fields in both m1
  -- and m2, m1 wins.

data MetaValue = MetaMap (M.Map String MetaValue)
               | MetaList [MetaValue]
               | MetaBool Bool
               | MetaString String
               | MetaInlines [Inline]
               | MetaBlocks [Block]
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

nullMeta :: Meta
nullMeta = Meta M.empty

isNullMeta :: Meta -> Bool
isNullMeta (Meta m) = M.null m

-- Helper functions to extract metadata

-- | Retrieve the metadata value for a given @key@.
lookupMeta :: String -> Meta -> Maybe MetaValue
lookupMeta key (Meta m) = M.lookup key m

-- | Extract document title from metadata; works just like the old @docTitle@.
docTitle :: Meta -> [Inline]
docTitle meta =
  case lookupMeta "title" meta of
         Just (MetaString s)           -> [Str s]
         Just (MetaInlines ils)        -> ils
         Just (MetaBlocks [Plain ils]) -> ils
         Just (MetaBlocks [Para ils])  -> ils
         _                             -> []

-- | Extract document authors from metadata; works just like the old
-- @docAuthors@.
docAuthors :: Meta -> [[Inline]]
docAuthors meta =
  case lookupMeta "author" meta of
        Just (MetaString s)    -> [[Str s]]
        Just (MetaInlines ils) -> [ils]
        Just (MetaList   ms)   -> [ils | MetaInlines ils <- ms] ++
                                  [ils | MetaBlocks [Plain ils] <- ms] ++
                                  [ils | MetaBlocks [Para ils]  <- ms] ++
                                  [[Str x] | MetaString x <- ms]
        _                      -> []

-- | Extract date from metadata; works just like the old @docDate@.
docDate :: Meta -> [Inline]
docDate meta =
  case lookupMeta "date" meta of
         Just (MetaString s)           -> [Str s]
         Just (MetaInlines ils)        -> ils
         Just (MetaBlocks [Plain ils]) -> ils
         Just (MetaBlocks [Para ils])  -> ils
         _                             -> []

-- | Alignment of a table column.
data Alignment = AlignLeft
               | AlignRight
               | AlignCenter
               | AlignDefault deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | List attributes.
type ListAttributes = (Int, ListNumberStyle, ListNumberDelim)

-- | Style of list numbers.
data ListNumberStyle = DefaultStyle
                     | Example
                     | Decimal
                     | LowerRoman
                     | UpperRoman
                     | LowerAlpha
                     | UpperAlpha deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Delimiter of list numbers.
data ListNumberDelim = DefaultDelim
                     | Period
                     | OneParen
                     | TwoParens deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Attributes: identifier, classes, key-value pairs
type Attr = (String, [String], [(String, String)])

nullAttr :: Attr
nullAttr = ("",[],[])

-- | Table cells are list of Blocks
type TableCell = [Block]

-- | Formats for raw blocks
newtype Format = Format String
               deriving (Read, Show, Typeable, Data, Generic, ToJSON, FromJSON)

instance IsString Format where
  fromString f = Format $ map toLower f

instance Eq Format where
  Format x == Format y = map toLower x == map toLower y

instance Ord Format where
  compare (Format x) (Format y) = compare (map toLower x) (map toLower y)

-- | Block element.
data Block
    = Plain [Inline]        -- ^ Plain text, not a paragraph
    | Para [Inline]         -- ^ Paragraph
    | LineBlock [[Inline]]  -- ^ Multiple non-breaking lines
    | CodeBlock Attr String -- ^ Code block (literal) with attributes
    | RawBlock Format String -- ^ Raw block
    | BlockQuote [Block]    -- ^ Block quote (list of blocks)
    | OrderedList ListAttributes [[Block]] -- ^ Ordered list (attributes
                            -- and a list of items, each a list of blocks)
    | BulletList [[Block]]  -- ^ Bullet list (list of items, each
                            -- a list of blocks)
    | DefinitionList [([Inline],[[Block]])]  -- ^ Definition list
                            -- Each list item is a pair consisting of a
                            -- term (a list of inlines) and one or more
                            -- definitions (each a list of blocks)
    | Header Int Attr [Inline] -- ^ Header - level (integer) and text (inlines)
    | HorizontalRule        -- ^ Horizontal rule
    | Table [Inline] [Alignment] [Double] [TableCell] [[TableCell]]  -- ^ Table,
                            -- with caption, column alignments (required),
                            -- relative column widths (0 = default),
                            -- column headers (each a list of blocks), and
                            -- rows (each a list of lists of blocks)
    | Div Attr [Block]      -- ^ Generic block container with attributes
    | Null                  -- ^ Nothing
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | Type of quotation marks to use in Quoted inline.
data QuoteType = SingleQuote | DoubleQuote deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | Link target (URL, title).
type Target = (String, String)

-- | Type of math element (display or inline).
data MathType = DisplayMath | InlineMath deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | Inline elements.
data Inline
    = Str String            -- ^ Text (string)
    | Emph [Inline]         -- ^ Emphasized text (list of inlines)
    | Strong [Inline]       -- ^ Strongly emphasized text (list of inlines)
    | Strikeout [Inline]    -- ^ Strikeout text (list of inlines)
    | Superscript [Inline]  -- ^ Superscripted text (list of inlines)
    | Subscript [Inline]    -- ^ Subscripted text (list of inlines)
    | SmallCaps [Inline]    -- ^ Small caps text (list of inlines)
    | Quoted QuoteType [Inline] -- ^ Quoted text (list of inlines)
    | Cite [Citation]  [Inline] -- ^ Citation (list of inlines)
    | Code Attr String      -- ^ Inline code (literal)
    | Space                 -- ^ Inter-word space
    | SoftBreak             -- ^ Soft line break
    | LineBreak             -- ^ Hard line break
    | PageBreak             -- ^ Force new page
    | Math MathType String  -- ^ TeX math (literal)
    | RawInline Format String -- ^ Raw inline
    | Link Attr [Inline] Target  -- ^ Hyperlink: alt text (list of inlines), target
    | Image Attr [Inline] Target -- ^ Image:  alt text (list of inlines), target
    | Note [Block]          -- ^ Footnote or endnote
    | Span Attr [Inline]    -- ^ Generic inline container with attributes
    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

data Citation = Citation { _citationId      :: String
                         , _citationPrefix  :: [Inline]
                         , _citationSuffix  :: [Inline]
                         , _citationMode    :: CitationMode
                         , _citationNoteNum :: Int
                         , _citationHash    :: Int
                         }
                deriving (Show, Eq, Read, Typeable, Data, Generic)

instance Ord Citation where
    compare = comparing _citationHash

data CitationMode = AuthorInText | SuppressAuthor | NormalCitation
                    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)


-- ToJSON/FromJSON instances. We do this by hand instead of deriving
-- from generics, so we can have more control over the format.

taggedNoContent :: [Char] -> Value
taggedNoContent x = object [ "t" .= x ]

tagged :: ToJSON a => [Char] -> a -> Value
tagged x y = object [ "t" .= x, "c" .= y ]

instance FromJSON MetaValue where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "MetaMap"     -> MetaMap     <$> (v .: "c")
      "MetaList"    -> MetaList    <$> (v .: "c")
      "MetaBool"    -> MetaBool    <$> (v .: "c")
      "MetaString"  -> MetaString  <$> (v .: "c")
      "MetaInlines" -> MetaInlines <$> (v .: "c")
      "MetaBlocks"  -> MetaBlocks  <$> (v .: "c")
      _ -> mempty
  parseJSON _ = mempty
instance ToJSON MetaValue where
  toJSON (MetaMap mp) = tagged "MetaMap" mp
  toJSON (MetaList lst) = tagged "MetaList" lst
  toJSON (MetaBool bool) = tagged "MetaBool" bool
  toJSON (MetaString s) = tagged "MetaString" s
  toJSON (MetaInlines ils) = tagged "MetaInlines" ils
  toJSON (MetaBlocks blks) = tagged "MetaBlocks" blks

instance FromJSON Meta where
  parseJSON j = Meta <$> parseJSON j
instance ToJSON Meta where
  toJSON meta = toJSON $ unMeta meta

instance FromJSON CitationMode where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "AuthorInText"   -> return AuthorInText
      "SuppressAuthor" -> return SuppressAuthor
      "NormalCitation" -> return NormalCitation
      _ -> mempty
  parseJSON _ = mempty
instance ToJSON CitationMode where
  toJSON cmode = taggedNoContent s
    where s = case cmode of
            AuthorInText   -> "AuthorInText"
            SuppressAuthor -> "SuppressAuthor"
            NormalCitation -> "NormalCitation"


instance FromJSON Citation where
  parseJSON (Object v) = do
    citationId'      <- v .: "citationId"
    citationPrefix'  <- v .: "citationPrefix"
    citationSuffix'  <- v .: "citationSuffix"
    citationMode'    <- v .: "citationMode"
    citationNoteNum' <- v .: "citationNoteNum"
    citationHash'    <- v .: "citationHash"
    return Citation { _citationId = citationId'
                    , _citationPrefix = citationPrefix'
                    , _citationSuffix = citationSuffix'
                    , _citationMode = citationMode'
                    , _citationNoteNum = citationNoteNum'
                    , _citationHash = citationHash'
                    }
  parseJSON _ = mempty
instance ToJSON Citation where
  toJSON cit =
    object [ "citationId"      .= _citationId cit
           , "citationPrefix"  .= _citationPrefix cit
           , "citationSuffix"  .= _citationSuffix cit
           , "citationMode"    .= _citationMode cit
           , "citationNoteNum" .= _citationNoteNum cit
           , "citationHash"    .= _citationHash cit
           ]

instance FromJSON QuoteType where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "SingleQuote" -> return SingleQuote
      "DoubleQuote" -> return DoubleQuote
      _                    -> mempty
  parseJSON _ = mempty
instance ToJSON QuoteType where
  toJSON qtype = taggedNoContent s
    where s = case qtype of
            SingleQuote -> "SingleQuote"
            DoubleQuote -> "DoubleQuote"


instance FromJSON MathType where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "DisplayMath" -> return DisplayMath
      "InlineMath"  -> return InlineMath
      _                    -> mempty
  parseJSON _ = mempty
instance ToJSON MathType where
  toJSON mtype = taggedNoContent s
    where s = case mtype of
            DisplayMath -> "DisplayMath"
            InlineMath  -> "InlineMath"

instance FromJSON ListNumberStyle where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "DefaultStyle" -> return DefaultStyle
      "Example"      -> return Example
      "Decimal"      -> return Decimal
      "LowerRoman"   -> return LowerRoman
      "UpperRoman"   -> return UpperRoman
      "LowerAlpha"   -> return LowerAlpha
      "UpperAlpha"   -> return UpperAlpha
      _              -> mempty
  parseJSON _ = mempty
instance ToJSON ListNumberStyle where
  toJSON lsty = taggedNoContent s
    where s = case lsty of
            DefaultStyle -> "DefaultStyle"
            Example      -> "Example"
            Decimal      -> "Decimal"
            LowerRoman   -> "LowerRoman"
            UpperRoman   -> "UpperRoman"
            LowerAlpha   -> "LowerAlpha"
            UpperAlpha   -> "UpperAlpha"

instance FromJSON ListNumberDelim where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "DefaultDelim" -> return DefaultDelim
      "Period"       -> return Period
      "OneParen"     -> return OneParen
      "TwoParens"    -> return TwoParens
      _                     -> mempty
  parseJSON _ = mempty
instance ToJSON ListNumberDelim where
  toJSON delim = taggedNoContent s
    where s = case delim of
            DefaultDelim -> "DefaultDelim"
            Period       -> "Period"
            OneParen     -> "OneParen"
            TwoParens    -> "TwoParens"

instance FromJSON Alignment where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "AlignLeft"    -> return AlignLeft
      "AlignRight"   -> return AlignRight
      "AlignCenter"  -> return AlignCenter
      "AlignDefault" -> return AlignDefault
      _                     -> mempty
  parseJSON _ = mempty
instance ToJSON Alignment where
  toJSON delim = taggedNoContent s
    where s = case delim of
            AlignLeft    -> "AlignLeft"
            AlignRight   -> "AlignRight"
            AlignCenter  -> "AlignCenter"
            AlignDefault -> "AlignDefault"


instance FromJSON Inline where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "Str"         -> Str <$> v .: "c"
      "Emph"        -> Emph <$> v .: "c"
      "Strong"      -> Strong <$> v .: "c"
      "Strikeout"   -> Strikeout <$> v .: "c"
      "Superscript" -> Superscript <$> v .: "c"
      "Subscript"   -> Subscript <$> v .: "c"
      "SmallCaps"   -> SmallCaps <$> v .: "c"
      "Quoted"      -> do (qt, ils) <- v .: "c"
                          return $ Quoted qt ils
      "Cite"        -> do (cits, ils) <- v .: "c"
                          return $ Cite cits ils
      "Code"        -> do (attr, s) <- v .: "c"
                          return $ Code attr s
      "Space"       -> return Space
      "SoftBreak"   -> return SoftBreak
      "LineBreak"   -> return LineBreak
      "PageBreak"   -> return PageBreak
      "Math"        -> do (mtype, s) <- v .: "c"
                          return $ Math mtype s
      "RawInline"   -> do (fmt, s) <- v .: "c"
                          return $ RawInline fmt s
      "Link"        -> do (attr, ils, tgt) <- v .: "c"
                          return $ Link attr ils tgt
      "Image"       -> do (attr, ils, tgt) <- v .: "c"
                          return $ Image attr ils tgt
      "Note"        -> Note <$> v .: "c"
      "Span"        -> do (attr, ils) <- v .: "c"
                          return $ Span attr ils
      _ -> mempty
  parseJSON _ = mempty

instance ToJSON Inline where
  toJSON (Str s) = tagged "Str" s
  toJSON (Emph ils) = tagged "Emph" ils
  toJSON (Strong ils) = tagged "Strong" ils
  toJSON (Strikeout ils) = tagged "Strikeout" ils
  toJSON (Superscript ils) = tagged "Superscript" ils
  toJSON (Subscript ils) = tagged "Subscript" ils
  toJSON (SmallCaps ils) = tagged "SmallCaps" ils
  toJSON (Quoted qtype ils) = tagged "Quoted" (qtype, ils)
  toJSON (Cite cits ils) = tagged "Cite" (cits, ils)
  toJSON (Code attr s) = tagged "Code" (attr, s)
  toJSON Space = taggedNoContent "Space"
  toJSON SoftBreak = taggedNoContent "SoftBreak"
  toJSON LineBreak = taggedNoContent "LineBreak"
  toJSON PageBreak = taggedNoContent "PageBreak"
  toJSON (Math mtype s) = tagged "Math" (mtype, s)
  toJSON (RawInline fmt s) = tagged "RawInline" (fmt, s)
  toJSON (Link attr ils target) = tagged "Link" (attr, ils, target)
  toJSON (Image attr ils target) = tagged "Image" (attr, ils, target)
  toJSON (Note blks) = tagged "Note" blks
  toJSON (Span attr ils) = tagged "Span" (attr, ils)

instance FromJSON Block where
  parseJSON (Object v) = do
    t <- v .: "t" :: Aeson.Parser Value
    case t of
      "Plain"          -> Plain <$> v .: "c"
      "Para"           -> Para  <$> v .: "c"
      "LineBlock"      -> LineBlock <$> v .: "c"
      "CodeBlock"      -> do (attr, s) <- v .: "c"
                             return $ CodeBlock attr s
      "RawBlock"       -> do (fmt, s) <- v .: "c"
                             return $ RawBlock fmt s
      "BlockQuote"     -> BlockQuote <$> v .: "c"
      "OrderedList"    -> do (attr, items) <- v .: "c"
                             return $ OrderedList attr items
      "BulletList"     -> BulletList <$> v .: "c"
      "DefinitionList" -> DefinitionList <$> v .: "c"
      "Header"         -> do (n, attr, ils) <- v .: "c"
                             return $ Header n attr ils
      "HorizontalRule" -> return $ HorizontalRule
      "Table"          -> do (cpt, align, wdths, hdr, rows) <- v .: "c"
                             return $ Table cpt align wdths hdr rows
      "Div"            -> do (attr, blks) <- v .: "c"
                             return $ Div attr blks
      "Null"           -> return $ Null
      _                -> mempty
  parseJSON _ = mempty
instance ToJSON Block where
  toJSON (Plain ils) = tagged "Plain" ils
  toJSON (Para ils) = tagged "Para" ils
  toJSON (LineBlock lns) = tagged "LineBlock" lns
  toJSON (CodeBlock attr s) = tagged "CodeBlock" (attr, s)
  toJSON (RawBlock fmt s) = tagged "RawBlock" (fmt, s)
  toJSON (BlockQuote blks) = tagged "BlockQuote" blks
  toJSON (OrderedList listAttrs blksList) = tagged "OrderedList" (listAttrs, blksList)
  toJSON (BulletList blksList) = tagged "BulletList" blksList
  toJSON (DefinitionList defs) = tagged "DefinitionList" defs
  toJSON (Header n attr ils) = tagged "Header" (n, attr, ils)
  toJSON HorizontalRule = taggedNoContent "HorizontalRule"
  toJSON (Table caption aligns widths cells rows) =
    tagged "Table" (caption, aligns, widths, cells, rows)
  toJSON (Div attr blks) = tagged "Div" (attr, blks)
  toJSON Null = taggedNoContent "Null"

instance FromJSON Pandoc where
  parseJSON (Object v) = do
    mbJVersion <- v .:? "pandoc-api-version" :: Aeson.Parser (Maybe [Int])
    case mbJVersion of
      Just jVersion  | x : y : _ <- jVersion
                     , x' : y' : _ <- versionBranch pandocTypesVersion
                     , x == x'
                     , y == y' -> Pandoc <$> v .: "meta" <*> v .: "blocks"
                     | otherwise ->
                         fail $ mconcat [ "Incompatible API versions: "
                                        , "encoded with "
                                        , show jVersion
                                        , " but attempted to decode with "
                                        , show $ versionBranch pandocTypesVersion
                                        , "."
                                        ]
      _ -> fail "JSON missing pandoc-api-version."
  parseJSON _ = mempty
instance ToJSON Pandoc where
  toJSON (Pandoc meta blks) =
    object [ "pandoc-api-version" .= versionBranch pandocTypesVersion
           , "meta"               .= meta
           , "blocks"             .= blks
           ]

-- Instances for deepseq
#if MIN_VERSION_base(4,8,0)
instance NFData MetaValue
instance NFData Meta
instance NFData Citation
instance NFData Alignment
instance NFData Inline
instance NFData MathType
instance NFData Format
instance NFData CitationMode
instance NFData QuoteType
instance NFData ListNumberDelim
instance NFData ListNumberStyle
instance NFData Block
instance NFData Pandoc
#else
instance NFData MetaValue where rnf = genericRnf
instance NFData Meta where rnf = genericRnf
instance NFData Citation where rnf = genericRnf
instance NFData Alignment where rnf = genericRnf
instance NFData Inline where rnf = genericRnf
instance NFData MathType where rnf = genericRnf
instance NFData Format where rnf = genericRnf
instance NFData CitationMode where rnf = genericRnf
instance NFData QuoteType where rnf = genericRnf
instance NFData ListNumberDelim where rnf = genericRnf
instance NFData ListNumberStyle where rnf = genericRnf
instance NFData Block where rnf = genericRnf
instance NFData Pandoc where rnf = genericRnf
#endif

pandocTypesVersion :: Version
pandocTypesVersion = version

instance Wrapped Meta where
  type Unwrapped Meta = M.Map String MetaValue
  _Wrapped' =
    iso
      (\(Meta x) -> x)
      Meta

instance Meta ~ t => Rewrapped Meta t

class HasMeta a where
  meta ::
    Lens'
      a
      Meta

instance HasMeta Meta where
  meta =
    id

instance HasMeta Pandoc where
  meta =
    lens
      (\(Pandoc m _) -> m)
      (\(Pandoc _ b) m -> Pandoc m b)

instance AsEmpty Meta where
  _Empty =
    _Wrapped . _Empty

instance Each Meta Meta MetaValue MetaValue where
  each =
    _Wrapped . each

type instance Index Meta = String
type instance IxValue Meta = MetaValue
instance Ixed Meta where
  ix i =
    _Wrapped . ix i

instance At Meta where
  at i =
    _Wrapped . at i

class HasBlockList a where
  blockList ::
    Lens'
      a
      [Block]

instance HasBlockList [Block] where
  blockList =
    id

instance HasBlockList Pandoc where
  blockList =
    lens
      (\(Pandoc _ b) -> b)
      (\(Pandoc m _) b -> Pandoc m b)

class HasBlocks a where
  blocks ::
    Traversal'
      a
      Block

instance HasBlocks Block where
  blocks =
    id

instance HasBlocks Pandoc where
  blocks f (Pandoc m b) =
    Pandoc m <$> traverse f b

class HasMetaValue a where
  metaValue ::
    Lens'
      a
      MetaValue

instance HasMetaValue MetaValue where
  metaValue =
    id

class HasMetaValues a where
  metaValues ::
    Traversal'
      a
      MetaValue

instance HasMetaValues MetaValue where
  metaValues =
    id

instance HasMetaValues Meta where
  metaValues =
    meta . _Wrapped . traverse

class AsMetaMap a where
  _MetaMap ::
    Prism'
      a
      (M.Map String MetaValue)

instance AsMetaMap MetaValue where
  _MetaMap =
    prism'
      MetaMap
      (\m -> case m of
               MetaMap n -> Just n
               _ -> Nothing)

instance AsMetaMap Meta where
  _MetaMap =
    _Wrapped

class AsMetaList a where
  _MetaList ::
    Prism'
      a
      [MetaValue]

instance AsMetaList MetaValue where
  _MetaList =
    prism'
      MetaList
      (\m -> case m of
               MetaList n -> Just n
               _ -> Nothing)

class AsMetaBool a where
  _MetaBool ::
    Prism'
      a
      Bool

instance AsMetaBool MetaValue where
  _MetaBool =
    prism'
      MetaBool
      (\m -> case m of
               MetaBool n -> Just n
               _ -> Nothing)

class AsMetaString a where
  _MetaString ::
    Prism'
      a
      String

instance AsMetaString MetaValue where
  _MetaString =
    prism'
      MetaString
      (\m -> case m of
               MetaString n -> Just n
               _ -> Nothing)

class AsMetaInlines a where
  _MetaInlines ::
    Prism'
      a
      [Inline]

instance AsMetaInlines MetaValue where
  _MetaInlines =
    prism'
      MetaInlines
      (\m -> case m of
               MetaInlines n -> Just n
               _ -> Nothing)

class AsMetaBlocks a where
  _MetaBlocks ::
    Prism'
      a
      [Block]

instance AsMetaBlocks MetaValue where
  _MetaBlocks =
    prism'
      MetaBlocks
      (\m -> case m of
               MetaBlocks n -> Just n
               _ -> Nothing)

instance HasBlocks MetaValue where
  blocks =
    _MetaBlocks . traverse

class AsAlignLeft a where
  _AlignLeft ::
    Prism'
      a
      ()

instance AsAlignLeft () where
  _AlignLeft =
    id

instance AsAlignLeft Alignment where
  _AlignLeft =
    prism'
      (\() -> AlignLeft)
      (\a -> case a of
                AlignLeft ->
                  Just ()
                _ -> 
                  Nothing)

class AsAlignRight a where
  _AlignRight ::
    Prism'
      a
      ()

instance AsAlignRight () where
  _AlignRight =
    id

instance AsAlignRight Alignment where
  _AlignRight =
    prism'
      (\() -> AlignRight)
      (\a -> case a of
                AlignRight ->
                  Just ()
                _ -> 
                  Nothing)

class AsAlignCenter a where
  _AlignCenter ::
    Prism'
      a
      ()

instance AsAlignCenter () where
  _AlignCenter =
    id

instance AsAlignCenter Alignment where
  _AlignCenter =
    prism'
      (\() -> AlignCenter)
      (\a -> case a of
                AlignCenter ->
                  Just ()
                _ -> 
                  Nothing)

class AsAlignDefault a where
  _AlignDefault ::
    Prism'
      a
      ()

instance AsAlignDefault () where
  _AlignDefault =
    id

instance AsAlignDefault Alignment where
  _AlignDefault =
    prism'
      (\() -> AlignDefault)
      (\a -> case a of
                AlignDefault ->
                  Just ()
                _ -> 
                  Nothing)

class HasListStartNumber a where
  listStartNumber ::
    Lens'
      a
      Int

instance HasListStartNumber Int where
  listStartNumber =
    id

instance HasListStartNumber ListAttributes where
  listStartNumber =
    _1

class HasListNumberStyle a where
  listNumberStyle ::
    Lens'
      a
      ListNumberStyle

instance HasListNumberStyle ListNumberStyle where
  listNumberStyle =
    id

instance HasListNumberStyle ListAttributes where
  listNumberStyle =
    _2

class HasListNumberDelim a where
  listNumberDelim ::
    Lens'
      a
      ListNumberDelim

instance HasListNumberDelim ListNumberDelim where
  listNumberDelim =
    id

instance HasListNumberDelim ListAttributes where
  listNumberDelim =
    _3

class AsDefaultStyle a where
  _DefaultStyle ::
    Prism'
      a
      ()

instance AsDefaultStyle () where
  _DefaultStyle =
    id

instance AsDefaultStyle ListNumberStyle where
  _DefaultStyle =
    prism'
      (\() -> DefaultStyle)
      (\a -> case a of
                DefaultStyle ->
                  Just ()
                _ -> 
                  Nothing)

class AsExample a where
  _Example ::
    Prism'
      a
      ()

instance AsExample () where
  _Example =
    id

instance AsExample ListNumberStyle where
  _Example =
    prism'
      (\() -> Example)
      (\a -> case a of
                Example ->
                  Just ()
                _ -> 
                  Nothing)

class AsDecimal a where
  _Decimal ::
    Prism'
      a
      ()

instance AsDecimal () where
  _Decimal =
    id

instance AsDecimal ListNumberStyle where
  _Decimal =
    prism'
      (\() -> Decimal)
      (\a -> case a of
                Decimal ->
                  Just ()
                _ -> 
                  Nothing)

class AsLowerRoman a where
  _LowerRoman ::
    Prism'
      a
      ()

instance AsLowerRoman () where
  _LowerRoman =
    id

instance AsLowerRoman ListNumberStyle where
  _LowerRoman =
    prism'
      (\() -> LowerRoman)
      (\a -> case a of
                LowerRoman ->
                  Just ()
                _ -> 
                  Nothing)

class AsUpperRoman a where
  _UpperRoman ::
    Prism'
      a
      ()

instance AsUpperRoman () where
  _UpperRoman =
    id

instance AsUpperRoman ListNumberStyle where
  _UpperRoman =
    prism'
      (\() -> UpperRoman)
      (\a -> case a of
                UpperRoman ->
                  Just ()
                _ -> 
                  Nothing)

class AsLowerAlpha a where
  _LowerAlpha ::
    Prism'
      a
      ()

instance AsLowerAlpha () where
  _LowerAlpha =
    id

instance AsLowerAlpha ListNumberStyle where
  _LowerAlpha =
    prism'
      (\() -> LowerAlpha)
      (\a -> case a of
                LowerAlpha ->
                  Just ()
                _ -> 
                  Nothing)

class AsUpperAlpha a where
  _UpperAlpha ::
    Prism'
      a
      ()

instance AsUpperAlpha () where
  _UpperAlpha =
    id

instance AsUpperAlpha ListNumberStyle where
  _UpperAlpha =
    prism'
      (\() -> UpperAlpha)
      (\a -> case a of
                UpperAlpha ->
                  Just ()
                _ -> 
                  Nothing)

class AsDefaultDelim a where
  _DefaultDelim ::
    Prism'
      a
      ()

instance AsDefaultDelim () where
  _DefaultDelim =
    id

instance AsDefaultDelim ListNumberDelim where
  _DefaultDelim =
    prism'
      (\() -> DefaultDelim)
      (\a -> case a of
                DefaultDelim ->
                  Just ()
                _ -> 
                  Nothing)

class AsPeriod a where
  _Period ::
    Prism'
      a
      ()

instance AsPeriod () where
  _Period =
    id

instance AsPeriod ListNumberDelim where
  _Period =
    prism'
      (\() -> Period)
      (\a -> case a of
                Period ->
                  Just ()
                _ -> 
                  Nothing)

class AsOneParen a where
  _OneParen ::
    Prism'
      a
      ()

instance AsOneParen () where
  _OneParen =
    id

instance AsOneParen ListNumberDelim where
  _OneParen =
    prism'
      (\() -> OneParen)
      (\a -> case a of
                OneParen ->
                  Just ()
                _ -> 
                  Nothing)

class AsTwoParens a where
  _TwoParens ::
    Prism'
      a
      ()

instance AsTwoParens () where
  _TwoParens =
    id

instance AsTwoParens ListNumberDelim where
  _TwoParens =
    prism'
      (\() -> TwoParens)
      (\a -> case a of
                TwoParens ->
                  Just ()
                _ -> 
                  Nothing)

class AttrIdentifier a where
  attrIdentifier ::
    Lens'
      a
      String

instance AttrIdentifier String where
  attrIdentifier =
    id

instance AttrIdentifier Attr where
  attrIdentifier =
    _1

class AttrClasses a where
  attrClasses ::
    Lens'
      a
      [String]

instance AttrClasses [String] where
  attrClasses =
    id

instance AttrClasses Attr where
  attrClasses =
    _2

class AttrKeyValuePairs a where
  attrKeyValuePairs ::
    Lens'
      a
      [(String, String)]

instance AttrKeyValuePairs [(String, String)] where
  attrKeyValuePairs =
    id

instance AttrKeyValuePairs Attr where
  attrKeyValuePairs =
    _3

instance Wrapped Format where
  type Unwrapped Format = String
  _Wrapped' =
    iso
      (\(Format x) -> x)
      Format

instance Format ~ t => Rewrapped Format t0

class HasQuoteType a where
  quoteType ::
    Lens'
      a
      QuoteType

instance HasQuoteType QuoteType where
  quoteType = 
    id

class AsSingleQuote a where
  _SingleQuote ::
    Prism'
      a
      ()

instance AsSingleQuote () where
  _SingleQuote =
    id

instance AsSingleQuote QuoteType where
  _SingleQuote =
    prism'
      (\() -> SingleQuote)
      (\a -> case a of
                SingleQuote ->
                  Just ()
                _ -> 
                  Nothing)

class AsDoubleQuote a where
  _DoubleQuote ::
    Prism'
      a
      ()

instance AsDoubleQuote () where
  _DoubleQuote =
    id

instance AsDoubleQuote QuoteType where
  _DoubleQuote =
    prism'
      (\() -> DoubleQuote)
      (\a -> case a of
                DoubleQuote ->
                  Just ()
                _ -> 
                  Nothing)

class HasTargetURL a where
  targetURL ::
    Lens'
      a
      String

instance HasTargetURL String where
  targetURL =
    id

instance HasTargetURL Target where
  targetURL =
    _1

class HasTargetTitle a where
  targetTitle ::
    Lens'
      a
      String

instance HasTargetTitle String where
  targetTitle =
    id

instance HasTargetTitle Target where
  targetTitle =
    _2

class HasMathType a where
  mathType ::
    Lens'
      a
      MathType

instance HasMathType MathType where
  mathType =
    id

class AsDisplayMath a where
  _DisplayMath ::
    Prism'
      a
      ()

instance AsDisplayMath () where
  _DisplayMath =
    id

instance AsDisplayMath MathType where
  _DisplayMath =
    prism'
      (\() -> DisplayMath)
      (\a -> case a of
                DisplayMath ->
                  Just ()
                _ -> 
                  Nothing)

class AsInlineMath a where
  _InlineMath ::
    Prism'
      a
      ()

instance AsInlineMath () where
  _InlineMath =
    id

instance AsInlineMath MathType where
  _InlineMath =
    prism'
      (\() -> InlineMath)
      (\a -> case a of
                InlineMath ->
                  Just ()
                _ -> 
                  Nothing)

class HasCitationMode a where
  citationMode ::
    Lens'
      a
      CitationMode

instance HasCitationMode CitationMode where
  citationMode =
    id

class AsAuthorInText a where
  _AuthorInText ::
    Prism'
      a
      ()

instance AsAuthorInText () where
  _AuthorInText =
    id

instance AsAuthorInText CitationMode where
  _AuthorInText =
    prism'
      (\() -> AuthorInText)
      (\a -> case a of
                AuthorInText ->
                  Just ()
                _ -> 
                  Nothing)

class AsSuppressAuthor a where
  _SuppressAuthor ::
    Prism'
      a
      ()

instance AsSuppressAuthor () where
  _SuppressAuthor =
    id

instance AsSuppressAuthor CitationMode where
  _SuppressAuthor =
    prism'
      (\() -> SuppressAuthor)
      (\a -> case a of
                SuppressAuthor ->
                  Just ()
                _ -> 
                  Nothing)

class AsNormalCitation a where
  _NormalCitation ::
    Prism'
      a
      ()

instance AsNormalCitation () where
  _NormalCitation =
    id

instance AsNormalCitation CitationMode where
  _NormalCitation =
    prism'
      (\() -> NormalCitation)
      (\a -> case a of
                NormalCitation ->
                  Just ()
                _ -> 
                  Nothing)

class HasCitation a where
  citation ::
    Lens'
      a
      Citation

instance HasCitation Citation where
  citation =
    id

class HasCitationId a where
  citationId ::
    Lens'
      a
      String

instance HasCitationId String where
  citationId =
    id

instance HasCitationId Citation where
  citationId =
    lens
      (\(Citation i _ _ _ _ _) -> i)
      (\(Citation _ p s m n h) i -> Citation i p s m n h)

class HasCitationPrefix a where
  citationPrefix ::
    Lens'
      a
      [Inline]

instance HasCitationPrefix [Inline] where
  citationPrefix =
    id

instance HasCitationPrefix Citation where
  citationPrefix =
    lens
      (\(Citation _ p _ _ _ _) -> p)
      (\(Citation i _ s m n h) p -> Citation i p s m n h)

class HasCitationSuffix a where
  citationSuffix ::
    Lens'
      a
      [Inline]

instance HasCitationSuffix [Inline] where
  citationSuffix =
    id

instance HasCitationSuffix Citation where
  citationSuffix =
    lens
      (\(Citation _ _ s _ _ _) -> s)
      (\(Citation i p _ m n h) s -> Citation i p s m n h)

instance HasCitationMode Citation where
  citationMode =
    lens
      (\(Citation _ _ _ m _ _) -> m)
      (\(Citation i p s _ n h) m -> Citation i p s m n h)


{-}

data Block
    = Plain [Inline]        -- ^ Plain text, not a paragraph
    | Para [Inline]         -- ^ Paragraph
    | LineBlock [[Inline]]  -- ^ Multiple non-breaking lines
    | CodeBlock Attr String -- ^ Code block (literal) with attributes
    | RawBlock Format String -- ^ Raw block
    | BlockQuote [Block]    -- ^ Block quote (list of blocks)
    | OrderedList ListAttributes [[Block]] -- ^ Ordered list (attributes
                            -- and a list of items, each a list of blocks)
    | BulletList [[Block]]  -- ^ Bullet list (list of items, each
                            -- a list of blocks)
    | DefinitionList [([Inline],[[Block]])]  -- ^ Definition list
                            -- Each list item is a pair consisting of a
                            -- term (a list of inlines) and one or more
                            -- definitions (each a list of blocks)
    | Header Int Attr [Inline] -- ^ Header - level (integer) and text (inlines)
    | HorizontalRule        -- ^ Horizontal rule
    | Table [Inline] [Alignment] [Double] [TableCell] [[TableCell]]  -- ^ Table,
                            -- with caption, column alignments (required),
                            -- relative column widths (0 = default),
                            -- column headers (each a list of blocks), and
                            -- rows (each a list of lists of blocks)
    | Div Attr [Block]      -- ^ Generic block container with attributes
    | Null                  -- ^ Nothing

data Inline
    = Str String            -- ^ Text (string)
    | Emph [Inline]         -- ^ Emphasized text (list of inlines)
    | Strong [Inline]       -- ^ Strongly emphasized text (list of inlines)
    | Strikeout [Inline]    -- ^ Strikeout text (list of inlines)
    | Superscript [Inline]  -- ^ Superscripted text (list of inlines)
    | Subscript [Inline]    -- ^ Subscripted text (list of inlines)
    | SmallCaps [Inline]    -- ^ Small caps text (list of inlines)
    | Quoted QuoteType [Inline] -- ^ Quoted text (list of inlines)
    | Cite [Citation]  [Inline] -- ^ Citation (list of inlines)
    | Code Attr String      -- ^ Inline code (literal)
    | Space                 -- ^ Inter-word space
    | SoftBreak             -- ^ Soft line break
    | LineBreak             -- ^ Hard line break
    | PageBreak             -- ^ Force new page
    | Math MathType String  -- ^ TeX math (literal)
    | RawInline Format String -- ^ Raw inline
    | Link Attr [Inline] Target  -- ^ Hyperlink: alt text (list of inlines), target
    | Image Attr [Inline] Target -- ^ Image:  alt text (list of inlines), target
    | Note [Block]          -- ^ Footnote or endnote
    | Span Attr [Inline]    -- ^ Generic inline container with attributes
    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
     
-}

