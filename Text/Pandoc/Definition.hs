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
                              -- * lenses, prisms and traversals
                              , HasPandoc(..)
                              , AsPandoc(..)
                              , AsMeta(..)
                              , HasMeta(..)
                              , HasBlockList(..)
                              , AsBlockList(..)
                              , AsBlock(..)
                              , HasBlock(..)
                              , AllBlocks(..)
                              , AsMetaValue(..)
                              , HasMetaValue(..)
                              , AllMetaValues(..)
                              , AsMetaMap(..)
                              , HasMetaMap(..)
                              , AsMetaList(..)
                              , AsMetaBool(..)
                              , AsMetaString(..)
                              , AsMetaInlines(..)
                              , AsMetaBlocks(..)
                              , AsAlignLeft(..)
                              , AsAlignRight(..)
                              , AsAlignCenter(..)
                              , AsAlignDefault(..)
                              , HasListStartNumber(..)
                              , HasListNumberStyle(..)
                              , HasListNumberDelim(..)
                              , AsDefaultStyle(..)
                              , AsExample(..)
                              , AsDecimal(..)
                              , AsLowerRoman(..)
                              , AsUpperRoman(..)
                              , AsLowerAlpha(..)
                              , AsUpperAlpha(..)
                              , AsDefaultDelim(..)
                              , AsPeriod(..)
                              , AsOneParen(..)
                              , AsTwoParens(..)
                              , AsAttr(..)
                              , AllAttrs(..)
                              , HasAttrIdentifier(..)
                              , HasAttrClasses(..)
                              , HasAttrKeyValuePairs(..)
                              , HasQuoteType(..)
                              , AsSingleQuote(..)
                              , AsDoubleQuote(..)
                              , HasTargetURL(..)
                              , HasTargetTitle(..)
                              , HasMathType(..)
                              , AsDisplayMath(..)
                              , AsInlineMath(..)
                              , HasCitationMode(..)
                              , AsAuthorInText(..)
                              , AsSuppressAuthor(..)
                              , AsNormalCitation(..)
                              , HasCitation(..)
                              , HasCitationId(..)
                              , HasCitationPrefix(..)
                              , HasCitationSuffix(..)
                              , AsPlain(..)
                              , AsPara(..)
                              , AsLineBlock(..)
                              , AsCodeBlock(..)
                              , AsRawBlock(..)
                              , AsBlockQuote(..)
                              , AsOrderedList(..)
                              , AsBulletList(..)
                              , AsDefinitionList(..)
                              , AsHeader(..)
                              , AsHorizontalRule(..)
                              , AsTable(..)
                              , AsDiv(..)
                              , AsNull(..)
                              , AsStr(..)
                              , AsEmph(..)
                              , AsStrong(..)
                              , AsStrikeout(..)
                              , AsSuperscript(..)
                              , AsSubscript(..)
                              , AsSmallCaps(..)
                              , AsQuoted(..)
                              , AsCite(..)
                              , AsCode(..)
                              , AsSpace(..)
                              , AsSoftBreak(..)
                              , AsLineBreak(..)
                              , AsPageBreak(..)
                              , AsMath(..)
                              , AsRawInline(..)
                              , AsLink(..)
                              , AsImage(..)
                              , AsNote(..)
                              , AsSpan(..)
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

----

-- Pandoc

class HasPandoc a where
  pandocL ::
    Lens'
      a
      Pandoc

instance HasPandoc Pandoc where
  pandocL =
    id
    
class AsPandoc a where
  _Pandoc ::
    Prism'
      a
      Pandoc

instance AsPandoc Pandoc where
  _Pandoc =
    id

-- Meta

class AsMeta a where
  _Meta ::
    Prism'
      a
      Meta

instance AsMeta Meta where
  _Meta =
    id

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

instance Wrapped Meta where
  type Unwrapped Meta = M.Map String MetaValue
  _Wrapped' =
    iso
      (\(Meta x) -> x)
      Meta

instance Meta ~ t => Rewrapped Meta t

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

-- [Block]

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

class AsBlockList a where
  _BlockList ::
    Prism'
      a
      [Block]

instance AsBlockList [Block] where
  _BlockList =
    id

-- Block

class AsBlock a where
  _Block ::
    Prism'
      a
      Block

instance AsBlock Block where
  _Block =
    id

class HasBlock a where
  block ::
    Lens'
      a
      Block

instance HasBlock Block where
  block =
    id

class AllBlocks a where
  blocks ::
    Traversal'
      a
      Block

instance AllBlocks Block where
  blocks =
    id

instance AllBlocks Pandoc where
  blocks =
    blockList . traverse

instance AllBlocks MetaValue where
  blocks =
    _MetaBlocks . traverse

-- MetaValue

class AsMetaValue a where
  _MetaValue ::
    Prism'
      a
      MetaValue

instance AsMetaValue MetaValue where
  _MetaValue =
    id

class HasMetaValue a where
  metaValue ::
    Lens'
      a
      MetaValue

instance HasMetaValue MetaValue where
  metaValue =
    id

class AllMetaValues a where
  metaValues ::
    Traversal'
      a
      MetaValue

instance AllMetaValues MetaValue where
  metaValues =
    id

instance AllMetaValues Meta where
  metaValues =
    meta . _Wrapped . traverse

-- MetaMap

class AsMetaMap a where
  _MetaMap ::
    Prism'
      a
      (M.Map String MetaValue)

instance AsMetaMap (M.Map String MetaValue) where
  _MetaMap =
    id

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

class HasMetaMap a where
  metaMap ::
    Lens'
      a
      (M.Map String MetaValue)

instance HasMetaMap (M.Map String MetaValue) where
  metaMap =
    id

-- MetaList

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

-- MetaBool

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

-- MetaString

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

-- MetaInlines

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

-- MetaBlocks

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

-- AlignLeft

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

-- AlignRight

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

-- AlignCenter

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

-- AlignDefault

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

-- list start number

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

-- list number style

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

-- list number delim

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

-- DefaultStyle

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

-- Example

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

-- Decimal

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

-- LowerRoman

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

-- UpperRoman

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

-- LowerAlpha

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

-- UpperAlpha

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

-- DefaultDelim

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

-- AsPeriod

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

-- AsOneParen

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

-- AsTwoParens

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

-- Attr

class AsAttr a where
  _Attr ::
    Prism'
      a
      Attr

instance AsAttr Attr where
  _Attr =
    id

class AllAttrs a where
  attrs ::
    Traversal'
      a
      Attr

instance AllAttrs Attr where
  attrs =
    id
   
instance AllAttrs Block where
  attrs f (CodeBlock a s) = fmap (\a' -> CodeBlock a' s) (f a)
  attrs f (Header n a s)  = fmap (\a' -> Header n a' s) (f a)
  attrs f (Div a s)       = fmap (\a' -> Div a' s) (f a)
  attrs _ x = pure x

instance AllAttrs Inline where
  attrs f (Code a s) = fmap (\a'->Code a' s) (f a)
  attrs f (Span a s) = fmap (\a'->Span a' s) (f a)
  attrs _ x = pure x

-- AttrIdentifier

class HasAttrIdentifier a where
  attrIdentifier ::
    Lens'
      a
      String

instance HasAttrIdentifier String where
  attrIdentifier =
    id

instance HasAttrIdentifier Attr where
  attrIdentifier =
    _1

-- AttrClasses

class HasAttrClasses a where
  attrClasses ::
    Lens'
      a
      [String]

instance HasAttrClasses [String] where
  attrClasses =
    id

instance HasAttrClasses Attr where
  attrClasses =
    _2

-- AttrKeyValuePairs

class HasAttrKeyValuePairs a where
  attrKeyValuePairs ::
    Lens'
      a
      [(String, String)]

instance HasAttrKeyValuePairs [(String, String)] where
  attrKeyValuePairs =
    id

instance HasAttrKeyValuePairs Attr where
  attrKeyValuePairs =
    _3

-- Format

instance Wrapped Format where
  type Unwrapped Format = String
  _Wrapped' =
    iso
      (\(Format x) -> x)
      Format

instance Format ~ t => Rewrapped Format t0

-- QuoteType

class HasQuoteType a where
  quoteType ::
    Lens'
      a
      QuoteType

instance HasQuoteType QuoteType where
  quoteType = 
    id

-- SingleQuote

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

-- DoubleQuote

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

-- TargetURL

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

-- TargetTitle

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

-- MathType

class HasMathType a where
  mathType ::
    Lens'
      a
      MathType

instance HasMathType MathType where
  mathType =
    id

-- DisplayMath

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

-- InlineMath

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

-- CitationMode

class HasCitationMode a where
  citationMode ::
    Lens'
      a
      CitationMode

instance HasCitationMode CitationMode where
  citationMode =
    id

instance HasCitationMode Citation where
  citationMode =
    lens
      (\(Citation _ _ _ m _ _) -> m)
      (\(Citation i p s _ n h) m -> Citation i p s m n h)

-- AuthorInText

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

-- SuppressAuthor

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

-- NormalCitation

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

-- Citation

class HasCitation a where
  citation ::
    Lens'
      a
      Citation

instance HasCitation Citation where
  citation =
    id

-- CitationId

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

-- CitationPrefix

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

-- CitationSuffix

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

-- Plain

class AsPlain a where
  _Plain ::
    Prism'
      a
      [Inline]

instance AsPlain [Inline] where
  _Plain =
    id

instance AsPlain Block where
  _Plain =
    prism'
      Plain
      (\b -> case b of
                Plain x -> Just x
                _ -> Nothing)

-- Para

class AsPara a where
  _Para ::
    Prism'
      a
      [Inline]

instance AsPara [Inline] where
  _Para =
    id
    
instance AsPara Block where
  _Para =
    prism'
      Para
      (\b -> case b of
                Para x -> Just x
                _ -> Nothing)

-- LineBlock

class AsLineBlock a where
  _LineBlock ::
    Prism'
      a
      [[Inline]]

instance AsLineBlock [[Inline]] where
  _LineBlock =
    id
    
instance AsLineBlock Block where
  _LineBlock =
    prism'
      LineBlock
      (\b -> case b of
                LineBlock x -> Just x
                _ -> Nothing)

-- CodeBlock

class AsCodeBlock a where
  _CodeBlock ::
    Prism'
      a
      (Attr, String)

instance AsCodeBlock (Attr, String) where
  _CodeBlock =
    id
    
instance AsCodeBlock Block where
  _CodeBlock =
    prism'
      (uncurry CodeBlock)
      (\b -> case b of
                CodeBlock x y -> Just (x, y)
                _ -> Nothing)

-- RawBlock

class AsRawBlock a where
  _RawBlock ::
    Prism'
      a
      (Format, String)

instance AsRawBlock (Format, String) where
  _RawBlock =
    id
    
instance AsRawBlock Block where
  _RawBlock =
    prism'
      (uncurry RawBlock)
      (\b -> case b of
                RawBlock x y -> Just (x, y)
                _ -> Nothing)

-- BlockQuote

class AsBlockQuote a where
  _BlockQuote ::
    Prism'
      a
      [Block]

instance AsBlockQuote [Block] where
  _BlockQuote =
    id
    
instance AsBlockQuote Block where
  _BlockQuote =
    prism'
      BlockQuote
      (\b -> case b of
                BlockQuote x -> Just x
                _ -> Nothing)

-- OrderedList

class AsOrderedList a where
  _OrderedList ::
    Prism'
      a
      (ListAttributes, [[Block]])

instance AsOrderedList (ListAttributes, [[Block]]) where
  _OrderedList =
    id
    
instance AsOrderedList Block where
  _OrderedList =
    prism'
      (uncurry OrderedList)
      (\b -> case b of
                OrderedList x y -> Just (x, y)
                _ -> Nothing)

-- BulletList

class AsBulletList a where
  _BulletList ::
    Prism'
      a
      [[Block]]

instance AsBulletList [[Block]] where
  _BulletList =
    id
    
instance AsBulletList Block where
  _BulletList =
    prism'
      BulletList
      (\b -> case b of
                BulletList x -> Just x
                _ -> Nothing)

-- DefinitionList

class AsDefinitionList a where
  _DefinitionList ::
    Prism'
      a
      [([Inline],[[Block]])]

instance AsDefinitionList [([Inline],[[Block]])] where
  _DefinitionList =
    id
    
instance AsDefinitionList Block where
  _DefinitionList =
    prism'
      DefinitionList
      (\b -> case b of
                DefinitionList x -> Just x
                _ -> Nothing)

-- Header

class AsHeader a where
  _Header ::
    Prism'
      a
      (Int, Attr, [Inline])

instance AsHeader (Int, Attr, [Inline]) where
  _Header =
    id
    
instance AsHeader Block where
  _Header =
    prism'
      (\(x, y, z) -> Header x y z)
      (\b -> case b of
                Header x y z -> Just (x, y, z)
                _ -> Nothing)

-- HorizontalRule

class AsHorizontalRule a where
  _HorizontalRule ::
    Prism'
      a
      ()

instance AsHorizontalRule () where
  _HorizontalRule =
    id
    
instance AsHorizontalRule Block where
  _HorizontalRule =
    prism'
      (\() -> HorizontalRule)
      (\b -> case b of
                HorizontalRule -> Just ()
                _ -> Nothing)

-- Table

class AsTable a where
  _Table ::
    Prism'
      a
      ([Inline], [Alignment], [Double], [TableCell], [[TableCell]])

instance AsTable ([Inline], [Alignment], [Double], [TableCell], [[TableCell]]) where
  _Table =
    id
    
instance AsTable Block where
  _Table =
    prism'
      (\(x, y, z, v, w) -> Table x y z v w)
      (\b -> case b of
                Table x y z v w -> Just (x, y, z, v, w)
                _ -> Nothing)

-- Div

class AsDiv a where
  _Div ::
    Prism'
      a
      (Attr, [Block])

instance AsDiv (Attr, [Block]) where
  _Div =
    id
    
instance AsDiv Block where
  _Div =
    prism'
      (\(x, y) -> Div x y)
      (\b -> case b of
                Div x y -> Just (x, y)
                _ -> Nothing)

-- Null

class AsNull a where
  _Null ::
    Prism'
      a
      ()

instance AsNull () where
  _Null =
    id

instance AsNull Block where
  _Null =
    prism'
      (\() -> Null)
      (\b -> case b of
                Null -> Just ()
                _ -> Nothing)

-- Str

class AsStr a where
  _Str ::
    Prism'
      a
      String

instance AsStr String where
  _Str =
    id

instance AsStr Inline where
  _Str =
    prism'
      Str
      (\b -> case b of
               Str x -> Just x
               _ -> Nothing)

-- Emph

class AsEmph a where
  _Emph ::
    Prism'
      a
      [Inline]

instance AsEmph [Inline] where
  _Emph =
    id

instance AsEmph Inline where
  _Emph =
    prism'
      Emph
      (\b -> case b of
               Emph x -> Just x
               _ -> Nothing)

-- Strong

class AsStrong a where
  _Strong ::
    Prism'
      a
      [Inline]

instance AsStrong [Inline] where
  _Strong =
    id

instance AsStrong Inline where
  _Strong =
    prism'
      Strong
      (\b -> case b of
               Strong x -> Just x
               _ -> Nothing)

-- Strikeout

class AsStrikeout a where
  _Strikeout ::
    Prism'
      a
      [Inline]

instance AsStrikeout [Inline] where
  _Strikeout =
    id

instance AsStrikeout Inline where
  _Strikeout =
    prism'
      Strikeout
      (\b -> case b of
               Strikeout x -> Just x
               _ -> Nothing)

-- Superscript

class AsSuperscript a where
  _Superscript ::
    Prism'
      a
      [Inline]

instance AsSuperscript [Inline] where
  _Superscript =
    id

instance AsSuperscript Inline where
  _Superscript =
    prism'
      Superscript
      (\b -> case b of
               Superscript x -> Just x
               _ -> Nothing)

-- Subscript

class AsSubscript a where
  _Subscript ::
    Prism'
      a
      [Inline]

instance AsSubscript [Inline] where
  _Subscript =
    id

instance AsSubscript Inline where
  _Subscript =
    prism'
      Subscript
      (\b -> case b of
               Subscript x -> Just x
               _ -> Nothing)

-- SmallCaps

class AsSmallCaps a where
  _SmallCaps ::
    Prism'
      a
      [Inline]

instance AsSmallCaps [Inline] where
  _SmallCaps =
    id

instance AsSmallCaps Inline where
  _SmallCaps =
    prism'
      SmallCaps
      (\b -> case b of
               SmallCaps x -> Just x
               _ -> Nothing)

-- Quoted

class AsQuoted a where
  _Quoted ::
    Prism'
      a
      (QuoteType, [Inline])

instance AsQuoted (QuoteType, [Inline]) where
  _Quoted =
    id

instance AsQuoted Inline where
  _Quoted =
    prism'
      (uncurry Quoted)
      (\b -> case b of
                Quoted x y -> Just (x, y)
                _ -> Nothing)

-- Cite

class AsCite a where
  _Cite ::
    Prism'
      a
      ([Citation], [Inline])

instance AsCite ([Citation], [Inline]) where
  _Cite =
    id

instance AsCite Inline where
  _Cite =
    prism'
      (uncurry Cite)
      (\b -> case b of
                Cite x y -> Just (x, y)
                _ -> Nothing)

-- Code

class AsCode a where
  _Code ::
    Prism'
      a
      (Attr, String)

instance AsCode (Attr, String) where
  _Code =
    id

instance AsCode Inline where
  _Code =
    prism'
      (uncurry Code)
      (\b -> case b of
                Code x y -> Just (x, y)
                _ -> Nothing)

-- Space

class AsSpace a where
  _Space ::
    Prism'
      a
      ()

instance AsSpace () where
  _Space =
    id

instance AsSpace Inline where
  _Space =
    prism'
      (\() -> Space)
      (\b -> case b of
                Space -> Just ()
                _ -> Nothing)

-- SoftBreak

class AsSoftBreak a where
  _SoftBreak ::
    Prism'
      a
      ()

instance AsSoftBreak () where
  _SoftBreak =
    id

instance AsSoftBreak Inline where
  _SoftBreak =
    prism'
      (\() -> SoftBreak)
      (\b -> case b of
                SoftBreak -> Just ()
                _ -> Nothing)

-- LineBreak

class AsLineBreak a where
  _LineBreak ::
    Prism'
      a
      ()

instance AsLineBreak () where
  _LineBreak =
    id

instance AsLineBreak Inline where
  _LineBreak =
    prism'
      (\() -> LineBreak)
      (\b -> case b of
                LineBreak -> Just ()
                _ -> Nothing)

-- PageBreak

class AsPageBreak a where
  _PageBreak ::
    Prism'
      a
      ()

instance AsPageBreak () where
  _PageBreak =
    id

instance AsPageBreak Inline where
  _PageBreak =
    prism'
      (\() -> PageBreak)
      (\b -> case b of
                PageBreak -> Just ()
                _ -> Nothing)

-- Math

class AsMath a where
  _Math ::
    Prism'
      a
      (MathType, String)

instance AsMath (MathType, String) where
  _Math =
    id

instance AsMath Inline where
  _Math =
    prism'
      (uncurry Math)
      (\b -> case b of
                Math x y -> Just (x, y)
                _ -> Nothing)

-- RawInline

class AsRawInline a where
  _RawInline ::
    Prism'
      a
      (Format, String)

instance AsRawInline (Format, String) where
  _RawInline =
    id

instance AsRawInline Inline where
  _RawInline =
    prism'
      (uncurry RawInline)
      (\b -> case b of
                RawInline x y -> Just (x, y)
                _ -> Nothing)

-- Link

class AsLink a where
  _Link ::
    Prism'
      a
      (Attr, [Inline], Target)

instance AsLink (Attr, [Inline], Target) where
  _Link =
    id

instance AsLink Inline where
  _Link =
    prism'
      (\(x, y, z) -> Link x y z)
      (\b -> case b of
                Link x y z -> Just (x, y, z)
                _ -> Nothing)

-- Image

class AsImage a where
  _Image ::
    Prism'
      a
      (Attr, [Inline], Target)

instance AsImage (Attr, [Inline], Target) where
  _Image =
    id

instance AsImage Inline where
  _Image =
    prism'
      (\(x, y, z) -> Image x y z)
      (\b -> case b of
                Image x y z -> Just (x, y, z)
                _ -> Nothing)

-- Note

class AsNote a where
  _Note ::
    Prism'
      a
      [Block]

instance AsNote [Block] where
  _Note =
    id

instance AsNote Inline where
  _Note =
    prism'
      Note
      (\b -> case b of
               Note x -> Just x
               _ -> Nothing)

-- Span

class AsSpan a where
  _Span ::
    Prism'
      a
      (Attr, [Inline])

instance AsSpan (Attr, [Inline]) where
  _Span =
    id

instance AsSpan Inline where
  _Span =
    prism'
      (uncurry Span)
      (\b -> case b of
                Span x y -> Just (x, y)
                _ -> Nothing)

----

instance Plated Block where
  plate f (BlockQuote blx) =
    BlockQuote <$> traverse f blx
  plate f (OrderedList attrs blx) =
    OrderedList attrs <$> traverseOf (each . each) f blx
  plate f (BulletList blx) =
    BulletList <$> traverseOf (each . each) f blx
  plate f (DefinitionList blx) =
    DefinitionList <$> traverseOf (each . _2 . each . each) f blx
  plate f (Table a b c h r) =
    Table a b c <$> traverseOf (each . each) f h <*> traverseOf (each . each . each) f r
  plate f (Div attrs blx) =
    Div attrs <$> traverseOf each f blx
  plate f x =
    pure x
  
instance Plated Inline where
  plate f (Emph cs) =
    Emph <$> traverseOf each f cs
  plate f (Strong cs) =
    Strong <$> traverseOf each f cs
  plate f (Strikeout cs) =
    Strikeout <$> traverseOf each f cs
  plate f (Superscript cs) =
    Superscript <$> traverseOf each f cs
  plate f (Subscript cs) =
    Subscript <$> traverseOf each f cs
  plate f (SmallCaps cs) =
    SmallCaps <$> traverseOf each f cs
  plate f (Cite cit cs) =
    Cite cit <$> traverseOf each f cs
  plate f (Span attrs cs) =
    Span attrs <$> traverseOf each f cs
  plate f x =
    pure x

instance Plated MetaValue where
  plate f (MetaMap cs) =
    MetaMap <$> traverseOf each f cs
  plate f (MetaList cs) =
    MetaList <$> traverseOf each f cs
  plate f x =
    pure x
