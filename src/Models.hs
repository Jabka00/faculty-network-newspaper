{-# LANGUAGE OverloadedStrings #-}
module Models where

import Data.Text (Text)
import Data.Time (UTCTime)

-- Категорія статті
data Category = Category
  { catId          :: Int
  , catName        :: Text
  , catDescription :: Maybe Text
  } deriving (Show, Eq)

-- Автор матеріалу
data Author = Author
  { authId        :: Int
  , authFullName  :: Text
  , authEmail     :: Maybe Text
  , authCreatedAt :: Maybe UTCTime
  } deriving (Show, Eq)

-- Читач газети
data Reader = Reader
  { readerId        :: Int
  , readerFullName  :: Text
  , readerEmail     :: Maybe Text
  , readerCreatedAt :: Maybe UTCTime
  } deriving (Show, Eq)

-- Стаття або графічний матеріал
data Article = Article
  { artId         :: Int
  , artTitle      :: Text
  , artContent    :: Maybe Text
  , artAbstract   :: Maybe Text
  , artCategoryId :: Maybe Int
  , artAuthorId   :: Maybe Int
  , artCreatedAt  :: Maybe UTCTime
  , artUpdatedAt  :: Maybe UTCTime
  } deriving (Show, Eq)

-- Файл публікації
data Publication = Publication
  { pubId        :: Int
  , pubArticleId :: Int
  , pubFilePath  :: Maybe Text
  , pubFileType  :: Maybe Text
  , pubFileSize  :: Maybe Integer
  , pubCreatedAt :: Maybe UTCTime
  } deriving (Show, Eq)

-- Відгук читача
data Review = Review
  { revId        :: Int
  , revArticleId :: Int
  , revReaderId  :: Int
  , revRating    :: Maybe Int
  , revComment   :: Maybe Text
  , revCreatedAt :: Maybe UTCTime
  } deriving (Show, Eq)

-- Статистика статті
data Statistics = Statistics
  { statId          :: Int
  , statArticleId   :: Int
  , statViewsCount  :: Int
  , statLikesCount  :: Int
  , statSharesCount :: Int
  , statLastUpdated :: Maybe UTCTime
  } deriving (Show, Eq)

-- Розширена інформація про статтю зі статистикою
data ArticleWithStats = ArticleWithStats
  { awsArticleId :: Int
  , awsTitle     :: Text
  , awsCategory  :: Maybe Text
  , awsAuthor    :: Maybe Text
  , awsViews     :: Int
  , awsLikes     :: Int
  , awsShares    :: Int
  } deriving (Show, Eq)

