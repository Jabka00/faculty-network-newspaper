{-# LANGUAGE OverloadedStrings #-}
module DAO
  ( -- Categories
    listCategories
  , insertCategory
  , updateCategory
  , deleteCategory
    -- Authors
  , listAuthors
  , insertAuthor
  , updateAuthor
  , deleteAuthor
    -- Readers
  , listReaders
  , insertReader
  , updateReader
  , deleteReader
    -- Articles
  , listArticles
  , getArticle
  , insertArticle
  , updateArticle
  , deleteArticle
  , searchArticles
  , listArticlesByCategory
  , listArticlesByAuthor
    -- Publications
  , listPublications
  , listPublicationsForArticle
  , insertPublication
  , deletePublication
    -- Reviews
  , listReviews
  , listReviewsForArticle
  , insertReview
  , deleteReview
    -- Statistics
  , listStatistics
  , getStatisticsForArticle
  , incrementViews
  , incrementLikes
  , incrementShares
  , initializeStatistics
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Database.MySQL.Simple
                   ( Connection, query, query_, execute )
import           Database.MySQL.Simple.Types (Only (..))
import           Data.Int (Int64)
import           Models

-- Допоміжна функція для отримання останнього ID
getLastInsertId :: Connection -> IO Int
getLastInsertId conn = do
  rows <- query_ conn "SELECT LAST_INSERT_ID()" :: IO [Only Int64]
  case rows of
    Only i : _ -> pure (fromIntegral i)
    _          -> pure 0


listCategories :: Connection -> IO [Category]
listCategories conn = do
  rows <- query_ conn "SELECT id, name, description FROM categories ORDER BY id DESC"
    :: IO [(Int, Text, Maybe Text)]
  pure [Category i n d | (i, n, d) <- rows]

insertCategory :: Connection -> Text -> Maybe Text -> IO Int
insertCategory conn name desc = do
  _ <- execute conn "INSERT INTO categories (name, description) VALUES (?,?)" (name, desc)
  getLastInsertId conn

updateCategory :: Connection -> Int -> Text -> Maybe Text -> IO Int
updateCategory conn catId name desc =
  fromIntegral <$> execute conn "UPDATE categories SET name=?, description=? WHERE id=?" (name, desc, catId)

deleteCategory :: Connection -> Int -> IO Int
deleteCategory conn catId =
  fromIntegral <$> execute conn "DELETE FROM categories WHERE id=?" (Only catId)


listAuthors :: Connection -> IO [Author]
listAuthors conn = do
  rows <- query_ conn "SELECT id, full_name, email, created_at FROM authors ORDER BY id DESC"
    :: IO [(Int, Text, Maybe Text, Maybe String)]
  pure [Author i n e Nothing | (i, n, e, _) <- rows]

insertAuthor :: Connection -> Text -> Maybe Text -> IO Int
insertAuthor conn fullName email = do
  _ <- execute conn "INSERT INTO authors (full_name, email) VALUES (?,?)" (fullName, email)
  getLastInsertId conn

updateAuthor :: Connection -> Int -> Text -> Maybe Text -> IO Int
updateAuthor conn authId fullName email =
  fromIntegral <$> execute conn "UPDATE authors SET full_name=?, email=? WHERE id=?" (fullName, email, authId)

deleteAuthor :: Connection -> Int -> IO Int
deleteAuthor conn authId =
  fromIntegral <$> execute conn "DELETE FROM authors WHERE id=?" (Only authId)


listReaders :: Connection -> IO [Reader]
listReaders conn = do
  rows <- query_ conn "SELECT id, full_name, email, created_at FROM readers ORDER BY id DESC"
    :: IO [(Int, Text, Maybe Text, Maybe String)]
  pure [Reader i n e Nothing | (i, n, e, _) <- rows]

insertReader :: Connection -> Text -> Maybe Text -> IO Int
insertReader conn fullName email = do
  _ <- execute conn "INSERT INTO readers (full_name, email) VALUES (?,?)" (fullName, email)
  getLastInsertId conn

updateReader :: Connection -> Int -> Text -> Maybe Text -> IO Int
updateReader conn readerId fullName email =
  fromIntegral <$> execute conn "UPDATE readers SET full_name=?, email=? WHERE id=?" (fullName, email, readerId)

deleteReader :: Connection -> Int -> IO Int
deleteReader conn readerId =
  fromIntegral <$> execute conn "DELETE FROM readers WHERE id=?" (Only readerId)


listArticles :: Connection -> IO [Article]
listArticles conn = do
  rows <- query_ conn 
    "SELECT id, title, content, abstract, category_id, author_id, created_at, updated_at \
    \FROM articles ORDER BY id DESC"
    :: IO [(Int, Text, Maybe Text, Maybe Text, Maybe Int, Maybe Int, Maybe String, Maybe String)]
  pure [Article i t c a cid aid Nothing Nothing | (i, t, c, a, cid, aid, _, _) <- rows]

getArticle :: Connection -> Int -> IO (Maybe Article)
getArticle conn artId = do
  rows <- query conn
    "SELECT id, title, content, abstract, category_id, author_id, created_at, updated_at \
    \FROM articles WHERE id = ?"
    (Only artId)
    :: IO [(Int, Text, Maybe Text, Maybe Text, Maybe Int, Maybe Int, Maybe String, Maybe String)]
  pure $ case rows of
    []  -> Nothing
    (i, t, c, a, cid, aid, _, _):_ -> Just (Article i t c a cid aid Nothing Nothing)

insertArticle :: Connection -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> IO Int
insertArticle conn title content abstract categoryId authorId = do
  _ <- execute conn 
    "INSERT INTO articles (title, content, abstract, category_id, author_id) VALUES (?,?,?,?,?)"
    (title, content, abstract, categoryId, authorId)
  artId <- getLastInsertId conn
  -- Автоматично створити запис статистики
  _ <- initializeStatistics conn artId
  pure artId

updateArticle :: Connection -> Int -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> IO Int
updateArticle conn artId title content abstract categoryId authorId =
  fromIntegral <$> execute conn 
    "UPDATE articles SET title=?, content=?, abstract=?, category_id=?, author_id=? WHERE id=?"
    (title, content, abstract, categoryId, authorId, artId)

deleteArticle :: Connection -> Int -> IO Int
deleteArticle conn artId =
  fromIntegral <$> execute conn "DELETE FROM articles WHERE id=?" (Only artId)

searchArticles :: Connection -> Text -> IO [Article]
searchArticles conn needle = do
  let like = T.concat ["%", needle, "%"]
  rows <- query conn
    "SELECT id, title, content, abstract, category_id, author_id, created_at, updated_at \
    \FROM articles WHERE title LIKE ? OR content LIKE ? OR abstract LIKE ? ORDER BY id DESC"
    (like, like, like)
    :: IO [(Int, Text, Maybe Text, Maybe Text, Maybe Int, Maybe Int, Maybe String, Maybe String)]
  pure [Article i t c a cid aid Nothing Nothing | (i, t, c, a, cid, aid, _, _) <- rows]

listArticlesByCategory :: Connection -> Int -> IO [Article]
listArticlesByCategory conn catId = do
  rows <- query conn
    "SELECT id, title, content, abstract, category_id, author_id, created_at, updated_at \
    \FROM articles WHERE category_id = ? ORDER BY id DESC"
    (Only catId)
    :: IO [(Int, Text, Maybe Text, Maybe Text, Maybe Int, Maybe Int, Maybe String, Maybe String)]
  pure [Article i t c a cid aid Nothing Nothing | (i, t, c, a, cid, aid, _, _) <- rows]

listArticlesByAuthor :: Connection -> Int -> IO [Article]
listArticlesByAuthor conn authId = do
  rows <- query conn
    "SELECT id, title, content, abstract, category_id, author_id, created_at, updated_at \
    \FROM articles WHERE author_id = ? ORDER BY id DESC"
    (Only authId)
    :: IO [(Int, Text, Maybe Text, Maybe Text, Maybe Int, Maybe Int, Maybe String, Maybe String)]
  pure [Article i t c a cid aid Nothing Nothing | (i, t, c, a, cid, aid, _, _) <- rows]


listPublications :: Connection -> IO [Publication]
listPublications conn = do
  rows <- query_ conn 
    "SELECT id, article_id, file_path, file_type, file_size, created_at FROM publications ORDER BY id DESC"
    :: IO [(Int, Int, Maybe Text, Maybe Text, Maybe Integer, Maybe String)]
  pure [Publication i aid fp ft fs Nothing | (i, aid, fp, ft, fs, _) <- rows]

listPublicationsForArticle :: Connection -> Int -> IO [Publication]
listPublicationsForArticle conn artId = do
  rows <- query conn
    "SELECT id, article_id, file_path, file_type, file_size, created_at \
    \FROM publications WHERE article_id = ? ORDER BY id DESC"
    (Only artId)
    :: IO [(Int, Int, Maybe Text, Maybe Text, Maybe Integer, Maybe String)]
  pure [Publication i aid fp ft fs Nothing | (i, aid, fp, ft, fs, _) <- rows]

insertPublication :: Connection -> Int -> Maybe Text -> Maybe Text -> Maybe Integer -> IO Int
insertPublication conn artId filePath fileType fileSize = do
  _ <- execute conn
    "INSERT INTO publications (article_id, file_path, file_type, file_size) VALUES (?,?,?,?)"
    (artId, filePath, fileType, fileSize)
  getLastInsertId conn

deletePublication :: Connection -> Int -> IO Int
deletePublication conn pubId =
  fromIntegral <$> execute conn "DELETE FROM publications WHERE id=?" (Only pubId)


listReviews :: Connection -> IO [Review]
listReviews conn = do
  rows <- query_ conn
    "SELECT id, article_id, reader_id, rating, comment, created_at FROM reviews ORDER BY id DESC"
    :: IO [(Int, Int, Int, Maybe Int, Maybe Text, Maybe String)]
  pure [Review i aid rid r c Nothing | (i, aid, rid, r, c, _) <- rows]

listReviewsForArticle :: Connection -> Int -> IO [Review]
listReviewsForArticle conn artId = do
  rows <- query conn
    "SELECT id, article_id, reader_id, rating, comment, created_at \
    \FROM reviews WHERE article_id = ? ORDER BY id DESC"
    (Only artId)
    :: IO [(Int, Int, Int, Maybe Int, Maybe Text, Maybe String)]
  pure [Review i aid rid r c Nothing | (i, aid, rid, r, c, _) <- rows]

insertReview :: Connection -> Int -> Int -> Maybe Int -> Maybe Text -> IO Int
insertReview conn artId readerId rating comment = do
  _ <- execute conn
    "INSERT INTO reviews (article_id, reader_id, rating, comment) VALUES (?,?,?,?)"
    (artId, readerId, rating, comment)
  getLastInsertId conn

deleteReview :: Connection -> Int -> IO Int
deleteReview conn revId =
  fromIntegral <$> execute conn "DELETE FROM reviews WHERE id=?" (Only revId)


listStatistics :: Connection -> IO [Statistics]
listStatistics conn = do
  rows <- query_ conn
    "SELECT id, article_id, views_count, likes_count, shares_count, last_updated \
    \FROM statistics ORDER BY article_id DESC"
    :: IO [(Int, Int, Int, Int, Int, Maybe String)]
  pure [Statistics i aid v l s Nothing | (i, aid, v, l, s, _) <- rows]

getStatisticsForArticle :: Connection -> Int -> IO (Maybe Statistics)
getStatisticsForArticle conn artId = do
  rows <- query conn
    "SELECT id, article_id, views_count, likes_count, shares_count, last_updated \
    \FROM statistics WHERE article_id = ?"
    (Only artId)
    :: IO [(Int, Int, Int, Int, Int, Maybe String)]
  pure $ case rows of
    []  -> Nothing
    (i, aid, v, l, s, _):_ -> Just (Statistics i aid v l s Nothing)

initializeStatistics :: Connection -> Int -> IO Int
initializeStatistics conn artId = do
  _ <- execute conn
    "INSERT INTO statistics (article_id, views_count, likes_count, shares_count) VALUES (?,0,0,0)"
    (Only artId)
  getLastInsertId conn

incrementViews :: Connection -> Int -> IO Int
incrementViews conn artId =
  fromIntegral <$> execute conn
    "UPDATE statistics SET views_count = views_count + 1 WHERE article_id = ?"
    (Only artId)

incrementLikes :: Connection -> Int -> IO Int
incrementLikes conn artId =
  fromIntegral <$> execute conn
    "UPDATE statistics SET likes_count = likes_count + 1 WHERE article_id = ?"
    (Only artId)

incrementShares :: Connection -> Int -> IO Int
incrementShares conn artId =
  fromIntegral <$> execute conn
    "UPDATE statistics SET shares_count = shares_count + 1 WHERE article_id = ?"
    (Only artId)

