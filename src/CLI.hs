{-# LANGUAGE OverloadedStrings #-}
module CLI (runCLI) where

import DB
import DAO
import Models

import Database.MySQL.Simple (Connection)

import qualified Data.Text as T
import           Data.Text (Text)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(LineBuffering))
import Text.Read (readMaybe)

runCLI :: IO ()
runCLI = do
  hSetBuffering stdout LineBuffering
  putStrLn ""
  putStrLn "╔═══════════════════════════════════════════════════╗"
  putStrLn "║   МЕРЕЖЕВА ГАЗЕТА ФАКУЛЬТЕТУ                      ║"
  putStrLn "║   Faculty Network Newspaper Information System    ║"
  putStrLn "╚═══════════════════════════════════════════════════╝"
  putStrLn ""
  withPool $ \pool -> withConn pool $ \conn -> menuLoop conn

menuLoop :: Connection -> IO ()
menuLoop conn = do
  putStrLn "══════════════════════════════════════════════════"
  putStrLn "               ГОЛОВНЕ МЕНЮ"
  putStrLn "══════════════════════════════════════════════════"
  putStrLn "┌─ КАТЕГОРІЇ ──────────────────────────────────┐"
  putStrLn "│  1) Список категорій                         │"
  putStrLn "│  2) Додати категорію                         │"
  putStrLn "│  3) Оновити категорію                        │"
  putStrLn "│  4) Видалити категорію                       │"
  putStrLn "├─ АВТОРИ ─────────────────────────────────────┤"
  putStrLn "│  5) Список авторів                           │"
  putStrLn "│  6) Додати автора                            │"
  putStrLn "│  7) Оновити автора                           │"
  putStrLn "│  8) Видалити автора                          │"
  putStrLn "├─ ЧИТАЧІ ─────────────────────────────────────┤"
  putStrLn "│  9) Список читачів                           │"
  putStrLn "│ 10) Додати читача                            │"
  putStrLn "│ 11) Оновити читача                           │"
  putStrLn "│ 12) Видалити читача                          │"
  putStrLn "├─ СТАТТІ ─────────────────────────────────────┤"
  putStrLn "│ 13) Список усіх статей                       │"
  putStrLn "│ 14) Додати статтю                            │"
  putStrLn "│ 15) Оновити статтю                           │"
  putStrLn "│ 16) Видалити статтю                          │"
  putStrLn "│ 17) Пошук статей за текстом                  │"
  putStrLn "│ 18) Статті за категорією                     │"
  putStrLn "│ 19) Статті за автором                        │"
  putStrLn "├─ ФАЙЛИ ПУБЛІКАЦІЙ ───────────────────────────┤"
  putStrLn "│ 20) Список усіх файлів                       │"
  putStrLn "│ 21) Файли для статті                         │"
  putStrLn "│ 22) Додати файл до статті                    │"
  putStrLn "│ 23) Видалити файл                            │"
  putStrLn "├─ ВІДГУКИ ────────────────────────────────────┤"
  putStrLn "│ 24) Список усіх відгуків                     │"
  putStrLn "│ 25) Відгуки для статті                       │"
  putStrLn "│ 26) Додати відгук                            │"
  putStrLn "│ 27) Видалити відгук                          │"
  putStrLn "├─ СТАТИСТИКА ─────────────────────────────────┤"
  putStrLn "│ 28) Переглянути всю статистику               │"
  putStrLn "│ 29) Статистика для статті                    │"
  putStrLn "│ 30) Збільшити перегляди (+1)                 │"
  putStrLn "│ 31) Збільшити лайки (+1)                     │"
  putStrLn "│ 32) Збільшити поділи (+1)                    │"
  putStrLn "└──────────────────────────────────────────────┘"
  putStrLn " 0) Вихід"
  putStrLn "══════════════════════════════════════════════════"
  choice <- askInt "Ваш вибір (0-32):"

  case choice of
    -- ========== КАТЕГОРІЇ ==========
    1 -> do
      cats <- listCategories conn
      if null cats then putStrLn "Категорій немає."
      else mapM_ printCategory cats
      back >> menuLoop conn
      
    2 -> do
      name <- askText "Назва категорії:"
      desc <- askMaybeText "Опис (Enter для пропуску):"
      catId <- insertCategory conn name desc
      putStrLn $ "✓ Категорію додано з ID: " ++ show catId
      back >> menuLoop conn
      
    3 -> do
      catId <- askInt "ID категорії для оновлення:"
      name <- askText "Нова назва:"
      desc <- askMaybeText "Новий опис (Enter для пропуску):"
      n <- updateCategory conn catId name desc
      putStrLn $ "✓ Оновлено записів: " ++ show n
      back >> menuLoop conn
      
    4 -> do
      catId <- askInt "ID категорії для видалення:"
      n <- deleteCategory conn catId
      putStrLn $ "✓ Видалено записів: " ++ show n
      back >> menuLoop conn

    -- ========== АВТОРИ ==========
    5 -> do
      authors <- listAuthors conn
      if null authors then putStrLn "Авторів немає."
      else mapM_ printAuthor authors
      back >> menuLoop conn
      
    6 -> do
      fullName <- askText "ПІБ автора:"
      email <- askMaybeText "Email (Enter для пропуску):"
      authId <- insertAuthor conn fullName email
      putStrLn $ "✓ Автора додано з ID: " ++ show authId
      back >> menuLoop conn
      
    7 -> do
      authId <- askInt "ID автора для оновлення:"
      fullName <- askText "Нове ПІБ:"
      email <- askMaybeText "Новий email (Enter для пропуску):"
      n <- updateAuthor conn authId fullName email
      putStrLn $ "✓ Оновлено записів: " ++ show n
      back >> menuLoop conn
      
    8 -> do
      authId <- askInt "ID автора для видалення:"
      n <- deleteAuthor conn authId
      putStrLn $ "✓ Видалено записів: " ++ show n
      back >> menuLoop conn

    -- ========== ЧИТАЧІ ==========
    9 -> do
      readers <- listReaders conn
      if null readers then putStrLn "Читачів немає."
      else mapM_ printReader readers
      back >> menuLoop conn
      
    10 -> do
      fullName <- askText "ПІБ читача:"
      email <- askMaybeText "Email (Enter для пропуску):"
      readerId <- insertReader conn fullName email
      putStrLn $ "✓ Читача додано з ID: " ++ show readerId
      back >> menuLoop conn
      
    11 -> do
      readerId <- askInt "ID читача для оновлення:"
      fullName <- askText "Нове ПІБ:"
      email <- askMaybeText "Новий email (Enter для пропуску):"
      n <- updateReader conn readerId fullName email
      putStrLn $ "✓ Оновлено записів: " ++ show n
      back >> menuLoop conn
      
    12 -> do
      readerId <- askInt "ID читача для видалення:"
      n <- deleteReader conn readerId
      putStrLn $ "✓ Видалено записів: " ++ show n
      back >> menuLoop conn

    -- ========== СТАТТІ ==========
    13 -> do
      articles <- listArticles conn
      if null articles then putStrLn "Статей немає."
      else mapM_ printArticle articles
      back >> menuLoop conn
      
    14 -> do
      title <- askText "Назва статті:"
      content <- askMaybeText "Зміст (Enter для пропуску):"
      abstract <- askMaybeText "Анотація (Enter для пропуску):"
      categoryId <- askMaybeInt "ID категорії (Enter для пропуску):"
      authorId <- askMaybeInt "ID автора (Enter для пропуску):"
      artId <- insertArticle conn title content abstract categoryId authorId
      putStrLn $ "✓ Статтю додано з ID: " ++ show artId
      putStrLn "  (автоматично створено запис статистики)"
      back >> menuLoop conn
      
    15 -> do
      artId <- askInt "ID статті для оновлення:"
      title <- askText "Нова назва:"
      content <- askMaybeText "Новий зміст (Enter для пропуску):"
      abstract <- askMaybeText "Нова анотація (Enter для пропуску):"
      categoryId <- askMaybeInt "ID категорії (Enter для пропуску):"
      authorId <- askMaybeInt "ID автора (Enter для пропуску):"
      n <- updateArticle conn artId title content abstract categoryId authorId
      putStrLn $ "✓ Оновлено записів: " ++ show n
      back >> menuLoop conn
      
    16 -> do
      artId <- askInt "ID статті для видалення:"
      n <- deleteArticle conn artId
      putStrLn $ "✓ Видалено записів: " ++ show n
      back >> menuLoop conn
      
    17 -> do
      query <- askText "Пошуковий запит:"
      articles <- searchArticles conn query
      if null articles then putStrLn "Статей не знайдено."
      else do
        putStrLn $ "Знайдено статей: " ++ show (length articles)
        mapM_ printArticle articles
      back >> menuLoop conn
      
    18 -> do
      catId <- askInt "ID категорії:"
      articles <- listArticlesByCategory conn catId
      if null articles then putStrLn "Статей у цій категорії немає."
      else mapM_ printArticle articles
      back >> menuLoop conn
      
    19 -> do
      authId <- askInt "ID автора:"
      articles <- listArticlesByAuthor conn authId
      if null articles then putStrLn "Статей від цього автора немає."
      else mapM_ printArticle articles
      back >> menuLoop conn

    -- ========== ФАЙЛИ ПУБЛІКАЦІЙ ==========
    20 -> do
      pubs <- listPublications conn
      if null pubs then putStrLn "Файлів немає."
      else mapM_ printPublication pubs
      back >> menuLoop conn
      
    21 -> do
      artId <- askInt "ID статті:"
      pubs <- listPublicationsForArticle conn artId
      if null pubs then putStrLn "Файлів для цієї статті немає."
      else mapM_ printPublication pubs
      back >> menuLoop conn
      
    22 -> do
      artId <- askInt "ID статті:"
      filePath <- askMaybeText "Шлях до файлу (Enter для пропуску):"
      fileType <- askMaybeText "Тип файлу (PDF/DOC/JPG/PNG тощо):"
      fileSize <- askMaybeInteger "Розмір файлу в байтах (Enter для пропуску):"
      pubId <- insertPublication conn artId filePath fileType fileSize
      putStrLn $ "✓ Файл додано з ID: " ++ show pubId
      back >> menuLoop conn
      
    23 -> do
      pubId <- askInt "ID файлу для видалення:"
      n <- deletePublication conn pubId
      putStrLn $ "✓ Видалено записів: " ++ show n
      back >> menuLoop conn

    -- ========== ВІДГУКИ ==========
    24 -> do
      reviews <- listReviews conn
      if null reviews then putStrLn "Відгуків немає."
      else mapM_ printReview reviews
      back >> menuLoop conn
      
    25 -> do
      artId <- askInt "ID статті:"
      reviews <- listReviewsForArticle conn artId
      if null reviews then putStrLn "Відгуків для цієї статті немає."
      else mapM_ printReview reviews
      back >> menuLoop conn
      
    26 -> do
      artId <- askInt "ID статті:"
      readerId <- askInt "ID читача:"
      rating <- askMaybeIntRange "Оцінка (1-5, Enter для пропуску):" 1 5
      comment <- askMaybeText "Коментар (Enter для пропуску):"
      revId <- insertReview conn artId readerId rating comment
      putStrLn $ "✓ Відгук додано з ID: " ++ show revId
      back >> menuLoop conn
      
    27 -> do
      revId <- askInt "ID відгуку для видалення:"
      n <- deleteReview conn revId
      putStrLn $ "✓ Видалено записів: " ++ show n
      back >> menuLoop conn

    -- ========== СТАТИСТИКА ==========
    28 -> do
      stats <- listStatistics conn
      if null stats then putStrLn "Статистики немає."
      else mapM_ printStatistics stats
      back >> menuLoop conn
      
    29 -> do
      artId <- askInt "ID статті:"
      mStat <- getStatisticsForArticle conn artId
      case mStat of
        Nothing -> putStrLn "Статистики для цієї статті не знайдено."
        Just stat -> printStatistics stat
      back >> menuLoop conn
      
    30 -> do
      artId <- askInt "ID статті:"
      n <- incrementViews conn artId
      if n > 0 
        then putStrLn "✓ Кількість переглядів збільшено на 1"
        else putStrLn "✗ Статтю не знайдено"
      back >> menuLoop conn
      
    31 -> do
      artId <- askInt "ID статті:"
      n <- incrementLikes conn artId
      if n > 0 
        then putStrLn "✓ Кількість лайків збільшено на 1"
        else putStrLn "✗ Статтю не знайдено"
      back >> menuLoop conn
      
    32 -> do
      artId <- askInt "ID статті:"
      n <- incrementShares conn artId
      if n > 0 
        then putStrLn "✓ Кількість поділів збільшено на 1"
        else putStrLn "✗ Статтю не знайдено"
      back >> menuLoop conn

    0 -> putStrLn "\n👋 До побачення! Дякуємо за використання системи.\n"
    _ -> do
      putStrLn "❌ Невірний вибір. Спробуйте ще раз."
      back >> menuLoop conn

-- ========== ДОПОМІЖНІ ФУНКЦІЇ ==========

askText :: String -> IO Text
askText msg = do
  putStrPrompt msg
  T.pack <$> getLine

askMaybeText :: String -> IO (Maybe Text)
askMaybeText msg = do
  putStrPrompt msg
  s <- getLine
  pure $ if null s then Nothing else Just (T.pack s)

askInt :: String -> IO Int
askInt msg = do
  putStrPrompt msg
  line <- getLine
  case readMaybe line :: Maybe Int of
    Just n  -> pure n
    Nothing -> putStrLn "Потрібно ціле число." >> askInt msg

askMaybeInt :: String -> IO (Maybe Int)
askMaybeInt msg = do
  putStrPrompt msg
  line <- getLine
  if null line then pure Nothing
  else case readMaybe line :: Maybe Int of
         Just n  -> pure (Just n)
         Nothing -> putStrLn "Потрібно ціле число або Enter для пропуску." >> askMaybeInt msg

askMaybeInteger :: String -> IO (Maybe Integer)
askMaybeInteger msg = do
  putStrPrompt msg
  line <- getLine
  if null line then pure Nothing
  else case readMaybe line :: Maybe Integer of
         Just n  -> pure (Just n)
         Nothing -> putStrLn "Потрібно число або Enter для пропуску." >> askMaybeInteger msg

askMaybeIntRange :: String -> Int -> Int -> IO (Maybe Int)
askMaybeIntRange msg minVal maxVal = do
  putStrPrompt msg
  line <- getLine
  if null line then pure Nothing
  else case readMaybe line :: Maybe Int of
         Just n | n >= minVal && n <= maxVal -> pure (Just n)
                | otherwise -> putStrLn ("Потрібно число від " ++ show minVal ++ " до " ++ show maxVal) >> askMaybeIntRange msg minVal maxVal
         Nothing -> putStrLn "Потрібно ціле число або Enter для пропуску." >> askMaybeIntRange msg minVal maxVal

back :: IO ()
back = do
  putStrPrompt "\nНатисніть Enter для продовження..."
  _ <- getLine
  putStrLn ""
  pure ()

putStrPrompt :: String -> IO ()
putStrPrompt msg = do
  putStr (msg <> " ")
  hFlush stdout

-- ========== ФУНКЦІЇ ВИВЕДЕННЯ ==========

printCategory :: Category -> IO ()
printCategory cat = putStrLn $ 
  "ID: " ++ show (catId cat) ++ 
  " | Назва: " ++ T.unpack (catName cat) ++
  maybe "" (\d -> " | Опис: " ++ T.unpack d) (catDescription cat)

printAuthor :: Author -> IO ()
printAuthor auth = putStrLn $ 
  "ID: " ++ show (authId auth) ++ 
  " | ПІБ: " ++ T.unpack (authFullName auth) ++
  maybe "" (\e -> " | Email: " ++ T.unpack e) (authEmail auth)

printReader :: Reader -> IO ()
printReader reader = putStrLn $ 
  "ID: " ++ show (readerId reader) ++ 
  " | ПІБ: " ++ T.unpack (readerFullName reader) ++
  maybe "" (\e -> " | Email: " ++ T.unpack e) (readerEmail reader)

printArticle :: Article -> IO ()
printArticle art = do
  putStrLn $ "ID: " ++ show (artId art) ++ " | Назва: " ++ T.unpack (artTitle art)
  maybe (pure ()) (\a -> putStrLn $ "  Анотація: " ++ T.unpack a) (artAbstract art)
  maybe (pure ()) (\c -> putStrLn $ "  Категорія ID: " ++ show c) (artCategoryId art)
  maybe (pure ()) (\a -> putStrLn $ "  Автор ID: " ++ show a) (artAuthorId art)

printPublication :: Publication -> IO ()
printPublication pub = putStrLn $ 
  "ID: " ++ show (pubId pub) ++ 
  " | Стаття ID: " ++ show (pubArticleId pub) ++
  maybe "" (\fp -> " | Файл: " ++ T.unpack fp) (pubFilePath pub) ++
  maybe "" (\ft -> " | Тип: " ++ T.unpack ft) (pubFileType pub) ++
  maybe "" (\fs -> " | Розмір: " ++ show fs ++ " байт") (pubFileSize pub)

printReview :: Review -> IO ()
printReview rev = do
  putStrLn $ "ID: " ++ show (revId rev) ++ 
    " | Стаття ID: " ++ show (revArticleId rev) ++ 
    " | Читач ID: " ++ show (revReaderId rev)
  maybe (pure ()) (\r -> putStrLn $ "  Оцінка: " ++ show r ++ "/5") (revRating rev)
  maybe (pure ()) (\c -> putStrLn $ "  Коментар: " ++ T.unpack c) (revComment rev)

printStatistics :: Statistics -> IO ()
printStatistics stat = putStrLn $ 
  "Стаття ID: " ++ show (statArticleId stat) ++ 
  " | Перегляди: " ++ show (statViewsCount stat) ++
  " | Лайки: " ++ show (statLikesCount stat) ++
  " | Поділи: " ++ show (statSharesCount stat)

