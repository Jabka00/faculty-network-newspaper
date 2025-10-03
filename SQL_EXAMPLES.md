# SQL Приклади для Faculty Newspaper

Якщо потрібно працювати безпосередньо з базою даних через MySQL client.

## Підключення до бази даних

```bash
docker exec -it faculty-newspaper-mysql mysql -u faculty_user -pfaculty_pass faculty_newspaper
```

Або через MySQL Workbench:
- Host: `127.0.0.1`
- Port: `3306`
- User: `faculty_user`
- Password: `faculty_pass`
- Database: `faculty_newspaper`

## Корисні запити

### Перегляд даних

```sql
-- Всі категорії
SELECT * FROM categories;

-- Всі автори
SELECT * FROM authors;

-- Всі статті з інформацією про категорію та автора
SELECT 
    a.id,
    a.title,
    c.name AS category,
    au.full_name AS author,
    a.created_at
FROM articles a
LEFT JOIN categories c ON a.category_id = c.id
LEFT JOIN authors au ON a.author_id = au.id
ORDER BY a.created_at DESC;

-- Статистика популярності
SELECT 
    a.id,
    a.title,
    s.views_count,
    s.likes_count,
    s.shares_count
FROM articles a
JOIN statistics s ON a.id = s.article_id
ORDER BY s.views_count DESC;

-- Відгуки зі статтями та читачами
SELECT 
    r.id,
    a.title AS article,
    rd.full_name AS reader,
    r.rating,
    r.comment,
    r.created_at
FROM reviews r
JOIN articles a ON r.article_id = a.id
JOIN readers rd ON r.reader_id = rd.id
ORDER BY r.created_at DESC;

-- Файли публікацій
SELECT 
    p.id,
    a.title AS article,
    p.file_path,
    p.file_type,
    p.file_size
FROM publications p
JOIN articles a ON p.article_id = a.id;
```

### Додавання даних

```sql
-- Нова категорія
INSERT INTO categories (name, description) 
VALUES ('Події', 'Факультетські події та заходи');

-- Новий автор
INSERT INTO authors (full_name, email) 
VALUES ('Тетяна Шевченко', 'tetiana@faculty.ua');

-- Нова стаття
INSERT INTO articles (title, content, abstract, category_id, author_id)
VALUES (
    'Цікава стаття про Haskell',
    'Тут буде зміст статті про функціональне програмування...',
    'Коротка анотація статті',
    2,  -- ID категорії "Наука"
    1   -- ID автора
);

-- Не забудьте створити запис статистики для нової статті
INSERT INTO statistics (article_id, views_count, likes_count, shares_count)
VALUES (LAST_INSERT_ID(), 0, 0, 0);
```

### Оновлення даних

```sql
-- Оновити назву статті
UPDATE articles 
SET title = 'Нова назва статті' 
WHERE id = 1;

-- Збільшити перегляди
UPDATE statistics 
SET views_count = views_count + 1 
WHERE article_id = 1;

-- Оновити інформацію про автора
UPDATE authors 
SET email = 'new_email@faculty.ua' 
WHERE id = 1;
```

### Пошук

```sql
-- Пошук статей за ключовим словом
SELECT id, title, abstract 
FROM articles 
WHERE title LIKE '%Haskell%' 
   OR content LIKE '%Haskell%' 
   OR abstract LIKE '%Haskell%';

-- Статті за категорією
SELECT a.* 
FROM articles a
JOIN categories c ON a.category_id = c.id
WHERE c.name = 'Наука';

-- Статті конкретного автора
SELECT a.* 
FROM articles a
JOIN authors au ON a.author_id = au.id
WHERE au.full_name LIKE '%Петренко%';
```

### Аналітика

```sql
-- Топ-5 найпопулярніших статей
SELECT 
    a.title,
    s.views_count,
    s.likes_count,
    s.shares_count,
    (s.views_count + s.likes_count * 2 + s.shares_count * 3) AS popularity_score
FROM articles a
JOIN statistics s ON a.id = s.article_id
ORDER BY popularity_score DESC
LIMIT 5;

-- Середня оцінка статей
SELECT 
    a.id,
    a.title,
    AVG(r.rating) AS avg_rating,
    COUNT(r.id) AS review_count
FROM articles a
LEFT JOIN reviews r ON a.id = r.article_id
GROUP BY a.id, a.title
HAVING review_count > 0
ORDER BY avg_rating DESC;

-- Активність авторів
SELECT 
    au.full_name,
    COUNT(a.id) AS article_count,
    SUM(s.views_count) AS total_views
FROM authors au
LEFT JOIN articles a ON au.id = a.author_id
LEFT JOIN statistics s ON a.id = s.article_id
GROUP BY au.id, au.full_name
ORDER BY article_count DESC;

-- Статистика по категоріях
SELECT 
    c.name AS category,
    COUNT(a.id) AS article_count,
    AVG(s.views_count) AS avg_views
FROM categories c
LEFT JOIN articles a ON c.id = a.category_id
LEFT JOIN statistics s ON a.id = s.article_id
GROUP BY c.id, c.name
ORDER BY article_count DESC;
```

### Видалення даних

```sql
-- Видалити статтю (автоматично видаляться пов'язані записи)
DELETE FROM articles WHERE id = 1;

-- Видалити категорію (статті залишаться з NULL категорією)
DELETE FROM categories WHERE id = 1;

-- Видалити відгук
DELETE FROM reviews WHERE id = 1;

-- Очистити всі статті (УВАГА!)
-- DELETE FROM articles;
```

### Резервне копіювання

```bash
# Створити дамп бази даних
docker exec faculty-newspaper-mysql mysqldump \
  -u faculty_user -pfaculty_pass faculty_newspaper \
  > backup_$(date +%Y%m%d_%H%M%S).sql

# Відновити з дампу
docker exec -i faculty-newspaper-mysql mysql \
  -u faculty_user -pfaculty_pass faculty_newspaper \
  < backup_20251003_190000.sql
```

## Перевірка цілісності даних

```sql
-- Статті без статистики (не повинно бути)
SELECT a.id, a.title 
FROM articles a
LEFT JOIN statistics s ON a.id = s.article_id
WHERE s.id IS NULL;

-- Статті без категорії
SELECT id, title 
FROM articles 
WHERE category_id IS NULL;

-- Статті без автора
SELECT id, title 
FROM articles 
WHERE author_id IS NULL;
```

