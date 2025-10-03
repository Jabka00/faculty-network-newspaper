-- Seed data for Faculty Network Newspaper Database
-- Початкові дані для тестування системи

-- Категорії
INSERT INTO categories (name, description) VALUES 
  ('Новини', 'Новини факультету'),
  ('Наука', 'Наукові статті'),
  ('Студентське життя', 'Матеріали про студентське життя'),
  ('Графіка', 'Графічні матеріали');

-- Автори
INSERT INTO authors (full_name, email) VALUES 
  ('Іван Петренко', 'ivan@faculty.ua'),
  ('Марія Коваленко', 'maria@faculty.ua'),
  ('Олексій Сидоренко', 'alex@faculty.ua');

-- Читачі
INSERT INTO readers (full_name, email) VALUES 
  ('Студент Первый', 'student1@faculty.ua'),
  ('Студент Второй', 'student2@faculty.ua'),
  ('Викладач Основний', 'teacher@faculty.ua');

-- Приклади статей (можна додати при необхідності)
-- INSERT INTO articles (title, content, abstract, category_id, author_id) VALUES 
--   ('Перша стаття', 'Зміст першої статті...', 'Анотація першої статті', 1, 1);

-- Примітка: Файли публікацій, відгуки та статистика будуть додані через CLI

