**Линейная регрессия на Haskell**

**Реализовано**
- Обучение линейной регрессии с градиентным спуском
- Три варианта регуляризации: NoReg, L1, L2
- Ввод гиперпараметров с клавиатуры
- Оценка качества: MSE
- Вывод предсказаний и реальных значений

**Формат данных**

Каждая строка: числовые признаки через пробел, последний столбец — целевое значение.

Пример:

| Признак 1 | Признак 2 | Целевое значение |
|-----------|-----------|------------------|
| 1.0       | 2.0       | 3.0              |
| 2.0       | 3.0       | 5.0              |
| 3.0       | 4.0       | 7.0              |


**Запуск**
```bash
stack run -- data/train.txt data/test.txt
```
**Структура модулей**

Types.hs - типы данных и ADT

DataLoader.hs - загрузка данных

LinearRegression.hs - алгоритм обучения

Metrics.hs - метрика MSE

Lib.hs - основная логика

Main.hs - точка входа

**Промпты, которые я использовала при работе с ИИ (DeepSeek)**

**Типы и архитектура**

"Нужно реализовать линейную регрессию на haskell, такие типы данных подойдут?
DataPoint, Dataset, Regularization (NoReg, L1, L2), HyperParams, LinearRegressionModel? (Нужно использовать adt для регуляризации как минимум с 3 вариантами)"

"Датасет тогда это список точек, нужен ли отдельный тип? И еще, если у нас будут признаки и метка, то можно сделать newtype для Features и Label?"

**Загрузка данных**

"Как считать файл (если передать в функцию путь) с числами? Думаю там будут строки типа '1.0 2.0 3.0', (последнее число это метка)"

"Есть сейчас такая функция parseLine
```haskell
parseLine :: (Int, String) -> DataPoint
parseLine (lineNum, line) =
    let nums = map read (words line)
        features = init nums
        label = last nums
    in DataPoint (Features features) (Label label), но возвращала Either String DataPoint. (если строка кривая — надо ошибку с номером строки)"
```
Помоги с реализацией обработки ошибок при неправильно введенных строках (если не одинаковое кол-во features, если некорректные числа и пр.),
нельзя использовать exitFailure. Надо сделать обработку через Left и Right."

**Алгоритм**

"Алгоритм линейной регрессии, градиентный спуск и регуляризация?"

"Нам же нужно, получается, накопить сумму ошибок по всем точкам"

"Почему parse error on input 'where'. что не так?" (там ошибка была типа такой, вложенный where, не знала, что так нельзя:)) 
```haskell
trainModel params dataset =
  LinearRegressionModel finalWeights finalBias params
  where
    rows = datasetPoints dataset
    featureCount = getFeatureCount rows
    initialWeights = Weights (replicate featureCount 0.0)
    initialBias = Bias 0.0
    initialState = (initialWeights, initialBias)
    finalState = process initialState (epochs params)
    where process state 0 = state
          process state n = process (one_step state) (n-1)
    finalWeights = getWeights (fst finalState)
    finalBias = getBias (snd finalState)
```
**Метрики и вывод**

"почему у меня предупреждение про Defaulting the type variable в (g - p) ^ 2?"

"хочу выводить не только MSE, но и первые несколько предсказаний рядом с реальными значениями"

**Прочее**

"как работает return в Haskell"

"как работает mapM?"

"я на маке с m1, у меня не работает stack. пишет bad CPU type"

"rosetta установлена у меня, терминал на x86_64, но stack setup падает с ошибкой C compiler cannot create executables"

"при stack build ошибка Could not find module 'System.Directory'"
