library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

labels <- data.frame(
  ru = c(
    "Реестр МСП\n(N zip-архивов)",
    "Извлечённые данные\n(N csv-файлов)",
    "Объединённая таблица\n(csv-файл)",
    "Геопривязанная таблица\n(csv-файл)",
    "Извлечение, фильтрация,\nтрансформация (2a)",
    "Агрегирование (3a)",
    "Геопривязка (4)",
    "Доходы и расходы\n(M zip-архивов)",
    "Извлечённые данные\n(M csv-файлов)",
    "Отфильтрованная таблица\n(csv-файл)",
    "Извлечение и трансформация (2b)",
    "Агрегирование и фильтрация(3b)",
    "Число работников\n(K zip-архивов)",
    "Извлечённые данные\n(K csv-файлов)",
    "Отфильтрованная таблица\n(csv-файл)",
    "Извлечение, трансформация (2c)",
    "Агрегирование и фильтрация (3c)",
    "Открытые данные ФНС России",
    "Панельная таблица",
    "Справочник\nнаселённых пунктов",
    "Загрузка (1)",
    "Объединение и трансформация (5)"
  ),
  en = c(
    "SMB registry\n(N zip archives)",
    "Extracted data\n(N csv files)",
    "Joint table\n(a csv file)",
    "Georeferenced table\n(a csv file)",
    "Extract, filter,\ntransform (2a)",
    "Aggregate (3a)",
    "Georeference (4)",
    "Revenue&expenditure\n(M zip archives)",
    "Extracted data\n(M csv files)",
    "Filtered table\n(a csv file)",
    "Extract and transform (2b)",
    "Aggregate and filter (3b)",
    "Employees count\n(K zip files)",
    "Extracted data\n(K csv files)",
    "Filtered table\n(a csv file)",
    "Extract and transform (2c)",
    "Aggregate and filter (3c)",
    "FTS open data",
    "Panel table",
    "Settlements\nlookup table",
    "Download (1)",
    "Join and transform (5)"
  )
)

labels <- labels[[LOCALE]]

diagram <- grViz("digraph {
  
graph[layout = dot, fontname = 'Ubuntu', fontsize = 18]

subgraph cluster_a {
  rank = same;
  node[shape = box, width = 2.7, height = .5, fixedsize = True]
  D1a[label='@@1']
  D2a[label='@@2']
  D3a[label='@@3']
  D4a[label='@@4']
  
  node[shape = none, width = 2, height = .5, fixedsize = True]
  A2a[label = '@@5']
  A3a[label = '@@6']
  A4a[label = '@@7']
  
  D1a -> A2a -> D2a -> A3a -> D3a -> A4a -> D4a
}

subgraph cluster_b {
  rank = same;
  node[shape = box, width = 2.7, height = .5, fixedsize = True]
  D1b[label='@@8']
  D2b[label='@@9']
  D4b[label='@@10']
  
  node[shape = none, width = 2, height = .5, fixedsize = True]
  A2b[label = '@@11']
  A3b[label = '@@12']

  D1b -> A2b -> D2b -> A3b -> D4b
}

subgraph cluster_c {
  rank = min;
  node[shape = box, width = 2.7, height = .5, fixedsize = True]
  D1c[label='@@13']
  D2c[label='@@14']
  D4c[label='@@15']
  
  node[shape = none, width = 2, height = .5, fixedsize = True]
  A2c[label = '@@16']
  A3c[label = '@@17']
  
  D1c -> A2c -> D2c -> A3c -> D4c
}

# nodes
# D stands for 'data', A stands for 'action'
node[shape = box, width = 3, height = .5, fixedsize = True]
Source[label='@@18']

D5[label = '@@19']

D_aux[label = '@@20']

node[shape = none, width = 2, height = .5, fixedsize = True]
A1[label = '@@21']
A5[label = '@@22']

Source -> A1 -> {D1a, D1b, D1c}
D_aux -> A4a
D3a -> {A3b, A3c} 
{D4a, D4b, D4c} -> A5 -> D5 
}

[1]: labels[1]
[2]: labels[2]
[3]: labels[3]
[4]: labels[4]
[5]: labels[5]
[6]: labels[6]
[7]: labels[7]
[8]: labels[8]
[9]: labels[9]
[10]: labels[10]
[11]: labels[11]
[12]: labels[12]
[13]: labels[13]
[14]: labels[14]
[15]: labels[15]
[16]: labels[16]
[17]: labels[17]
[18]: labels[18]
[19]: labels[19]
[20]: labels[20]
[21]: labels[21]
[22]: labels[22]
")
diagram
diagram_raster <- diagram %>% export_svg() %>% charToRaw() %>% rsvg_nativeraster(width = 2000)
grid::grid.raster(diagram_raster)
